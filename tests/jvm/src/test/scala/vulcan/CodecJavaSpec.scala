/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Show
import cats.effect.IO
import cats.syntax.all.*
import java.io.EOFException
import java.nio.ByteBuffer
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
import java.util.UUID
import org.apache.avro.Conversions
import org.apache.avro.data.TimeConversions
import org.apache.avro.generic.GenericFixed
import org.apache.avro.generic.GenericRecord
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import scala.jdk.CollectionConverters.*
import scala.util.Try
import scodec.bits.ByteVector
import vulcan.Codec.InstantMicros
import weaver.Expectations
import weaver.SourceLocation

/** Checks codecs against schema resolution and logical types in the Java Avro library. */
object CodecJavaSpec extends BaseSuite {
  implicit val instantShow: Show[Instant] =
    Show.fromToString

  implicit val instantMicrosShow: Show[InstantMicros] =
    Show.fromToString

  implicit val localDateShow: Show[LocalDate] =
    Show.fromToString

  implicit val localDateTimeShow: Show[LocalDateTime] =
    Show.fromToString

  implicit val localTimeShow: Show[LocalTime] =
    Show.fromToString

  private val primitiveReaders: List[Reader[_]] =
    List(
      Reader[Unit](Codec.unit, _ => ()),
      Reader[Boolean](Codec.boolean, _.asInstanceOf[java.lang.Boolean].booleanValue),
      Reader[Int](Codec.int, _.asInstanceOf[java.lang.Integer].intValue),
      Reader[Long](Codec.long, _.asInstanceOf[java.lang.Long].longValue),
      Reader[Float](Codec.float, _.asInstanceOf[java.lang.Float].floatValue),
      Reader[Double](Codec.double, _.asInstanceOf[java.lang.Double].doubleValue),
      Reader[ByteVector](Codec.bytes, bytes => ByteVector(bytes.asInstanceOf[ByteBuffer])),
      Reader[String](Codec.string, _.toString)
    )

  private val primitiveSchemas: List[Schema] =
    List(
      Schema.Null,
      Schema.Boolean,
      Schema.Int,
      Schema.Long,
      Schema.Float,
      Schema.Double,
      Schema.Bytes,
      Schema.String
    )

  test("Codec should decode primitives the same as the Java Avro library resolves schemas") {
    primitiveSchemas
      .flatTraverse { writerSchema =>
        primitiveReaders.traverse { reader =>
          forall(AvroValueGenerators.genValue(writerSchema)) { value =>
            expectSameAsJava(reader, writerSchema, value)
          }
        }
      }
      .map(_.combineAll)
  }

  test("Codec should decode arrays the same as the Java Avro library resolves schemas") {
    primitiveSchemas
      .flatTraverse { items =>
        primitiveReaders.traverse { reader =>
          val writerSchema = Schema.Array(items)
          forall(AvroValueGenerators.genValue(writerSchema)) { value =>
            expectSameAsJava(Reader.vector(reader), writerSchema, value)
          }
        }
      }
      .map(_.combineAll)
  }

  test("Codec should decode maps the same as the Java Avro library resolves schemas") {
    primitiveSchemas
      .flatTraverse { values =>
        primitiveReaders.traverse { reader =>
          val writerSchema = Schema.Map(values)
          forall(AvroValueGenerators.genValue(writerSchema)) { value =>
            expectSameAsJava(Reader.map(reader), writerSchema, value)
          }
        }
      }
      .map(_.combineAll)
  }

  test("Codec should decode enums the same as the Java Avro library resolves schemas") {
    def reader(default: Option[String]): Reader[String] =
      Reader(
        Codec.enumeration[String](
          name = "Suit",
          namespace = "",
          symbols = List("Hearts", "Spades"),
          encode = identity,
          decode = Right(_),
          default = default
        ),
        _.toString
      )

    val writerSymbols =
      List(
        Vector("Hearts", "Spades"),
        Vector("Spades", "Hearts"),
        Vector("Clubs", "Hearts", "Spades")
      )

    writerSymbols
      .flatTraverse { symbols =>
        List(reader(Some("Hearts")), reader(None)).traverse { reader =>
          val writerSchema = Schema.Enum(Schema.Name("Suit"), symbols)
          forall(AvroValueGenerators.genValue(writerSchema)) { value =>
            expectSameAsJava(reader, writerSchema, value)
          }
        }
      }
      .map(_.combineAll)
  }

  test("Codec should decode unions the same as the Java Avro library resolves schemas") {
    val either =
      Reader[Either[Long, String]](
        Codec.either[Long, String],
        {
          case long: java.lang.Long => Left(long.longValue)
          case string => Right(string.toString)
        }
      )

    val option =
      Reader[Option[Long]](
        Codec.option[Long],
        value => Option(value).map(_.asInstanceOf[java.lang.Long].longValue)
      )

    val writerSchemas =
      primitiveSchemas ++ List(
        Schema.Union(Vector(Schema.Int, Schema.String)),
        Schema.Union(Vector(Schema.String, Schema.Long)),
        Schema.Union(Vector(Schema.Null, Schema.Int)),
        Schema.Union(Vector(Schema.Bytes, Schema.Null)),
        Schema.Union(Vector(Schema.Float, Schema.Int))
      )

    writerSchemas
      .flatTraverse { writerSchema =>
        List(either, option).traverse { reader =>
          forall(AvroValueGenerators.genValue(writerSchema)) { value =>
            expectSameAsJava(reader, writerSchema, value)
          }
        }
      }
      .map(_.combineAll)
  }

  test("Codec should decode records the same as the Java Avro library resolves schemas") {
    val reader =
      Reader[(Int, String)](
        Codec.record[(Int, String)]("Person", "") { field =>
          (field("a", _._1), field("b", _._2, default = Some("x"), aliases = List("c"))).tupled
        },
        value => {
          val record = value.asInstanceOf[GenericRecord]
          (record.get("a").asInstanceOf[java.lang.Integer].intValue, record.get("b").toString)
        }
      )

    def person(fields: (String, Schema)*): Schema =
      Schema.Record(
        Schema.Name("Person"),
        fields.toVector.map { case (name, schema) => Schema.Field(name, schema) }
      )

    val writerSchemas =
      List(
        person("a" -> Schema.Int, "b" -> Schema.String),
        person("b" -> Schema.String, "a" -> Schema.Int),
        person("a" -> Schema.Int),
        person("a" -> Schema.Int, "c" -> Schema.String),
        person("a" -> Schema.Int, "b" -> Schema.String, "d" -> Schema.Double),
        person("b" -> Schema.String),
        person("a" -> Schema.Long, "b" -> Schema.String),
        person("a" -> Schema.Int, "b" -> Schema.Bytes)
      )

    writerSchemas
      .traverse { writerSchema =>
        forall(AvroValueGenerators.genValue(writerSchema)) { value =>
          expectSameAsJava(reader, writerSchema, value)
        }
      }
      .map(_.combineAll)
  }

  test("Codec should decode fixed the same as the Java Avro library resolves schemas") {
    val reader =
      Reader[ByteVector](
        Codec.fixed[ByteVector](
          name = "Id",
          namespace = "",
          size = 2,
          encode = identity,
          decode = Right(_)
        ),
        fixed => ByteVector(fixed.asInstanceOf[GenericFixed].bytes)
      )

    List(1, 2, 3)
      .traverse { size =>
        val writerSchema = Schema.Fixed(Schema.Name("Id"), size)
        forall(AvroValueGenerators.genValue(writerSchema)) { value =>
          expectSameAsJava(reader, writerSchema, value)
        }
      }
      .map(_.combineAll)
  }

  test("logical types should encode and decode decimal the same as the Java Avro library") {
    val codec = Codec.decimal(precision = 10, scale = 5)
    val conversion = new Conversions.DecimalConversion
    val genUnscaled = Gen.chooseNum(-9999999999L, 9999999999L)

    for {
      schema <- IO.fromEither(codec.schema.map(JavaAvro.schema))

      encode <- forall(genUnscaled.map(BigDecimal(_, 5))) { decimal =>
        expectEncodeSameAsJava(codec, decimal) { decimal =>
          val bytes = conversion.toBytes(decimal.underlying, schema, schema.getLogicalType)
          AvroValue.Bytes(ByteVector(bytes))
        }
      }

      decode <- forall(genUnscaled.map(unscaled => ByteVector(BigInt(unscaled).toByteArray))) { bytes =>
        expectDecodeSameAsJava(codec, AvroValue.Bytes(bytes)) { _ =>
          BigDecimal(conversion.fromBytes(bytes.toByteBuffer, schema, schema.getLogicalType))
        }
      }
    } yield encode && decode
  }

  test("logical types should encode and decode timestamp-millis the same as the Java Avro library") {
    val conversion = new TimeConversions.TimestampMillisConversion

    for {
      encode <- forall(genInstant) { instant =>
        expectEncodeSameAsJava(Codec.instant, instant) { instant =>
          AvroValue.Long(conversion.toLong(instant, null, null))
        }
      }

      decode <- forall(arbitrary[Long]) { millis =>
        expectDecodeSameAsJava(Codec.instant, AvroValue.Long(millis)) { _ =>
          conversion.fromLong(millis, null, null)
        }
      }
    } yield encode && decode
  }

  test("logical types should encode and decode timestamp-micros the same as the Java Avro library") {
    val conversion = new TimeConversions.TimestampMicrosConversion

    for {
      encode <- forall(genInstant.map(InstantMicros(_))) { instantMicros =>
        expectEncodeSameAsJava(Codec.instantMicros, instantMicros) { instantMicros =>
          AvroValue.Long(conversion.toLong(instantMicros.value, null, null))
        }
      }

      decode <- forall(arbitrary[Long]) { micros =>
        expectDecodeSameAsJava(Codec.instantMicros, AvroValue.Long(micros)) { _ =>
          InstantMicros(conversion.fromLong(micros, null, null))
        }
      }
    } yield encode && decode
  }

  test("logical types should encode and decode local-timestamp-millis the same as the Java Avro library") {
    val conversion = new TimeConversions.LocalTimestampMillisConversion
    val genLocalDateTime =
      for {
        seconds <- Gen.chooseNum(
          LocalDateTime.MIN.toEpochSecond(ZoneOffset.UTC),
          LocalDateTime.MAX.toEpochSecond(ZoneOffset.UTC)
        )
        nanos <- Gen.chooseNum(0, 999999999)
      } yield LocalDateTime.ofEpochSecond(seconds, nanos, ZoneOffset.UTC)

    for {
      encode <- forall(genLocalDateTime) { dateTime =>
        expectEncodeSameAsJava(Codec.localDateTime, dateTime) { dateTime =>
          AvroValue.Long(conversion.toLong(dateTime, null, null))
        }
      }

      decode <- forall(arbitrary[Long]) { millis =>
        expectDecodeSameAsJava(Codec.localDateTime, AvroValue.Long(millis)) { _ =>
          conversion.fromLong(millis, null, null)
        }
      }
    } yield encode && decode
  }

  test("logical types should encode and decode date the same as the Java Avro library") {
    val conversion = new TimeConversions.DateConversion
    val genLocalDate =
      Gen.chooseNum(Int.MinValue.toLong, Int.MaxValue.toLong).map(LocalDate.ofEpochDay)

    for {
      encode <- forall(genLocalDate) { date =>
        expectEncodeSameAsJava(Codec.localDate, date) { date =>
          AvroValue.Int(conversion.toInt(date, null, null))
        }
      }

      decode <- forall(arbitrary[Int]) { days =>
        expectDecodeSameAsJava(Codec.localDate, AvroValue.Int(days)) { _ =>
          conversion.fromInt(days, null, null)
        }
      }
    } yield encode && decode
  }

  test("logical types should encode and decode time-millis the same as the Java Avro library") {
    val conversion = new TimeConversions.TimeMillisConversion

    for {
      encode <- forall(genLocalTime) { time =>
        expectEncodeSameAsJava(Codec.localTimeMillis, time) { time =>
          AvroValue.Int(conversion.toInt(time, null, null))
        }
      }

      decode <- forall(Gen.oneOf(Gen.chooseNum(0, 86399999), arbitrary[Int])) { millis =>
        expectDecodeSameAsJava(Codec.localTimeMillis, AvroValue.Int(millis)) { _ =>
          conversion.fromInt(millis, null, null)
        }
      }
    } yield encode && decode
  }

  test("logical types should encode and decode time-micros the same as the Java Avro library") {
    val conversion = new TimeConversions.TimeMicrosConversion

    for {
      encode <- forall(genLocalTime) { time =>
        expectEncodeSameAsJava(Codec.localTimeMicros, time) { time =>
          AvroValue.Long(conversion.toLong(time, null, null))
        }
      }

      decode <- forall(Gen.oneOf(Gen.chooseNum(0L, 86399999999L), arbitrary[Long])) { micros =>
        expectDecodeSameAsJava(Codec.localTimeMicros, AvroValue.Long(micros)) { _ =>
          conversion.fromLong(micros, null, null)
        }
      }
    } yield encode && decode
  }

  test("logical types should encode and decode uuid the same as the Java Avro library") {
    val conversion = new Conversions.UUIDConversion
    val genUuid = Gen.zip(arbitrary[Long], arbitrary[Long]).map { case (most, least) =>
      new UUID(most, least)
    }

    for {
      encode <- forall(genUuid) { uuid =>
        expectEncodeSameAsJava(Codec.uuid, uuid) { uuid =>
          AvroValue.String(conversion.toCharSequence(uuid, null, null).toString)
        }
      }

      decode <- forall(Gen.oneOf(genUuid.map(_.toString), arbitrary[String])) { string =>
        expectDecodeSameAsJava(Codec.uuid, AvroValue.String(string)) { _ =>
          conversion.fromCharSequence(string, null, null)
        }
      }
    } yield encode && decode
  }

  /** Instants across the whole range, including ones exceeding the range of epoch milliseconds. */
  private val genInstant: Gen[Instant] =
    for {
      seconds <- Gen.oneOf(
        Gen.chooseNum(Instant.MIN.getEpochSecond, Instant.MAX.getEpochSecond),
        Gen.chooseNum(-100000000000L, 100000000000L)
      )
      nanos <- Gen.chooseNum(0, 999999999)
    } yield Instant.ofEpochSecond(seconds, nanos.toLong)

  private val genLocalTime: Gen[LocalTime] =
    Gen.chooseNum(0L, 86399999999999L).map(LocalTime.ofNanoOfDay)

  /** Checks that encoding succeeds and fails the same way as for the Java Avro library. */
  private def expectEncodeSameAsJava[A](codec: Codec[A], value: A)(
    javaEncode: A => AvroValue
  )(implicit loc: SourceLocation): Expectations =
    (codec.encode(value), Try(javaEncode(value)).toEither) match {
      case (Right(encoded), Right(javaEncoded)) => expect.same(javaEncoded, encoded)
      case (Left(_), Left(_)) => success
      case (encoded, javaEncoded) =>
        failure(s"Encoded $value as $encoded, Java Avro encoded $javaEncoded")
    }

  /** Checks that decoding succeeds and fails the same way as for the Java Avro library. */
  private def expectDecodeSameAsJava[A](codec: Codec[A], value: AvroValue)(
    javaDecode: AvroValue => A
  )(implicit loc: SourceLocation): Expectations =
    whenSuccess(codec.schema.flatMap(codec.decoder)) { decoder =>
      (decoder.decode(value), Try(javaDecode(value)).toEither) match {
        case (Right(decoded), Right(javaDecoded)) => expect.same(javaDecoded, decoded)
        case (Left(_), Left(_)) => success
        case (decoded, javaDecoded) =>
          failure(s"Decoded $value as $decoded, Java Avro decoded $javaDecoded")
      }
    }

  /** Checks that decoding succeeds and fails the same way as for the Java Avro library. */
  private def expectSameAsJava[A](
    reader: Reader[A],
    writerSchema: Schema,
    value: AvroValue
  )(implicit loc: SourceLocation): Expectations =
    whenSuccess(reader.codec.schema) { readerSchema =>
      val javaWriterSchema = JavaAvro.schema(writerSchema)
      val javaReaderSchema = JavaAvro.schema(readerSchema)
      val bytes =
        JavaAvro.writeBinary(javaWriterSchema, JavaAvro.toJava(value, writerSchema, javaWriterSchema))

      val decoded = reader.codec.binaryDecoder(writerSchema).flatMap(_.decode(bytes))
      val javaDecoded =
        Try(JavaAvro.readBinary(javaWriterSchema, javaReaderSchema, bytes)).toEither
          .map(reader.fromJava)

      (decoded, javaDecoded) match {
        case (Right(a), Right(javaA)) => expect(same(a, javaA))
        case (Left(_), Left(_)) => success
        // The Java Avro library checks the remaining bytes using the minimum size of reader items,
        // so rejects collections of items promoted to larger types, for example from int to float
        case (Right(_), Left(error: EOFException)) if isReaderItemSizeCheck(error) => success
        case _ => failure(s"Decoded $decoded from $writerSchema, Java Avro decoded $javaDecoded")
      }
    }

  private def isReaderItemSizeCheck(error: EOFException): Boolean =
    error.getMessage.startsWith("Collection claims") || error.getMessage.startsWith("Map claims")

  /** A codec, together with a conversion from values read by the Java Avro library. */
  final case class Reader[A](codec: Codec[A], fromJava: Any => A)

  object Reader {
    def vector[A](items: Reader[A]): Reader[Vector[A]] =
      Reader(
        Codec.vector(items.codec),
        _.asInstanceOf[java.util.List[Any]].asScala.toVector.map(items.fromJava)
      )

    def map[A](values: Reader[A]): Reader[Map[String, A]] =
      Reader(
        Codec.map(values.codec),
        _.asInstanceOf[java.util.Map[Any, Any]].asScala.iterator
          .map { case (key, value) =>
            (key.toString, values.fromJava(value))
          }
          .toMap
      )
  }

  /** Compares boxed values, so NaN is equal to itself, and 0.0 and -0.0 are different. */
  def same(a: Any, b: Any): Boolean =
    (a, b) match {
      case (as: Vector[_], bs: Vector[_]) =>
        as.size == bs.size && as.lazyZip(bs).forall(same)

      case (as: Map[_, _], bs: Map[_, _]) =>
        as.keySet == bs.keySet && as.forall { case (key, value) =>
          same(value, bs.asInstanceOf[Map[Any, Any]](key))
        }

      case _ =>
        a.asInstanceOf[AnyRef].equals(b)
    }
}
