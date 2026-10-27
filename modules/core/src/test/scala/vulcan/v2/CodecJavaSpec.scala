/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import cats.syntax.all._
import java.io.EOFException
import java.nio.ByteBuffer
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZoneOffset}
import java.util.UUID
import org.apache.avro.Conversions
import org.apache.avro.data.TimeConversions
import org.apache.avro.generic.{GenericFixed, GenericRecord}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.Assertion
import scala.jdk.CollectionConverters._
import scala.util.Try
import scodec.bits.ByteVector
import vulcan.BaseSpec
import vulcan.v2.Codec.InstantMicros
import vulcan.v2.CodecJavaSpec.{same, Reader}

/** Checks codecs against schema resolution and logical types in the Java Avro library. */
final class CodecJavaSpec extends BaseSpec {
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

  describe("Codec") {
    it("should decode primitives the same as the Java Avro library resolves schemas") {
      primitiveSchemas.foreach { writerSchema =>
        primitiveReaders.foreach { reader =>
          forAll(AvroValueGenerators.genValue(writerSchema)) { value =>
            assertSameAsJava(reader, writerSchema, value)
          }
        }
      }
    }

    it("should decode arrays the same as the Java Avro library resolves schemas") {
      primitiveSchemas.foreach { items =>
        primitiveReaders.foreach { reader =>
          val writerSchema = Schema.Array(items)
          forAll(AvroValueGenerators.genValue(writerSchema)) { value =>
            assertSameAsJava(Reader.vector(reader), writerSchema, value)
          }
        }
      }
    }

    it("should decode maps the same as the Java Avro library resolves schemas") {
      primitiveSchemas.foreach { values =>
        primitiveReaders.foreach { reader =>
          val writerSchema = Schema.Map(values)
          forAll(AvroValueGenerators.genValue(writerSchema)) { value =>
            assertSameAsJava(Reader.map(reader), writerSchema, value)
          }
        }
      }
    }

    it("should decode enums the same as the Java Avro library resolves schemas") {
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

      writerSymbols.foreach { symbols =>
        List(reader(Some("Hearts")), reader(None)).foreach { reader =>
          val writerSchema = Schema.Enum(Schema.Name("Suit"), symbols)
          forAll(AvroValueGenerators.genValue(writerSchema)) { value =>
            assertSameAsJava(reader, writerSchema, value)
          }
        }
      }
    }

    it("should decode unions the same as the Java Avro library resolves schemas") {
      val either =
        Reader[Either[Long, String]](
          Codec.either[Long, String],
          {
            case long: java.lang.Long => Left(long.longValue)
            case string               => Right(string.toString)
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

      writerSchemas.foreach { writerSchema =>
        List(either, option).foreach { reader =>
          forAll(AvroValueGenerators.genValue(writerSchema)) { value =>
            assertSameAsJava(reader, writerSchema, value)
          }
        }
      }
    }

    it("should decode records the same as the Java Avro library resolves schemas") {
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

      writerSchemas.foreach { writerSchema =>
        forAll(AvroValueGenerators.genValue(writerSchema)) { value =>
          assertSameAsJava(reader, writerSchema, value)
        }
      }
    }

    it("should decode fixed the same as the Java Avro library resolves schemas") {
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

      List(1, 2, 3).foreach { size =>
        val writerSchema = Schema.Fixed(Schema.Name("Id"), size)
        forAll(AvroValueGenerators.genValue(writerSchema)) { value =>
          assertSameAsJava(reader, writerSchema, value)
        }
      }
    }
  }

  describe("logical types") {
    it("should encode and decode decimal the same as the Java Avro library") {
      val codec = Codec.decimal(precision = 10, scale = 5)
      val schema = JavaAvro.schema(codec.schema.value)
      val conversion = new Conversions.DecimalConversion
      val genUnscaled = Gen.chooseNum(-9999999999L, 9999999999L)

      forAll(genUnscaled.map(BigDecimal(_, 5))) { decimal =>
        assertEncodeSameAsJava(codec, decimal) { decimal =>
          val bytes = conversion.toBytes(decimal.underlying, schema, schema.getLogicalType)
          AvroValue.Bytes(ByteVector(bytes))
        }
      }

      forAll(genUnscaled.map(unscaled => ByteVector(BigInt(unscaled).toByteArray))) { bytes =>
        assertDecodeSameAsJava(codec, AvroValue.Bytes(bytes)) { _ =>
          BigDecimal(conversion.fromBytes(bytes.toByteBuffer, schema, schema.getLogicalType))
        }
      }
    }

    it("should encode and decode timestamp-millis the same as the Java Avro library") {
      val conversion = new TimeConversions.TimestampMillisConversion

      forAll(genInstant) { instant =>
        assertEncodeSameAsJava(Codec.instant, instant) { instant =>
          AvroValue.Long(conversion.toLong(instant, null, null))
        }
      }

      forAll(arbitrary[Long]) { millis =>
        assertDecodeSameAsJava(Codec.instant, AvroValue.Long(millis)) { _ =>
          conversion.fromLong(millis, null, null)
        }
      }
    }

    it("should encode and decode timestamp-micros the same as the Java Avro library") {
      val conversion = new TimeConversions.TimestampMicrosConversion

      forAll(genInstant.map(InstantMicros(_))) { instantMicros =>
        assertEncodeSameAsJava(Codec.instantMicros, instantMicros) { instantMicros =>
          AvroValue.Long(conversion.toLong(instantMicros.value, null, null))
        }
      }

      forAll(arbitrary[Long]) { micros =>
        assertDecodeSameAsJava(Codec.instantMicros, AvroValue.Long(micros)) { _ =>
          InstantMicros(conversion.fromLong(micros, null, null))
        }
      }
    }

    it("should encode and decode local-timestamp-millis the same as the Java Avro library") {
      val conversion = new TimeConversions.LocalTimestampMillisConversion
      val genLocalDateTime =
        for {
          seconds <- Gen.chooseNum(
            LocalDateTime.MIN.toEpochSecond(ZoneOffset.UTC),
            LocalDateTime.MAX.toEpochSecond(ZoneOffset.UTC)
          )
          nanos <- Gen.chooseNum(0, 999999999)
        } yield LocalDateTime.ofEpochSecond(seconds, nanos, ZoneOffset.UTC)

      forAll(genLocalDateTime) { dateTime =>
        assertEncodeSameAsJava(Codec.localDateTime, dateTime) { dateTime =>
          AvroValue.Long(conversion.toLong(dateTime, null, null))
        }
      }

      forAll(arbitrary[Long]) { millis =>
        assertDecodeSameAsJava(Codec.localDateTime, AvroValue.Long(millis)) { _ =>
          conversion.fromLong(millis, null, null)
        }
      }
    }

    it("should encode and decode date the same as the Java Avro library") {
      val conversion = new TimeConversions.DateConversion
      val genLocalDate =
        Gen.chooseNum(Int.MinValue.toLong, Int.MaxValue.toLong).map(LocalDate.ofEpochDay)

      forAll(genLocalDate) { date =>
        assertEncodeSameAsJava(Codec.localDate, date) { date =>
          AvroValue.Int(conversion.toInt(date, null, null))
        }
      }

      forAll(arbitrary[Int]) { days =>
        assertDecodeSameAsJava(Codec.localDate, AvroValue.Int(days)) { _ =>
          conversion.fromInt(days, null, null)
        }
      }
    }

    it("should encode and decode time-millis the same as the Java Avro library") {
      val conversion = new TimeConversions.TimeMillisConversion

      forAll(genLocalTime) { time =>
        assertEncodeSameAsJava(Codec.localTimeMillis, time) { time =>
          AvroValue.Int(conversion.toInt(time, null, null))
        }
      }

      forAll(Gen.oneOf(Gen.chooseNum(0, 86399999), arbitrary[Int])) { millis =>
        assertDecodeSameAsJava(Codec.localTimeMillis, AvroValue.Int(millis)) { _ =>
          conversion.fromInt(millis, null, null)
        }
      }
    }

    it("should encode and decode time-micros the same as the Java Avro library") {
      val conversion = new TimeConversions.TimeMicrosConversion

      forAll(genLocalTime) { time =>
        assertEncodeSameAsJava(Codec.localTimeMicros, time) { time =>
          AvroValue.Long(conversion.toLong(time, null, null))
        }
      }

      forAll(Gen.oneOf(Gen.chooseNum(0L, 86399999999L), arbitrary[Long])) { micros =>
        assertDecodeSameAsJava(Codec.localTimeMicros, AvroValue.Long(micros)) { _ =>
          conversion.fromLong(micros, null, null)
        }
      }
    }

    it("should encode and decode uuid the same as the Java Avro library") {
      val conversion = new Conversions.UUIDConversion
      val genUuid = Gen.zip(arbitrary[Long], arbitrary[Long]).map { case (most, least) =>
        new UUID(most, least)
      }

      forAll(genUuid) { uuid =>
        assertEncodeSameAsJava(Codec.uuid, uuid) { uuid =>
          AvroValue.String(conversion.toCharSequence(uuid, null, null).toString)
        }
      }

      forAll(Gen.oneOf(genUuid.map(_.toString), arbitrary[String])) { string =>
        assertDecodeSameAsJava(Codec.uuid, AvroValue.String(string)) { _ =>
          conversion.fromCharSequence(string, null, null)
        }
      }
    }
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
  private def assertEncodeSameAsJava[A](codec: Codec[A], value: A)(
    javaEncode: A => AvroValue
  ): Assertion =
    (codec.encode(value), Try(javaEncode(value)).toEither) match {
      case (Right(encoded), Right(javaEncoded)) => assert(encoded == javaEncoded)
      case (Left(_), Left(_))                   => succeed
      case (encoded, javaEncoded)               =>
        fail(s"Encoded $value as $encoded, Java Avro encoded $javaEncoded")
    }

  /** Checks that decoding succeeds and fails the same way as for the Java Avro library. */
  private def assertDecodeSameAsJava[A](codec: Codec[A], value: AvroValue)(
    javaDecode: AvroValue => A
  ): Assertion =
    (codec.decode(value, codec.schema.value), Try(javaDecode(value)).toEither) match {
      case (Right(decoded), Right(javaDecoded)) => assert(decoded == javaDecoded)
      case (Left(_), Left(_))                   => succeed
      case (decoded, javaDecoded)               =>
        fail(s"Decoded $value as $decoded, Java Avro decoded $javaDecoded")
    }

  /** Checks that decoding succeeds and fails the same way as for the Java Avro library. */
  private def assertSameAsJava[A](
    reader: Reader[A],
    writerSchema: Schema,
    value: AvroValue
  ): Assertion = {
    val javaWriterSchema = JavaAvro.schema(writerSchema)
    val javaReaderSchema = JavaAvro.schema(reader.codec.schema.value)
    val bytes =
      JavaAvro.writeBinary(javaWriterSchema, JavaAvro.toJava(value, writerSchema, javaWriterSchema))

    val decoded = Codec.fromBinary(bytes, writerSchema)(reader.codec)
    val javaDecoded =
      Try(JavaAvro.readBinary(javaWriterSchema, javaReaderSchema, bytes)).toEither
        .map(reader.fromJava)

    (decoded, javaDecoded) match {
      case (Right(a), Right(javaA)) => assert(same(a, javaA))
      case (Left(_), Left(_))       => succeed
      // The Java Avro library checks the remaining bytes using the minimum size of reader items,
      // so rejects collections of items promoted to larger types, for example from int to float
      case (Right(_), Left(error: EOFException)) if isReaderItemSizeCheck(error) => succeed
      case _ => fail(s"Decoded $decoded from $writerSchema, Java Avro decoded $javaDecoded")
    }
  }

  private def isReaderItemSizeCheck(error: EOFException): Boolean =
    error.getMessage.startsWith("Collection claims") || error.getMessage.startsWith("Map claims")
}

object CodecJavaSpec {

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
