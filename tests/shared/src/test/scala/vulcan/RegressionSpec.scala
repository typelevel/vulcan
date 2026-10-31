/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Json
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.time.LocalDate
import scala.util.Failure
import scodec.bits.ByteVector
import vulcan.Codec.InstantMicros
import vulcan.examples.*

/**
  * Tests for issues reported against earlier versions. Values are written as Avro binary with one
  * codec and read with another, using the writer schema parsed from JSON, as schema registries return
  * it. Issues about derivation (#294, #593) are covered by the tests of the generic module.
  */
object RegressionSpec extends BaseSuite {
  locally {
    final case class Element(value: String)

    implicit val elementCodec: Codec[Element] =
      Codec.record("Element", "com.example") { field =>
        field("value", _.value).map(Element(_))
      }

    final case class Before(id: Int)

    final case class After(
      id: Int,
      enumeration: SealedTraitEnum,
      bytes: ByteVector,
      fixed: FixedBoolean,
      element: Element,
      elements: List[Element],
      elementMap: Map[String, Element],
      either: Either[Int, String]
    )

    implicit val beforeCodec: Codec[Before] =
      Codec.record("Record", "com.example") { field =>
        field("id", _.id).map(Before(_))
      }

    implicit val afterCodec: Codec[After] =
      Codec.record("Record", "com.example") { field =>
        (
          field("id", _.id),
          field("enumeration", _.enumeration, default = Some(SecondInSealedTraitEnum)),
          field("bytes", _.bytes, default = Some(ByteVector(1, 2))),
          field("fixed", _.fixed, default = Some(TrueFixedBoolean)),
          field("element", _.element, default = Some(Element("a"))),
          field("elements", _.elements, default = Some(List(Element("b")))),
          field("elementMap", _.elementMap, default = Some(Map("key" -> Element("c")))),
          field("either", _.either, default = Some(Right("d")))
        ).mapN(After(_, _, _, _, _, _, _, _))
      }

    def default(name: String): Option[Json] =
      afterCodec.schema match {
        case Right(record: Schema.Record) => record.fields.find(_.name == name).flatMap(_.default)
        case _ => None
      }

    pureTest("#33 and #592: defaults should include defaults for any type in the schema") {
      expect.eql(Some(Json.fromString("second")), default("enumeration")) &&
      expect.eql(Some(Json.fromString("\u0001\u0002")), default("bytes")) &&
      expect.eql(Some(Json.fromString("\u0001")), default("fixed")) &&
      expect.eql(Some(Json.obj("value" -> Json.fromString("a"))), default("element")) &&
      expect.eql(Some(Json.arr(Json.obj("value" -> Json.fromString("b")))), default("elements")) &&
      expect.eql(
        Some(Json.obj("key" -> Json.obj("value" -> Json.fromString("c")))),
        default("elementMap")
      ) &&
      expect.eql(Some(Json.fromString("d")), default("either"))
    }

    pureTest("#33 and #592: defaults should decode fields missing in the writer schema using the defaults") {
      expect.same(
        Right(
          After(
            id = 1,
            enumeration = SecondInSealedTraitEnum,
            bytes = ByteVector(1, 2),
            fixed = TrueFixedBoolean,
            element = Element("a"),
            elements = List(Element("b")),
            elementMap = Map("key" -> Element("c")),
            either = Right("d")
          )
        ),
        writeRead[Before, After](Before(1))
      )
    }
  }

  pureTest("#64 and #749: Java string types in writer schemas should decode strings and maps") {
    def read[A](a: A, writerSchema: String)(implicit codec: Codec[A]): Either[AvroException, A] =
      for {
        schema <- Schema.parse(writerSchema)
        encoder <- codec.binaryEncoder
        bytes <- encoder.encode(a)
        decoder <- codec.binaryDecoder(schema)
        decoded <- decoder.decode(bytes)
      } yield decoded

    expect.same(Right("value"), read("value", """{"type":"string","avro.java.string":"String"}""")) &&
    expect.same(
      Right(Map("key" -> 1)),
      read(Map("key" -> 1), """{"type":"map","values":"int","avro.java.string":"String"}""")
    )
  }

  locally {
    final case class Inner(value: Int)
    final case class Outer(inner: Inner)

    def innerCodec(namespace: String): Codec[Inner] =
      Codec.record("Inner", namespace) { field =>
        field("value", _.value).map(Inner(_))
      }

    def outerCodec(inner: Codec[Inner]): Codec[Outer] =
      Codec.record("Outer", "com.example") { field =>
        field("inner", _.inner)(inner).map(Outer(_))
      }

    pureTest(
      "#94: nested records in the null namespace should keep the null namespace when parsing the schema"
    ) {
      whenSuccess(outerCodec(innerCodec("")).schema) { schema =>
        expect.eql(
          """{"type":"record","name":"Outer","namespace":"com.example","fields":[{"name":"inner","type":{"type":"record","name":"Inner","namespace":"","fields":[{"name":"value","type":"int"}]}}]}""",
          schema.toString
        ) &&
        expect.same(Right(schema), Schema.parse(schema.toString))
      }
    }

    pureTest(
      "#94: nested records in the null namespace should decode records with the same name in other namespaces"
    ) {
      val writer = outerCodec(innerCodec("com.example"))
      val reader = outerCodec(innerCodec(""))
      expect.same(Right(Outer(Inner(1))), writeRead(Outer(Inner(1)))(writer, reader))
    }
  }

  pureTest("#134: type promotion should promote int to long, float and double") {
    expect.same(Right(1L), writeRead[Int, Long](1)) &&
    expect.same(Right(1f), writeRead[Int, Float](1)) &&
    expect.same(Right(1d), writeRead[Int, Double](1))
  }

  pureTest("#134: type promotion should promote long to float and double") {
    expect.same(Right(1f), writeRead[Long, Float](1L)) &&
    expect.same(Right(1d), writeRead[Long, Double](1L))
  }

  pureTest("#134: type promotion should promote float to double") {
    expect.same(Right(1.5d), writeRead[Float, Double](1.5f))
  }

  pureTest("#134: type promotion should promote between string and bytes") {
    val bytes = ByteVector("value".getBytes(StandardCharsets.UTF_8))
    expect.same(Right(bytes), writeRead[String, ByteVector]("value")) &&
    expect.same(Right("value"), writeRead[ByteVector, String](bytes))
  }

  pureTest(
    "#191: non-union writer schemas should decode unions using the alternative for the writer schema"
  ) {
    expect.same(Right(Some(1)), writeRead[Int, Option[Int]](1)) &&
    expect.same(Right(Right("value")), writeRead[String, Either[Int, String]]("value")) &&
    expect.same(
      Right(FirstInSealedTraitCaseClass(1)),
      writeRead[FirstInSealedTraitCaseClass, SealedTraitCaseClass](FirstInSealedTraitCaseClass(1))
    )
  }

  locally {
    val writer =
      Codec.enumeration[String]("Suit", "com.example", List("hearts", "spades", "clubs"), identity, Right(_))

    pureTest("#270: enum defaults should decode unknown symbols as the default") {
      val reader =
        Codec.enumeration[String](
          "Suit",
          "com.example",
          List("hearts", "spades"),
          identity,
          Right(_),
          default = Some("hearts")
        )

      expect.same(Right("hearts"), writeRead("clubs")(writer, reader))
    }

    pureTest("#270: enum defaults should error for unknown symbols without a default") {
      val reader =
        Codec.enumeration[String]("Suit", "com.example", List("hearts", "spades"), identity, Right(_))

      expect.same(
        Left("Error decoding com.example.Suit: clubs is not one of schema symbols [hearts, spades]"),
        writeRead("clubs")(writer, reader).left.map(_.message)
      )
    }
  }

  pureTest("#299: errors of union alternatives should include the error of the alternative") {
    final case class Thing(value: String)

    implicit val thingCodec: Codec[Thing] =
      Codec.string.imapError { string =>
        if (string == "thing") Right(Thing(string)) else Left(AvroException(s"$string is not a thing"))
      }(_.value)

    expect.same(
      Left("Error decoding Option: other is not a thing"),
      writeRead[Option[String], Option[Thing]](Some("other")).left.map(_.message)
    )
  }

  locally {
    final case class Inner(value: Int)
    final case class Outer(inner: Inner)

    object Codecs {
      implicit val outerCodec: Codec[Outer] =
        Codec.record("Outer", "com.example") { field =>
          field("inner", _.inner).map(Outer(_))
        }

      implicit val innerCodec: Codec[Inner] =
        Codec.record("Inner", "com.example") { field =>
          field("value", _.value).map(Inner(_))
        }
    }

    pureTest("#317: codecs used before their initialization should error for the field") {
      matches(Codecs.outerCodec.schema) { case Left(error: AvroException.UninitializedCodec) =>
        expect.eql(
          "Got unexpected null codec for field inner; use lazy val or def for codecs used before their definition",
          error.message
        )
      }
    }
  }

  pureTest("#317: codecs used before their initialization should error for the type using them") {
    val codec: Codec[Int] = null
    val codecs: List[(String, Codec[_])] = List(
      "Chain items" -> Codec.chain(codec),
      "Left value" -> Codec.either(codec, Codec.string),
      "List items" -> Codec.list(codec),
      "Map values" -> Codec.map(codec),
      "NonEmptyChain items" -> Codec.nonEmptyChain(codec),
      "NonEmptyList items" -> Codec.nonEmptyList(codec),
      "NonEmptyMap values" -> Codec.nonEmptyMap(codec),
      "NonEmptySet items" -> Codec.nonEmptySet(codec, Ordering.Int),
      "NonEmptyVector items" -> Codec.nonEmptyVector(codec),
      "Option value" -> Codec.option(codec),
      "Right value" -> Codec.either(Codec.string, codec),
      "Seq items" -> Codec.seq(codec),
      "Set items" -> Codec.set(codec),
      "Some value" -> Codec.some(codec),
      "union alternative" -> Codec.union[Int](_[Int](codec, Prism.identity)),
      "Vector items" -> Codec.vector(codec)
    )

    forEach(codecs) { case (description, codec) =>
      expect.eql(
        Left(
          s"Got unexpected null codec for $description; use lazy val or def for codecs used before their definition"
        ),
        codec.schema.left.map(_.message)
      )
    }
  }

  pureTest("#320: errors from exceptions should preserve the exception as cause") {
    val exception = new IllegalArgumentException("invalid")
    val codec = Codec[Int].imapTry[Int](_ => Failure(exception))(identity)
    matches(codec.decoder(Schema.Int).flatMap(_.decode(AvroValue.Int(1)))) { case Left(error) =>
      expect(clue(error.getCause) eq exception)
    }
  }

  pureTest("#333: missing fields should decode fields missing in the writer schema using the default") {
    final case class Before(a: Int)
    final case class After(a: Int, b: Option[String])

    implicit val beforeCodec: Codec[Before] =
      Codec.record("Record", "com.example") { field =>
        field("a", _.a).map(Before(_))
      }

    implicit val afterCodec: Codec[After] =
      Codec.record("Record", "com.example") { field =>
        (field("a", _.a), field("b", _.b, default = Some(None))).mapN(After(_, _))
      }

    expect.same(Right(After(1, None)), writeRead[Before, After](Before(1)))
  }

  locally {
    final case class Before(oldName: Int)
    final case class After(newName: Int)

    implicit val beforeCodec: Codec[Before] =
      Codec.record("Before", "com.example") { field =>
        field("oldName", _.oldName).map(Before(_))
      }

    implicit val afterCodec: Codec[After] =
      Codec.record("After", "com.example", aliases = Seq("Before")) { field =>
        field("newName", _.newName, aliases = Seq("oldName")).map(After(_))
      }

    pureTest("#334: aliases should decode fields using aliases") {
      expect.same(Right(After(1)), writeRead[Before, After](Before(1)))
    }

    pureTest("#334: aliases should decode union alternatives using aliases") {
      expect.same(Right(Some(After(1))), writeRead[Option[Before], Option[After]](Some(Before(1))))
    }
  }

  pureTest("#335: logical types of writer schemas should error decoding instants from plain longs") {
    expect.same(
      Left("Error decoding Instant: Got unexpected missing logical type"),
      writeRead[Long, Instant](123L).left.map(_.message)
    )
  }

  pureTest(
    "#335: logical types of writer schemas should error decoding instants from plain longs in records"
  ) {
    final case class InstantRecord(instant: Instant)
    final case class LongRecord(instant: Long)

    implicit val instantRecordCodec: Codec[InstantRecord] =
      Codec.record("Example", "") { field =>
        field("instant", _.instant).map(InstantRecord(_))
      }

    implicit val longRecordCodec: Codec[LongRecord] =
      Codec.record("Example", "") { field =>
        field("instant", _.instant).map(LongRecord(_))
      }

    expect.same(
      Left(
        "Error decoding Example: Error decoding field instant: Error decoding Instant: Got unexpected missing logical type"
      ),
      writeRead[LongRecord, InstantRecord](LongRecord(123L)).left.map(_.message)
    )
  }

  pureTest(
    "#335: logical types of writer schemas should decode decimals using the scale of the writer schema"
  ) {
    final case class High(value: BigDecimal)
    final case class Low(value: BigDecimal)

    implicit val highCodec: Codec[High] =
      Codec.record("Example", "") { field =>
        field("value", _.value)(Codec.decimal(4, 2)).map(High(_))
      }

    implicit val lowCodec: Codec[Low] =
      Codec.record("Example", "") { field =>
        field("value", _.value)(Codec.decimal(4, 1)).map(Low(_))
      }

    expect.same(Right(Low(BigDecimal("10.00"))), writeRead[High, Low](High(BigDecimal("10.00"))))
  }

  pureTest("#350: dates exceeding the int range should error when encoding") {
    expect.same(
      Left(
        "Error encoding LocalDate: Unable to encode date as epoch days of 365241780471 exceeds the maximum integer size"
      ),
      Codec[LocalDate].encoder.flatMap(_.encode(LocalDate.MAX)).left.map(_.message)
    )
  }

  pureTest("#351: options of non-empty lists should roundtrip") {
    forEach(List(None, Some(NonEmptyList.of(1, 2, 3)))) { value =>
      expect.same(Right(value), writeRead[Option[NonEmptyList[Int]], Option[NonEmptyList[Int]]](value))
    }
  }

  pureTest(
    "#385: unions with records of the same name should decode using the alternative with the same full name"
  ) {
    final case class FooEvent(value: Int)
    final case class BarEvent(value: Int)

    implicit val fooEventCodec: Codec[FooEvent] =
      Codec.record("MyEvent", "org.foo.events") { field =>
        field("value", _.value).map(FooEvent(_))
      }

    implicit val barEventCodec: Codec[BarEvent] =
      Codec.record("MyEvent", "org.bar.events") { field =>
        field("value", _.value).map(BarEvent(_))
      }

    expect.same(Right(Right(BarEvent(1))), writeRead[BarEvent, Either[FooEvent, BarEvent]](BarEvent(1))) &&
    expect.same(
      Right(Right(BarEvent(1))),
      writeRead[Either[FooEvent, BarEvent], Either[FooEvent, BarEvent]](Right(BarEvent(1)))
    )
  }

  pureTest("#398: zero bytes should decode as null") {
    val option = Codec[Option[Int]].binaryDecoder(Schema.Null)
    val unit = Codec[Unit].binaryDecoder(Schema.Null)
    expect.same(Right(None), option.flatMap(_.decode(ByteVector.empty))) &&
    expect.same(Right(()), unit.flatMap(_.decode(ByteVector.empty)))
  }

  pureTest("#450: options of unions should include the alternatives of the union") {
    expect.same(
      Right(
        """["null",{"type":"record","name":"FirstInSealedTraitCaseClass","namespace":"com.example","fields":[{"name":"value","type":"int"}]},{"type":"record","name":"SecondInSealedTraitCaseClass","namespace":"com.example","fields":[{"name":"value","type":"string"}]},{"type":"array","items":"int"}]"""
      ),
      Codec[Option[SealedTraitCaseClass]].schema.map(_.toString)
    )
  }

  pureTest("#450: options of unions should roundtrip") {
    forEach(List(None, Some(SecondInSealedTraitCaseClass("value")))) { value =>
      expect.same(
        Right(value),
        writeRead[Option[SealedTraitCaseClass], Option[SealedTraitCaseClass]](value)
      )
    }
  }

  locally {
    def suitCodec(namespace: String): Codec[String] =
      Codec.enumeration[String]("Suit", namespace, List("hearts", "spades"), identity, Right(_))

    pureTest("#453: enums in other namespaces should decode enums with the same name") {
      expect.same(Right("hearts"), writeRead("hearts")(suitCodec("com.writer"), suitCodec("com.reader")))
    }

    pureTest("#453: enums in other namespaces should decode union alternatives with the same name") {
      expect.same(
        Right(Some("hearts")),
        writeRead(Option("hearts"))(
          Codec.option(suitCodec("com.writer")),
          Codec.option(suitCodec("com.reader"))
        )
      )
    }
  }

  locally {
    final case class Element(value: String)
    final case class Pair(first: Element, second: Element)

    def elementCodec(field: String): Codec[Element] =
      Codec.record("Element", "com.example") { f =>
        f(field, _.value).map(Element(_))
      }

    def pairCodec(first: Codec[Element], second: Codec[Element]): Codec[Pair] =
      Codec.record("Pair", "com.example") { field =>
        (field("first", _.first)(first), field("second", _.second)(second)).mapN(Pair(_, _))
      }

    pureTest("#538: named schemas defined more than once should reference identical definitions") {
      val codec = pairCodec(elementCodec("value"), elementCodec("value"))
      val pair = Pair(Element("a"), Element("b"))

      expect.same(
        Right(
          """{"type":"record","name":"Pair","namespace":"com.example","fields":[{"name":"first","type":{"type":"record","name":"Element","fields":[{"name":"value","type":"string"}]}},{"name":"second","type":"Element"}]}"""
        ),
        codec.schema.map(_.toString)
      ) &&
      expect.same(Right(pair), writeRead(pair)(codec, codec))
    }

    pureTest("#538: named schemas defined more than once should error for conflicting definitions") {
      val codec = pairCodec(elementCodec("value"), elementCodec("other"))

      expect.eql(
        Left("Conflicting definitions of named schema com.example.Element"),
        codec.schema.left.map(_.message)
      )
    }
  }

  pureTest("#789: microsecond timestamps should roundtrip negative and large timestamps") {
    forEach(List(Long.MinValue, -1L, Long.MaxValue)) { micros =>
      val value = InstantMicros.fromEpochMicro(micros)
      expect.eql(micros, value.toEpochMicro) &&
      expect.same(Right(value), writeRead[InstantMicros, InstantMicros](value))
    }
  }

  pureTest("#789: microsecond timestamps should encode timestamps after the year 2262") {
    val value = InstantMicros(Instant.parse("3000-01-01T00:00:00.000001Z"))
    val encoded = Codec[InstantMicros].encoder.flatMap(_.encode(value))
    expect.same(Right(AvroValue.Long(32503680000000001L)), encoded)
  }

  /**
    * Writes the value as Avro binary using the writer codec, and reads it using the reader codec, with
    * the writer schema parsed from JSON.
    */
  def writeRead[A, B](a: A)(implicit writer: Codec[A], reader: Codec[B]): Either[AvroException, B] =
    for {
      schema <- writer.schema
      writerSchema <- Schema.parse(schema.toString)
      encoder <- writer.binaryEncoder
      bytes <- encoder.encode(a)
      decoder <- reader.binaryDecoder(writerSchema)
      b <- decoder.decode(bytes)
    } yield b
}
