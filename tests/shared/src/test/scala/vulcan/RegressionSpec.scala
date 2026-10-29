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
final class RegressionSpec extends BaseSpec {
  describe("#33 and #592: defaults") {
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
      afterCodec.schema.value match {
        case record: Schema.Record => record.fields.find(_.name == name).flatMap(_.default)
        case _ => None
      }

    it("should include defaults for any type in the schema") {
      assert(default("enumeration") == Some(Json.fromString("second")))
      assert(default("bytes") == Some(Json.fromString("\u0001\u0002")))
      assert(default("fixed") == Some(Json.fromString("\u0001")))
      assert(default("element") == Some(Json.obj("value" -> Json.fromString("a"))))
      assert(default("elements") == Some(Json.arr(Json.obj("value" -> Json.fromString("b")))))
      assert {
        default("elementMap") ==
          Some(Json.obj("key" -> Json.obj("value" -> Json.fromString("c"))))
      }
      assert(default("either") == Some(Json.fromString("d")))
    }

    it("should decode fields missing in the writer schema using the defaults") {
      assert {
        writeRead[Before, After](Before(1)) ==
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
          )
      }
    }
  }

  describe("#94: nested records in the null namespace") {
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

    it("should keep the null namespace when parsing the schema") {
      val schema = outerCodec(innerCodec("")).schema.value

      assert {
        schema.toString ==
          """{"type":"record","name":"Outer","namespace":"com.example","fields":[{"name":"inner","type":{"type":"record","name":"Inner","namespace":"","fields":[{"name":"value","type":"int"}]}}]}"""
      }

      assert(Schema.parse(schema.toString) == Right(schema))
    }

    it("should decode records with the same name in other namespaces") {
      val writer = outerCodec(innerCodec("com.example"))
      val reader = outerCodec(innerCodec(""))
      assert(writeRead(Outer(Inner(1)))(writer, reader) == Right(Outer(Inner(1))))
    }
  }

  describe("#134: type promotion") {
    it("should promote int to long, float and double") {
      assert(writeRead[Int, Long](1) == Right(1L))
      assert(writeRead[Int, Float](1) == Right(1f))
      assert(writeRead[Int, Double](1) == Right(1d))
    }

    it("should promote long to float and double") {
      assert(writeRead[Long, Float](1L) == Right(1f))
      assert(writeRead[Long, Double](1L) == Right(1d))
    }

    it("should promote float to double") {
      assert(writeRead[Float, Double](1.5f) == Right(1.5d))
    }

    it("should promote between string and bytes") {
      val bytes = ByteVector("value".getBytes(StandardCharsets.UTF_8))
      assert(writeRead[String, ByteVector]("value") == Right(bytes))
      assert(writeRead[ByteVector, String](bytes) == Right("value"))
    }
  }

  describe("#191: non-union writer schemas") {
    it("should decode unions using the alternative for the writer schema") {
      assert(writeRead[Int, Option[Int]](1) == Right(Some(1)))
      assert(writeRead[String, Either[Int, String]]("value") == Right(Right("value")))
      assert {
        writeRead[FirstInSealedTraitCaseClass, SealedTraitCaseClass](FirstInSealedTraitCaseClass(1)) ==
          Right(FirstInSealedTraitCaseClass(1))
      }
    }
  }

  describe("#270: enum defaults") {
    val writer =
      Codec.enumeration[String]("Suit", "com.example", List("hearts", "spades", "clubs"), identity, Right(_))

    it("should decode unknown symbols as the default") {
      val reader =
        Codec.enumeration[String](
          "Suit",
          "com.example",
          List("hearts", "spades"),
          identity,
          Right(_),
          default = Some("hearts")
        )

      assert(writeRead("clubs")(writer, reader) == Right("hearts"))
    }

    it("should error for unknown symbols without a default") {
      val reader =
        Codec.enumeration[String]("Suit", "com.example", List("hearts", "spades"), identity, Right(_))

      assert {
        writeRead("clubs")(writer, reader).swap.value.message ==
          "Error decoding com.example.Suit: clubs is not one of schema symbols [hearts, spades]"
      }
    }
  }

  describe("#299: errors of union alternatives") {
    final case class Thing(value: String)

    implicit val thingCodec: Codec[Thing] =
      Codec.string.imapError { string =>
        if (string == "thing") Right(Thing(string)) else Left(AvroError(s"$string is not a thing"))
      }(_.value)

    it("should include the error of the alternative") {
      assert {
        writeRead[Option[String], Option[Thing]](Some("other")).swap.value.message ==
          "Error decoding Option: other is not a thing"
      }
    }
  }

  describe("#320: errors from exceptions") {
    it("should preserve the exception as cause") {
      val exception = new IllegalArgumentException("invalid")
      val codec = Codec[Int].imapTry[Int](_ => Failure(exception))(identity)
      val error = codec.decoder(Schema.Int).flatMap(_.decode(AvroValue.Int(1))).swap.value
      assert(error.throwable.getCause eq exception)
    }
  }

  describe("#333: missing fields") {
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

    it("should decode fields missing in the writer schema using the default") {
      assert(writeRead[Before, After](Before(1)) == Right(After(1, None)))
    }
  }

  describe("#334: aliases") {
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

    it("should decode fields using aliases") {
      assert(writeRead[Before, After](Before(1)) == Right(After(1)))
    }

    it("should decode union alternatives using aliases") {
      assert(writeRead[Option[Before], Option[After]](Some(Before(1))) == Right(Some(After(1))))
    }
  }

  describe("#335: logical types of writer schemas") {
    it("should error decoding instants from plain longs") {
      assert {
        writeRead[Long, Instant](123L).swap.value.message ==
          "Error decoding Instant: Got unexpected missing logical type"
      }
    }

    it("should error decoding instants from plain longs in records") {
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

      assert {
        writeRead[LongRecord, InstantRecord](LongRecord(123L)).swap.value.message ==
          "Error decoding Example: Error decoding field instant: Error decoding Instant: Got unexpected missing logical type"
      }
    }

    it("should decode decimals using the scale of the writer schema") {
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

      assert(writeRead[High, Low](High(BigDecimal("10.00"))) == Right(Low(BigDecimal("10.00"))))
    }
  }

  describe("#350: dates exceeding the int range") {
    it("should error when encoding") {
      assert {
        Codec[LocalDate].encoder.flatMap(_.encode(LocalDate.MAX)).swap.value.message ==
          "Error encoding LocalDate: Unable to encode date as epoch days of 365241780471 exceeds the maximum integer size"
      }
    }
  }

  describe("#351: options of non-empty lists") {
    it("should roundtrip") {
      List(None, Some(NonEmptyList.of(1, 2, 3))).foreach { value =>
        assert(writeRead[Option[NonEmptyList[Int]], Option[NonEmptyList[Int]]](value) == Right(value))
      }
    }
  }

  describe("#385: unions with records of the same name") {
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

    it("should decode using the alternative with the same full name") {
      assert {
        writeRead[BarEvent, Either[FooEvent, BarEvent]](BarEvent(1)) ==
          Right(Right(BarEvent(1)))
      }

      assert {
        writeRead[Either[FooEvent, BarEvent], Either[FooEvent, BarEvent]](Right(BarEvent(1))) ==
          Right(Right(BarEvent(1)))
      }
    }
  }

  describe("#398: zero bytes") {
    it("should decode as null") {
      val option = Codec[Option[Int]].binaryDecoder(Schema.Null).value
      val unit = Codec[Unit].binaryDecoder(Schema.Null).value
      assert(option.decode(ByteVector.empty) == Right(None))
      assert(unit.decode(ByteVector.empty) == Right(()))
    }
  }

  describe("#450: options of unions") {
    it("should include the alternatives of the union") {
      assert {
        Codec[Option[SealedTraitCaseClass]].schema.value.toString ==
          """["null",{"type":"record","name":"FirstInSealedTraitCaseClass","namespace":"com.example","fields":[{"name":"value","type":"int"}]},{"type":"record","name":"SecondInSealedTraitCaseClass","namespace":"com.example","fields":[{"name":"value","type":"string"}]},{"type":"array","items":"int"}]"""
      }
    }

    it("should roundtrip") {
      List(None, Some(SecondInSealedTraitCaseClass("value"))).foreach { value =>
        assert {
          writeRead[Option[SealedTraitCaseClass], Option[SealedTraitCaseClass]](value) == Right(value)
        }
      }
    }
  }

  describe("#453: enums in other namespaces") {
    def suitCodec(namespace: String): Codec[String] =
      Codec.enumeration[String]("Suit", namespace, List("hearts", "spades"), identity, Right(_))

    it("should decode enums with the same name") {
      assert(writeRead("hearts")(suitCodec("com.writer"), suitCodec("com.reader")) == Right("hearts"))
    }

    it("should decode union alternatives with the same name") {
      assert {
        writeRead(Option("hearts"))(
          Codec.option(suitCodec("com.writer")),
          Codec.option(suitCodec("com.reader"))
        ) == Right(Some("hearts"))
      }
    }
  }

  describe("#538: named schemas defined more than once") {
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

    it("should reference identical definitions") {
      val codec = pairCodec(elementCodec("value"), elementCodec("value"))
      val pair = Pair(Element("a"), Element("b"))

      assert {
        codec.schema.value.toString ==
          """{"type":"record","name":"Pair","namespace":"com.example","fields":[{"name":"first","type":{"type":"record","name":"Element","fields":[{"name":"value","type":"string"}]}},{"name":"second","type":"Element"}]}"""
      }

      assert(writeRead(pair)(codec, codec) == Right(pair))
    }

    it("should error for conflicting definitions") {
      val codec = pairCodec(elementCodec("value"), elementCodec("other"))

      assert {
        codec.schema.swap.value.message ==
          "Conflicting definitions of named schema com.example.Element"
      }
    }
  }

  describe("#789: microsecond timestamps") {
    it("should roundtrip negative and large timestamps") {
      List(Long.MinValue, -1L, Long.MaxValue).foreach { micros =>
        val value = InstantMicros.fromEpochMicro(micros)
        assert(value.toEpochMicro == micros)
        assert(writeRead[InstantMicros, InstantMicros](value) == Right(value))
      }
    }

    it("should encode timestamps after the year 2262") {
      val value = InstantMicros(Instant.parse("3000-01-01T00:00:00.000001Z"))
      val encoded = Codec[InstantMicros].encoder.flatMap(_.encode(value))
      assert(encoded == Right(AvroValue.Long(32503680000000001L)))
    }
  }

  /**
    * Writes the value as Avro binary using the writer codec, and reads it using the reader codec, with
    * the writer schema parsed from JSON.
    */
  def writeRead[A, B](a: A)(implicit writer: Codec[A], reader: Codec[B]): Either[AvroError, B] =
    for {
      schema <- writer.schema
      writerSchema <- Schema.parse(schema.toString)
      encoder <- writer.binaryEncoder
      bytes <- encoder.encode(a)
      decoder <- reader.binaryDecoder(writerSchema)
      b <- decoder.decode(bytes)
    } yield b
}
