/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import io.circe.Json
import vulcan.BaseSpec
import vulcan.Codec
import vulcan.Schema
import vulcan.generic.examples.CaseClassAvroNullDefault

final class AvroNullDefaultSpec extends BaseSpec {
  describe("AvroNullDefault") {
    it("should provide null default flag via enabled") {
      forAll { (b: Boolean) =>
        assert(new AvroNullDefault(b).enabled == b)
      }
    }

    it("should include enabled flag value in toString") {
      forAll { (b: Boolean) =>
        assert(new AvroNullDefault(b).toString.contains(b.toString))
      }
    }

    it("should provide an extractor for enabled flag") {
      forAll { (b1: Boolean) =>
        assert(new AvroNullDefault(b1) match {
          case AvroNullDefault(`b1`) => true
          case AvroNullDefault(b2) => fail(b2.toString)
        })
      }
    }

    it("should decode null default when field is missing from writer schema") {
      case class WriterCaseClass(int: Int)

      implicit val writerCodec: Codec[WriterCaseClass] =
        Codec.record("CaseClassAvroNullDefault", "vulcan.generic.examples") { f =>
          f("int", _.int).map { WriterCaseClass(_) }
        }

      val decoder = Codec[CaseClassAvroNullDefault].binaryDecoder(writerCodec.schema.value).value
      val decoded = writerCodec.binaryEncoder.value.encode(WriterCaseClass(3)).flatMap(decoder.decode)

      assert(decoded.value == CaseClassAvroNullDefault(Some(3), None, None, None, None, None, None))

    }

    it("should only include null defaults for fields with null values") {
      val schema = Codec[AvroNullDefaultSpec.NullableFields].schema.value
      val defaults = schema.asInstanceOf[Schema.Record].fields.map(field => (field.name, field.default))

      assert {
        defaults == Vector(
          "option" -> Some(Json.Null),
          "unit" -> Some(Json.Null),
          "none" -> Some(Json.Null),
          "either" -> None
        )
      }
    }
  }
}

object AvroNullDefaultSpec {
  @AvroNullDefault(true)
  final case class NullableFields(option: Option[Int], unit: Unit, none: None.type, either: Either[Unit, Int])

  object NullableFields {
    implicit val codec: Codec[NullableFields] =
      Codec.derive
  }
}
