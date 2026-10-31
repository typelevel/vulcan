/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import io.circe.Json
import vulcan.BaseSuite
import vulcan.Codec
import vulcan.Schema
import vulcan.generic.examples.CaseClassAvroNullDefault

object AvroNullDefaultSpec extends BaseSuite {
  test("AvroNullDefault should provide null default flag via enabled") {
    forall { (b: Boolean) =>
      expect.eql(b, new AvroNullDefault(b).enabled)
    }
  }

  test("AvroNullDefault should include enabled flag value in toString") {
    forall { (b: Boolean) =>
      expect(new AvroNullDefault(b).toString.contains(b.toString))
    }
  }

  test("AvroNullDefault should provide an extractor for enabled flag") {
    forall { (b1: Boolean) =>
      matches(new AvroNullDefault(b1)) { case AvroNullDefault(b2) =>
        expect.eql(b1, b2)
      }
    }
  }

  pureTest("AvroNullDefault should decode null default when field is missing from writer schema") {
    case class WriterCaseClass(int: Int)

    implicit val writerCodec: Codec[WriterCaseClass] =
      Codec.record("CaseClassAvroNullDefault", "vulcan.generic.examples") { f =>
        f("int", _.int).map { WriterCaseClass(_) }
      }

    val decoded =
      for {
        schema <- writerCodec.schema
        binary <- writerCodec.binaryEncoder.flatMap(_.encode(WriterCaseClass(3)))
        decoded <- Codec[CaseClassAvroNullDefault].binaryDecoder(schema).flatMap(_.decode(binary))
      } yield decoded

    expect.same(Right(CaseClassAvroNullDefault(Some(3), None, None, None, None, None, None)), decoded)
  }

  pureTest("AvroNullDefault should only include null defaults for fields with null values") {
    matches(Codec[NullableFields].schema) { case Right(record: Schema.Record) =>
      expect.eql(
        Vector(
          "option" -> Some(Json.Null),
          "unit" -> Some(Json.Null),
          "none" -> Some(Json.Null),
          "either" -> None
        ),
        record.fields.map(field => (field.name, field.default))
      )
    }
  }

  @AvroNullDefault(true)
  final case class NullableFields(option: Option[Int], unit: Unit, none: None.type, either: Either[Unit, Int])

  object NullableFields {
    implicit val codec: Codec[NullableFields] =
      Codec.derive
  }
}
