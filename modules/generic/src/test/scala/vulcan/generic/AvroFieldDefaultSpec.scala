/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import io.circe.Json
import vulcan.BaseSpec
import vulcan.Codec
import vulcan.CodecSpecHelpers
import vulcan.Schema
import vulcan.generic.examples.AvroRecordDefault.*

final class AvroFieldDefaultSpec extends BaseSpec with CodecSpecHelpers {
  describe("AvroFieldDefault") {
    it("should create a schema with a default for a field") {
      assert(fieldDefault[Foo]("a") == Some(Json.fromInt(1)))
      assert(fieldDefault[Foo]("b") == Some(Json.fromString("foo")))
      assert(fieldDefault[Foo]("c") == Some(Json.Null))
    }

    it("should succeed when annotating an Option") {
      assertSchemaIs[HasOptionDefault](
        """{"type":"record","name":"HasOptionDefault","namespace":"vulcan.generic.examples.AvroRecordDefault","fields":[{"name":"a","type":["null","string"],"default":"foo"}]}"""
      )
    }

    it("should succeed when annotating an enum first element") {
      assert(fieldDefault[HasSFirst]("s") == Some(Json.fromString("A")))
    }

    it("should succeed when annotating an enum second element") {
      assert(fieldDefault[HasSSecond]("s") == Some(Json.fromString("B")))
    }

    it("should succeed with the first member of a union") {
      assertSchemaIs[HasUnion](
        """{"type":"record","name":"HasUnion","namespace":"vulcan.generic.examples.AvroRecordDefault","fields":[{"name":"u","type":[{"type":"record","name":"A","namespace":"vulcan.generic.examples.AvroRecordDefault.Union","fields":[{"name":"a","type":"int"}]},{"type":"record","name":"B","namespace":"vulcan.generic.examples.AvroRecordDefault.Union","fields":[{"name":"b","type":"string"}]}],"default":{"a":1}}]}"""
      )
      val result = Codec[HasUnion].decode(unsafeEncode(Empty()), unsafeSchema[Empty])
      assert(result.value == HasUnion(Union.A(1)))
    }

    it("should succeed with the second member of a union") {
      assertSchemaIs[HasUnionSecond](
        """{"type":"record","name":"HasUnionSecond","namespace":"vulcan.generic.examples.AvroRecordDefault","fields":[{"name":"u","type":[{"type":"record","name":"A","namespace":"vulcan.generic.examples.AvroRecordDefault.Union","fields":[{"name":"a","type":"int"}]},{"type":"record","name":"B","namespace":"vulcan.generic.examples.AvroRecordDefault.Union","fields":[{"name":"b","type":"string"}]}],"default":{"b":"foo"}}]}"""
      )
      val result = Codec[HasUnionSecond].decode(unsafeEncode(Empty()), unsafeSchema[Empty])
      assert(result.value == HasUnionSecond(Union.B("foo")))
    }
  }

  def fieldDefault[A](name: String)(implicit codec: Codec[A]): Option[Json] =
    unsafeSchema[A] match {
      case record: Schema.Record => record.fields.find(_.name == name).flatMap(_.default)
      case _ => None
    }
}
