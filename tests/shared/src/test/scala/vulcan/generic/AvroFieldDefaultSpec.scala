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
import vulcan.generic.examples.AvroRecordDefault.*

object AvroFieldDefaultSpec extends BaseSuite {
  private def fieldDefault[A](name: String)(implicit codec: Codec[A]): Option[Json] =
    codec.schema match {
      case Right(record: Schema.Record) => record.fields.find(_.name == name).flatMap(_.default)
      case _ => None
    }

  pureTest("AvroFieldDefault should create a schema with a default for a field") {
    expect.eql(Some(Json.fromInt(1)), fieldDefault[Foo]("a")) &&
    expect.eql(Some(Json.fromString("foo")), fieldDefault[Foo]("b")) &&
    expect.eql(Some(Json.Null), fieldDefault[Foo]("c"))
  }

  pureTest("AvroFieldDefault should succeed when annotating an Option") {
    expectSchemaIs[HasOptionDefault](
      """{"type":"record","name":"HasOptionDefault","namespace":"vulcan.generic.examples.AvroRecordDefault","fields":[{"name":"a","type":["null","string"],"default":"foo"}]}"""
    )
  }

  pureTest("AvroFieldDefault should succeed when annotating an enum first element") {
    expect.eql(Some(Json.fromString("A")), fieldDefault[HasSFirst]("s"))
  }

  pureTest("AvroFieldDefault should succeed when annotating an enum second element") {
    expect.eql(Some(Json.fromString("B")), fieldDefault[HasSSecond]("s"))
  }

  pureTest("AvroFieldDefault should succeed with the first member of a union") {
    expectSchemaIs[HasUnion](
      """{"type":"record","name":"HasUnion","namespace":"vulcan.generic.examples.AvroRecordDefault","fields":[{"name":"u","type":[{"type":"record","name":"A","namespace":"vulcan.generic.examples.AvroRecordDefault.Union","fields":[{"name":"a","type":"int"}]},{"type":"record","name":"B","namespace":"vulcan.generic.examples.AvroRecordDefault.Union","fields":[{"name":"b","type":"string"}]}],"default":{"a":1}}]}"""
    ) &&
    expectDecodeIs[HasUnion](
      Codec[Empty].encode(Empty()),
      Right(HasUnion(Union.A(1))),
      Codec[Empty].schema
    )
  }

  pureTest("AvroFieldDefault should succeed with the second member of a union") {
    expectSchemaIs[HasUnionSecond](
      """{"type":"record","name":"HasUnionSecond","namespace":"vulcan.generic.examples.AvroRecordDefault","fields":[{"name":"u","type":[{"type":"record","name":"A","namespace":"vulcan.generic.examples.AvroRecordDefault.Union","fields":[{"name":"a","type":"int"}]},{"type":"record","name":"B","namespace":"vulcan.generic.examples.AvroRecordDefault.Union","fields":[{"name":"b","type":"string"}]}],"default":{"b":"foo"}}]}"""
    ) &&
    expectDecodeIs[HasUnionSecond](
      Codec[Empty].encode(Empty()),
      Right(HasUnionSecond(Union.B("foo"))),
      Codec[Empty].schema
    )
  }
}
