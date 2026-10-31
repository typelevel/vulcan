/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import scodec.bits.*
import vulcan.internal.BinaryReader
import vulcan.internal.BinaryWriter

object BinaryValueSpec extends BaseSuite {
  private def encode(schema: Schema, value: AvroValue): Either[String, ByteVector] =
    BinaryEncoder(schema).flatMap(_.encode(value)).left.map(_.message)

  /** Decodes the value, failing with the number of bytes left unread if not all bytes were read. */
  private def decode(schema: Schema, bytes: ByteVector): Either[String, AvroValue] =
    BinaryDecoder.reader(schema).left.map(_.message).flatMap { read =>
      val reader = BinaryReader(bytes)
      try {
        val value = read(reader)
        Either.cond(reader.remaining == 0, value, s"${reader.remaining} bytes left unread")
      } catch {
        case e: AvroException => Left(e.message)
      }
    }

  /** Block counts without items, which is valid for items encoded using zero bytes. */
  private def blockCounts(counts: Long*): ByteVector = {
    val writer = new BinaryWriter(0)
    counts.foreach(writer.writeLong)
    writer.toByteVector
  }

  private def name(name: String): Schema.Name =
    Schema.Name(name)

  private val record: Schema =
    Schema.Record(
      name("test"),
      Vector(Schema.Field("a", Schema.Long), Schema.Field("b", Schema.String))
    )

  private val enumeration: Schema =
    Schema.Enum(name("Foo"), Vector("A", "B", "C", "D"))

  private val nullableString: Schema =
    Schema.Union(Vector(Schema.Null, Schema.String))

  /** A linked list of ints. */
  private val node: Schema =
    Schema.Record(
      name("Node"),
      Vector(
        Schema.Field("value", Schema.Int),
        Schema.Field("next", Schema.Union(Vector(Schema.Null, Schema.Ref(name("Node")))))
      )
    )

  test("BinaryEncoder and BinaryDecoder should decode encoded values") {
    forall.withConfig(checkConfig.withMinimumSuccessful(1000))(AvroValueGenerators.genSchemaAndValue) {
      case (schema, value) =>
        expect.same(Right(value), encode(schema, value).flatMap(decode(schema, _)))
    }
  }

  pureTest(
    "BinaryEncoder and BinaryDecoder should encode values the same as the examples in the specification"
  ) {
    val examples =
      List(
        (
          record,
          AvroValue.Record(Vector(AvroValue.Long(27L), AvroValue.String("foo"))),
          hex"3606666f6f"
        ),
        (enumeration, AvroValue.Enum("D"), hex"06"),
        (
          Schema.Array(Schema.Long),
          AvroValue.Array(Vector(AvroValue.Long(3L), AvroValue.Long(27L))),
          hex"04063600"
        ),
        (nullableString, AvroValue.Union(0, AvroValue.Null), hex"00"),
        (nullableString, AvroValue.Union(1, AvroValue.String("a")), hex"020261")
      )

    forEach(examples) { case (schema, value, bytes) =>
      expect.same(Right(bytes), encode(schema, value)) &&
      expect.same(Right(value), decode(schema, bytes))
    }
  }

  pureTest("BinaryEncoder and BinaryDecoder should encode maps, fixed, and recursive records") {
    val examples =
      List(
        (
          Schema.Map(Schema.Long),
          AvroValue.Map(Vector("a" -> AvroValue.Long(1L))),
          hex"0202610200"
        ),
        (Schema.Map(Schema.Long), AvroValue.Map(Vector.empty), hex"00"),
        (Schema.Fixed(name("Id"), 2), AvroValue.Fixed(hex"0102"), hex"0102"),
        (
          node,
          AvroValue.Record(
            Vector(
              AvroValue.Int(1),
              AvroValue.Union(
                1,
                AvroValue.Record(Vector(AvroValue.Int(2), AvroValue.Union(0, AvroValue.Null)))
              )
            )
          ),
          hex"02020400"
        )
      )

    forEach(examples) { case (schema, value, bytes) =>
      expect.same(Right(bytes), encode(schema, value)) &&
      expect.same(Right(value), decode(schema, bytes))
    }
  }

  pureTest("BinaryEncoder and BinaryDecoder should encode values of different sizes using the same encoder") {
    val encoder = BinaryEncoder(Schema.String)

    forEach(List("a" * 1000, "b", "c" * 100, "", "d" * 10000)) { string =>
      val bytes = encoder.flatMap(_.encode(AvroValue.String(string))).left.map(_.message)
      expect.same(Right(AvroValue.String(string)), bytes.flatMap(decode(Schema.String, _)))
    }
  }

  pureTest("BinaryEncoder and BinaryDecoder should encode null using zero bytes") {
    expect.same(Right(ByteVector.empty), encode(Schema.Null, AvroValue.Null)) &&
    expect.same(Right(AvroValue.Null), decode(Schema.Null, ByteVector.empty))
  }

  pureTest(
    "BinaryEncoder and BinaryDecoder should decode arrays and maps with multiple blocks, and with block sizes"
  ) {
    val values = AvroValue.Array(Vector(AvroValue.Long(3L), AvroValue.Long(27L)))
    expect.same(Right(values), decode(Schema.Array(Schema.Long), hex"0206023600")) &&
    expect.same(Right(values), decode(Schema.Array(Schema.Long), hex"0304063600")) &&
    expect.same(
      Right(AvroValue.Map(Vector("a" -> AvroValue.Long(1L), "b" -> AvroValue.Long(2L)))),
      decode(Schema.Map(Schema.Long), hex"01080261020202620400")
    )
  }

  pureTest("BinaryEncoder and BinaryDecoder should not read past the end of the value") {
    val reader = BinaryReader(hex"3606666f6fff")
    whenSuccess(BinaryDecoder.reader(record)) { read =>
      expect.same(AvroValue.Record(Vector(AvroValue.Long(27L), AvroValue.String("foo"))), read(reader)) &&
      expect.eql(5, reader.position)
    }
  }

  pureTest("BinaryEncoder and BinaryDecoder should not encode values which do not match the schema") {
    expect.same(Left("Invalid int value for long"), encode(Schema.Long, AvroValue.Int(1))) &&
    expect.same(Left("Invalid null value for record test"), encode(record, AvroValue.Null)) &&
    expect.same(
      Left("Invalid record with 1 values for record test with 2 fields"),
      encode(record, AvroValue.Record(Vector(AvroValue.Long(1L))))
    ) &&
    expect.same(Left("Invalid symbol E for enum Foo"), encode(enumeration, AvroValue.Enum("E"))) &&
    expect.same(
      Left("Invalid fixed with 3 bytes for fixed Id with size 2"),
      encode(Schema.Fixed(name("Id"), 2), AvroValue.Fixed(hex"010203"))
    ) &&
    expect.same(
      Left("Invalid index 2 for union with 2 types"),
      encode(nullableString, AvroValue.Union(2, AvroValue.Null))
    ) &&
    expect.same(
      Left("Invalid long value for int"),
      encode(Schema.Array(Schema.Int), AvroValue.Array(Vector(AvroValue.Long(1L))))
    )
  }

  pureTest("BinaryEncoder and BinaryDecoder should not decode invalid indices") {
    expect.same(
      Left("Invalid index 2 at position 0 for union with 2 types"),
      decode(nullableString, hex"04")
    ) &&
    expect.same(
      Left("Invalid index -1 at position 0 for union with 2 types"),
      decode(nullableString, hex"01")
    ) &&
    expect.same(
      Left("Invalid index 4 at position 0 for enum Foo with 4 symbols"),
      decode(enumeration, hex"08")
    )
  }

  pureTest("BinaryEncoder and BinaryDecoder should not decode truncated input") {
    expect.same(Left("Unexpected end of input at position 1"), decode(record, hex"36")) &&
    expect.same(
      Left("Unexpected end of input at position 3"),
      decode(Schema.Array(Schema.Long), hex"040600")
    )
  }

  pureTest("BinaryEncoder and BinaryDecoder should not decode more items than the remaining input can hold") {
    expect.same(
      Left("Invalid block count 5 at position 0, exceeding the remaining input"),
      decode(Schema.Array(Schema.Long), hex"0a00")
    ) &&
    expect.same(
      Left("Invalid block count 2 at position 0, exceeding the remaining input"),
      decode(Schema.Array(Schema.Double), hex"0400000000000000f03f")
    ) &&
    expect.same(
      Left("Invalid block count 3 at position 0, exceeding the remaining input"),
      decode(Schema.Map(Schema.Null), hex"0600")
    )
  }

  pureTest(
    "BinaryEncoder and BinaryDecoder should limit the total number of items which are encoded using zero bytes"
  ) {
    val nulls = Schema.Array(Schema.Null)
    val limit = BinaryDecoder.MaxZeroByteItems

    val twoArrays =
      Schema.Record(name("Nulls"), Vector(Schema.Field("a", nulls), Schema.Field("b", nulls)))

    val first = limit / 2 + 1
    val second = limit - first + 1
    val position = blockCounts(first, 0L).size

    expect.same(
      Right(AvroValue.Array(Vector.fill(limit.toInt)(AvroValue.Null))),
      decode(nulls, blockCounts(limit, 0L))
    ) &&
    expect.same(
      Left(s"Invalid block count ${limit + 1} at position 0, exceeding the limit of $limit zero byte items"),
      decode(nulls, blockCounts(limit + 1, 0L))
    ) &&
    expect.same(
      Left(
        s"Invalid block count $second at position $position, exceeding the limit of $limit zero byte items"
      ),
      decode(twoArrays, blockCounts(first, 0L, second, 0L))
    ) &&
    expect.same(
      Left(
        s"Invalid block count ${Long.MaxValue} at position 0, exceeding the limit of $limit zero byte items"
      ),
      decode(nulls, blockCounts(Long.MaxValue))
    )
  }

  pureTest("BinaryEncoder and BinaryDecoder should limit the number of nested values of recursive schemas") {
    val limit = BinaryDecoder.MaxDepth

    def nodes(count: Int): ByteVector = {
      val writer = new BinaryWriter(0)
      (1 until count).foreach { _ =>
        writer.writeInt(0)
        writer.writeInt(1)
      }
      writer.writeInt(0)
      writer.writeInt(0)
      writer.toByteVector
    }

    val error =
      s"Invalid depth ${limit + 1} at position ${2 * (limit + 1)}, exceeding the limit of $limit nested values of recursive schemas"

    expect(decode(node, nodes(limit + 1)).isRight) &&
    expect.same(Left(error), decode(node, nodes(limit + 2))) &&
    expect.same(Left(error), decode(node, nodes(100000)))
  }

  pureTest("BinaryEncoder and BinaryDecoder should not decode records which always contain themselves") {
    val schema =
      Schema.Record(name("Loop"), Vector(Schema.Field("loop", Schema.Ref(name("Loop")))))
    expect.same(
      Left("Unable to decode record Loop, since it always contains itself"),
      BinaryDecoder(schema).left.map(_.message)
    )
  }

  pureTest("BinaryEncoder and BinaryDecoder should not compile invalid schemas") {
    val schema = Schema.Union(Vector(Schema.Int, Schema.Int))
    val undefined = Schema.Ref(name("Undefined"))
    expect.same(
      Left("""Duplicate int in union ["int","int"]"""),
      BinaryEncoder(schema).left.map(_.message)
    ) &&
    expect.same(
      Left("""Duplicate int in union ["int","int"]"""),
      BinaryDecoder(schema).left.map(_.message)
    ) &&
    expect.same(Left("Undefined name Undefined"), BinaryEncoder(undefined).left.map(_.message)) &&
    expect.same(Left("Undefined name Undefined"), BinaryDecoder(undefined).left.map(_.message))
  }
}
