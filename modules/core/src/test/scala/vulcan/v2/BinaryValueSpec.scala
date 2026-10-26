/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import scodec.bits._
import vulcan.BaseSpec
import vulcan.v2.internal.{BinaryDecoder, BinaryEncoder, BinaryReader}

final class BinaryValueSpec extends BaseSpec {
  private def encode(schema: Schema, value: AvroValue): ByteVector =
    BinaryEncoder(schema).value.encode(value).value

  private def encodeError(schema: Schema, value: AvroValue): String =
    BinaryEncoder(schema).value.encode(value).swap.value.message

  /** Decodes the value, checking that all bytes were read. */
  private def decode(schema: Schema, bytes: ByteVector): AvroValue = {
    val reader = BinaryReader(bytes)
    val value = BinaryDecoder(schema).value.read(reader)
    assert(reader.remaining == 0)
    value
  }

  private def decodeError(schema: Schema, bytes: ByteVector): String =
    BinaryDecoder(schema).value.decode(bytes).swap.value.message

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

  describe("BinaryEncoder and BinaryDecoder") {
    it("should decode encoded values") {
      forAll(AvroValueGenerators.genSchemaAndValue, minSuccessful(1000)) { case (schema, value) =>
        assert(decode(schema, encode(schema, value)) == value)
      }
    }

    it("should encode values the same as the examples in the specification") {
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

      examples.foreach { case (schema, value, bytes) =>
        assert(encode(schema, value) == bytes)
        assert(decode(schema, bytes) == value)
      }
    }

    it("should encode maps, fixed, and recursive records") {
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

      examples.foreach { case (schema, value, bytes) =>
        assert(encode(schema, value) == bytes)
        assert(decode(schema, bytes) == value)
      }
    }

    it("should encode null using zero bytes") {
      assert(encode(Schema.Null, AvroValue.Null) == ByteVector.empty)
      assert(decode(Schema.Null, ByteVector.empty) == AvroValue.Null)
    }

    it("should decode arrays and maps with multiple blocks, and with block sizes") {
      val values = AvroValue.Array(Vector(AvroValue.Long(3L), AvroValue.Long(27L)))
      assert(decode(Schema.Array(Schema.Long), hex"0206023600") == values)
      assert(decode(Schema.Array(Schema.Long), hex"0304063600") == values)

      assert {
        decode(Schema.Map(Schema.Long), hex"01080261020202620400") ==
          AvroValue.Map(Vector("a" -> AvroValue.Long(1L), "b" -> AvroValue.Long(2L)))
      }
    }

    it("should not read past the end of the value") {
      val reader = BinaryReader(hex"3606666f6fff")
      BinaryDecoder(record).value.read(reader)
      assert(reader.position == 5)
    }

    it("should not encode values which do not match the schema") {
      assert(encodeError(Schema.Long, AvroValue.Int(1)) == "Invalid int value for long")
      assert(encodeError(record, AvroValue.Null) == "Invalid null value for record test")

      assert {
        encodeError(record, AvroValue.Record(Vector(AvroValue.Long(1L)))) ==
          "Invalid record with 1 values for record test with 2 fields"
      }

      assert(encodeError(enumeration, AvroValue.Enum("E")) == "Invalid symbol E for enum Foo")

      assert {
        encodeError(Schema.Fixed(name("Id"), 2), AvroValue.Fixed(hex"010203")) ==
          "Invalid fixed with 3 bytes for fixed Id with size 2"
      }

      assert {
        encodeError(nullableString, AvroValue.Union(2, AvroValue.Null)) ==
          "Invalid index 2 for union with 2 types"
      }

      assert {
        encodeError(Schema.Array(Schema.Int), AvroValue.Array(Vector(AvroValue.Long(1L)))) ==
          "Invalid long value for int"
      }
    }

    it("should not decode invalid indices") {
      assert {
        decodeError(nullableString, hex"04") ==
          "Invalid index 2 at position 0 for union with 2 types"
      }

      assert {
        decodeError(nullableString, hex"01") ==
          "Invalid index -1 at position 0 for union with 2 types"
      }

      assert {
        decodeError(enumeration, hex"08") ==
          "Invalid index 4 at position 0 for enum Foo with 4 symbols"
      }
    }

    it("should not decode truncated input") {
      assert(decodeError(record, hex"36") == "Unexpected end of input at position 1")

      assert {
        decodeError(
          Schema.Array(Schema.Long),
          hex"040600"
        ) == "Unexpected end of input at position 3"
      }
    }

    it("should not decode more items than the remaining input can hold") {
      assert {
        decodeError(Schema.Array(Schema.Long), hex"0a00") ==
          "Invalid block count 5 at position 0, exceeding the remaining input"
      }

      assert {
        decodeError(Schema.Array(Schema.Double), hex"0400000000000000f03f") ==
          "Invalid block count 2 at position 0, exceeding the remaining input"
      }

      assert {
        decodeError(Schema.Map(Schema.Null), hex"0600") ==
          "Invalid block count 3 at position 0, exceeding the remaining input"
      }
    }

    it("should limit the total number of items which are encoded using zero bytes") {
      val nulls = Schema.Array(Schema.Null)
      val decoder = BinaryDecoder(nulls, maxZeroByteItems = 2L).value
      assert(decoder.decode(hex"0400") == Right(AvroValue.Array(Vector.fill(2)(AvroValue.Null))))

      assert {
        decoder.decode(hex"0600").swap.value.message ==
          "Invalid block count 3 at position 0, exceeding the limit of 2 zero byte items"
      }

      val twoArrays =
        Schema.Record(name("Nulls"), Vector(Schema.Field("a", nulls), Schema.Field("b", nulls)))

      assert {
        BinaryDecoder(twoArrays, maxZeroByteItems = 3L).value
          .decode(hex"04000400")
          .swap
          .value
          .message ==
          "Invalid block count 2 at position 2, exceeding the limit of 3 zero byte items"
      }

      assert {
        decodeError(Schema.Array(Schema.Null), hex"feffffffffffffffff01") ==
          s"Invalid block count ${Long.MaxValue} at position 0, exceeding the limit of ${BinaryDecoder.DefaultMaxZeroByteItems} zero byte items"
      }
    }

    it("should not decode records which always contain themselves") {
      val schema =
        Schema.Record(name("Loop"), Vector(Schema.Field("loop", Schema.Ref(name("Loop")))))
      assert {
        BinaryDecoder(schema).swap.value.message ==
          "Unable to decode record Loop, since it always contains itself"
      }
    }

    it("should not compile invalid schemas") {
      val schema = Schema.Union(Vector(Schema.Int, Schema.Int))
      assert(BinaryEncoder(schema).swap.value.message == """Duplicate int in union ["int","int"]""")
      assert(BinaryDecoder(schema).swap.value.message == """Duplicate int in union ["int","int"]""")
    }
  }
}
