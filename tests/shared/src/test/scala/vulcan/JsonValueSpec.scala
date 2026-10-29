/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json
import scodec.bits.*
import vulcan.internal.JsonPrinter

final class JsonValueSpec extends BaseSpec {
  private def encode(schema: Schema, value: AvroValue): String =
    JsonPrinter.print(JsonEncoder(schema).value.encode(value).value)

  private def encodeError(schema: Schema, value: AvroValue): String =
    JsonEncoder(schema).value.encode(value).swap.value.message

  private def decode(schema: Schema, json: String): AvroValue =
    JsonDecoder(schema).value.decode(io.circe.jawn.parse(json).value).value

  private def decodeError(schema: Schema, json: String): String =
    JsonDecoder(schema).value.decode(io.circe.jawn.parse(json).value).swap.value.message

  /** A JSON string with the characters, for characters which do not need escaping. */
  private def string(chars: Int*): String =
    JsonPrinter.print(Json.fromString(chars.map(_.toChar).mkString))

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

  describe("JsonEncoder and JsonDecoder") {
    it("should decode encoded values") {
      forAll(AvroValueGenerators.genSchemaAndValue, minSuccessful(1000)) { case (schema, value) =>
        assert(decode(schema, encode(schema, value)) == value)
      }
    }

    it("should encode values in the Avro JSON encoding") {
      val examples =
        List(
          (Schema.Null, AvroValue.Null, "null"),
          (Schema.Boolean, AvroValue.Boolean(true), "true"),
          (Schema.Int, AvroValue.Int(-1), "-1"),
          (Schema.Long, AvroValue.Long(Long.MaxValue), Long.MaxValue.toString),
          (Schema.Float, AvroValue.Float(1.5f), "1.5"),
          (Schema.Double, AvroValue.Double(-2.5), "-2.5"),
          (Schema.String, AvroValue.String("foo"), "\"foo\""),
          (
            record,
            AvroValue.Record(Vector(AvroValue.Long(27L), AvroValue.String("foo"))),
            """{"a":27,"b":"foo"}"""
          ),
          (enumeration, AvroValue.Enum("D"), "\"D\""),
          (
            Schema.Array(Schema.Long),
            AvroValue.Array(Vector(AvroValue.Long(3L), AvroValue.Long(27L))),
            "[3,27]"
          ),
          (
            Schema.Map(Schema.Long),
            AvroValue.Map(Vector("a" -> AvroValue.Long(1L))),
            """{"a":1}"""
          ),
          (nullableString, AvroValue.Union(0, AvroValue.Null), "null"),
          (nullableString, AvroValue.Union(1, AvroValue.String("a")), """{"string":"a"}"""),
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
            """{"value":1,"next":{"Node":{"value":2,"next":null}}}"""
          )
        )

      examples.foreach { case (schema, value, json) =>
        assert(encode(schema, value) == json)
        assert(decode(schema, json) == value)
      }
    }

    it("should encode bytes and fixed as strings with one character per byte") {
      assert(encode(Schema.Bytes, AvroValue.Bytes(hex"41e9ff")) == string(0x41, 0xe9, 0xff))
      assert(decode(Schema.Bytes, string(0x41, 0xe9, 0xff)) == AvroValue.Bytes(hex"41e9ff"))

      val fixed = Schema.Fixed(name("Id"), 2)
      assert(encode(fixed, AvroValue.Fixed(hex"4142")) == "\"AB\"")
      assert(decode(fixed, "\"AB\"") == AvroValue.Fixed(hex"4142"))
    }

    it("should encode floats and doubles which are not finite as strings") {
      assert(encode(Schema.Float, AvroValue.Float(Float.NaN)) == "\"NaN\"")
      assert(encode(Schema.Float, AvroValue.Float(Float.PositiveInfinity)) == "\"Infinity\"")
      assert(encode(Schema.Double, AvroValue.Double(Double.NegativeInfinity)) == "\"-Infinity\"")
    }

    it("should decode the same as the Java Avro library") {
      assert(decode(Schema.Int, "1.0") == AvroValue.Int(1))
      assert(decode(Schema.Long, "1e2") == AvroValue.Long(100L))
      assert(decode(Schema.Float, "\"INF\"") == AvroValue.Float(Float.PositiveInfinity))
      assert(decode(Schema.Double, "\"-INF\"") == AvroValue.Double(Double.NegativeInfinity))
      assert(decode(Schema.Double, "\"NaN\"") == AvroValue.Double(Double.NaN))
      assert(decode(nullableString, """{"null":null}""") == AvroValue.Union(0, AvroValue.Null))

      assert {
        decode(record, """{"b":"foo","a":27}""") ==
          AvroValue.Record(Vector(AvroValue.Long(27L), AvroValue.String("foo")))
      }

      val aliased =
        Schema.Record(name("test"), Vector(Schema.Field("a", Schema.Long).withAliases(Vector("x"))))

      assert(decode(aliased, """{"x":1}""") == AvroValue.Record(Vector(AvroValue.Long(1L))))
    }

    it("should not encode values which do not match the schema") {
      assert(encodeError(Schema.Long, AvroValue.Int(1)) == "Invalid int value for long")

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
    }

    it("should not decode values which do not match the schema") {
      assert(decodeError(Schema.Int, "1.5") == "Invalid number 1.5 for int")
      assert(decodeError(Schema.Long, "\"1\"") == "Invalid string for long")
      assert(decodeError(Schema.String, "1") == "Invalid number 1 for string")
      assert(decodeError(Schema.Array(Schema.Int), "{}") == "Invalid object for array")
      assert(decodeError(record, """{"a":27}""") == "Missing field b for record test")

      assert {
        decodeError(record, """{"a":27,"b":"foo","c":1,"d":2}""") ==
          "Unknown fields c, d for record test"
      }

      assert(decodeError(enumeration, "\"E\"") == "Invalid symbol E for enum Foo")

      assert {
        decodeError(Schema.Fixed(name("Id"), 2), "\"ABC\"") ==
          "Invalid fixed with 3 bytes for fixed Id with size 2"
      }

      assert(decodeError(nullableString, """{"int":1}""") == "Unknown type int for union")

      assert {
        decodeError(nullableString, """{"null":null,"string":"a"}""") ==
          "Invalid object with 2 keys for union, expected one key"
      }

      assert(decodeError(Schema.Union(Vector(Schema.Int)), "null") == "Invalid null for union")
    }

    it("should not compile invalid schemas") {
      val undefined = Schema.Ref(name("Undefined"))
      assert(JsonEncoder(undefined).swap.value.message == "Undefined name Undefined")
      assert(JsonDecoder(undefined).swap.value.message == "Undefined name Undefined")
    }
  }
}
