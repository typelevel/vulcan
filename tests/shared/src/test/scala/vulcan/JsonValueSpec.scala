/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json
import scodec.bits.*
import vulcan.internal.JsonPrinter

object JsonValueSpec extends BaseSuite {
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

  test("JsonEncoder and JsonDecoder should decode encoded values") {
    forall.withConfig(checkConfig.withMinimumSuccessful(1000))(AvroValueGenerators.genSchemaAndValue) {
      case (schema, value) =>
        expect.same(value, decode(schema, encode(schema, value)))
    }
  }

  pureTest("JsonEncoder and JsonDecoder should encode values in the Avro JSON encoding") {
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

    forEach(examples) { case (schema, value, json) =>
      expect.eql(json, encode(schema, value)) &&
      expect.same(value, decode(schema, json))
    }
  }

  pureTest(
    "JsonEncoder and JsonDecoder should encode bytes and fixed as strings with one character per byte"
  ) {
    val fixed = Schema.Fixed(name("Id"), 2)
    expect.eql(string(0x41, 0xe9, 0xff), encode(Schema.Bytes, AvroValue.Bytes(hex"41e9ff"))) &&
    expect.same(AvroValue.Bytes(hex"41e9ff"), decode(Schema.Bytes, string(0x41, 0xe9, 0xff))) &&
    expect.eql("\"AB\"", encode(fixed, AvroValue.Fixed(hex"4142"))) &&
    expect.same(AvroValue.Fixed(hex"4142"), decode(fixed, "\"AB\""))
  }

  pureTest("JsonEncoder and JsonDecoder should encode floats and doubles which are not finite as strings") {
    expect.eql("\"NaN\"", encode(Schema.Float, AvroValue.Float(Float.NaN))) &&
    expect.eql("\"Infinity\"", encode(Schema.Float, AvroValue.Float(Float.PositiveInfinity))) &&
    expect.eql("\"-Infinity\"", encode(Schema.Double, AvroValue.Double(Double.NegativeInfinity)))
  }

  pureTest("JsonEncoder and JsonDecoder should decode the same as the Java Avro library") {
    val aliased =
      Schema.Record(name("test"), Vector(Schema.Field("a", Schema.Long).withAliases(Vector("x"))))

    expect.same(AvroValue.Int(1), decode(Schema.Int, "1.0")) &&
    expect.same(AvroValue.Long(100L), decode(Schema.Long, "1e2")) &&
    expect.same(AvroValue.Float(Float.PositiveInfinity), decode(Schema.Float, "\"INF\"")) &&
    expect.same(AvroValue.Double(Double.NegativeInfinity), decode(Schema.Double, "\"-INF\"")) &&
    expect.same(AvroValue.Double(Double.NaN), decode(Schema.Double, "\"NaN\"")) &&
    expect.same(AvroValue.Union(0, AvroValue.Null), decode(nullableString, """{"null":null}""")) &&
    expect.same(
      AvroValue.Record(Vector(AvroValue.Long(27L), AvroValue.String("foo"))),
      decode(record, """{"b":"foo","a":27}""")
    ) &&
    expect.same(AvroValue.Record(Vector(AvroValue.Long(1L))), decode(aliased, """{"x":1}"""))
  }

  pureTest("JsonEncoder and JsonDecoder should not encode values which do not match the schema") {
    expect.eql("Invalid int value for long", encodeError(Schema.Long, AvroValue.Int(1))) &&
    expect.eql(
      "Invalid record with 1 values for record test with 2 fields",
      encodeError(record, AvroValue.Record(Vector(AvroValue.Long(1L))))
    ) &&
    expect.eql("Invalid symbol E for enum Foo", encodeError(enumeration, AvroValue.Enum("E"))) &&
    expect.eql(
      "Invalid fixed with 3 bytes for fixed Id with size 2",
      encodeError(Schema.Fixed(name("Id"), 2), AvroValue.Fixed(hex"010203"))
    ) &&
    expect.eql(
      "Invalid index 2 for union with 2 types",
      encodeError(nullableString, AvroValue.Union(2, AvroValue.Null))
    )
  }

  pureTest("JsonEncoder and JsonDecoder should not decode values which do not match the schema") {
    val colliding =
      Schema.Record(
        name("test"),
        Vector(Schema.Field("a", Schema.Long).withAliases(Vector("b")), Schema.Field("b", Schema.Long))
      )

    expect.eql("Invalid number 1.5 for int", decodeError(Schema.Int, "1.5")) &&
    expect.eql("Invalid string for long", decodeError(Schema.Long, "\"1\"")) &&
    expect.eql("Invalid number 1 for string", decodeError(Schema.String, "1")) &&
    expect.eql("Invalid object for array", decodeError(Schema.Array(Schema.Int), "{}")) &&
    expect.eql("Missing field b for record test", decodeError(record, """{"a":27}""")) &&
    expect.eql(
      "Unknown fields c, d for record test",
      decodeError(record, """{"a":27,"b":"foo","c":1,"d":2}""")
    ) &&
    expect.eql("Unknown fields c for record test", decodeError(colliding, """{"b":1,"c":2}""")) &&
    expect.eql("Invalid symbol E for enum Foo", decodeError(enumeration, "\"E\"")) &&
    expect.eql(
      "Invalid fixed with 3 bytes for fixed Id with size 2",
      decodeError(Schema.Fixed(name("Id"), 2), "\"ABC\"")
    ) &&
    expect.eql("Unknown type int for union", decodeError(nullableString, """{"int":1}""")) &&
    expect.eql(
      "Invalid object with 2 keys for union, expected one key",
      decodeError(nullableString, """{"null":null,"string":"a"}""")
    ) &&
    expect.eql("Invalid null for union", decodeError(Schema.Union(Vector(Schema.Int)), "null"))
  }

  pureTest("JsonEncoder and JsonDecoder should limit the number of nested values of recursive schemas") {
    val limit = JsonDecoder.MaxDepth

    def nodes(count: Int): Json =
      (1 until count).foldLeft(Json.obj("value" -> Json.fromInt(0), "next" -> Json.Null)) { (next, _) =>
        Json.obj("value" -> Json.fromInt(0), "next" -> Json.obj("Node" -> next))
      }

    val decoder = JsonDecoder(node).value

    val error =
      s"Invalid depth ${limit + 1}, exceeding the limit of $limit nested values of recursive schemas"

    expect(decoder.decode(nodes(limit + 1)).isRight) &&
    expect.eql(error, decoder.decode(nodes(limit + 2)).swap.value.message) &&
    expect.eql(error, decoder.decode(nodes(100000)).swap.value.message)
  }

  pureTest("JsonEncoder and JsonDecoder should not compile invalid schemas") {
    val undefined = Schema.Ref(name("Undefined"))
    expect.eql("Undefined name Undefined", JsonEncoder(undefined).swap.value.message) &&
    expect.eql("Undefined name Undefined", JsonDecoder(undefined).swap.value.message)
  }
}
