/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json
import io.circe.JsonObject

final class SchemaToStringSpec extends BaseSpec {
  private def record(
    name: String,
    namespace: Option[String],
    fields: Schema.Field*
  ): Schema.Record =
    Schema.Record(Schema.Name(name, namespace), fields.toVector)

  describe("Schema") {
    describe("toJson") {
      it("should return the schema as JSON") {
        assert {
          Schema.Union(Vector(Schema.Null, Schema.Int)).toJson ==
            Json.arr(Json.fromString("null"), Json.fromString("int"))
        }
      }
    }

    describe("toString") {
      it("should print primitives without props as type names") {
        assert(Schema.Null.toString == "\"null\"")
        assert(Schema.Boolean.toString == "\"boolean\"")
        assert(Schema.Int.toString == "\"int\"")
        assert(Schema.Long.toString == "\"long\"")
        assert(Schema.Float.toString == "\"float\"")
        assert(Schema.Double.toString == "\"double\"")
        assert(Schema.Bytes.toString == "\"bytes\"")
        assert(Schema.String.toString == "\"string\"")
      }

      it("should print primitives with props as objects") {
        val props = JsonObject("avro.java.string" -> Json.fromString("String"))
        assert(
          Schema.String
            .withProps(props)
            .toString == """{"type":"string","avro.java.string":"String"}"""
        )
      }

      it("should print logical types followed by their parameters") {
        assert {
          Schema.Long.withLogicalType(LogicalType.timestampMillis).toString ==
            """{"type":"long","logicalType":"timestamp-millis"}"""
        }

        assert {
          Schema.Bytes.withLogicalType(LogicalType.decimal(10, 2)).toString ==
            """{"type":"bytes","logicalType":"decimal","precision":10,"scale":2}"""
        }
      }

      it("should print arrays, maps, and unions") {
        assert(Schema.Array(Schema.Int).toString == """{"type":"array","items":"int"}""")

        assert {
          Schema.Map(Schema.String).withProps(JsonObject("custom" -> Json.True)).toString ==
            """{"type":"map","values":"string","custom":true}"""
        }

        assert(Schema.Union(Vector(Schema.Null, Schema.Int)).toString == """["null","int"]""")
      }

      it("should print records in the same order as the Java Avro library") {
        val schema =
          record(
            "Person",
            Some("com.example"),
            Schema
              .Field("name", Schema.String)
              .withDoc(Some("The full name"))
              .withAliases(Vector("fullName")),
            Schema
              .Field("age", Schema.Union(Vector(Schema.Null, Schema.Int)))
              .withDefault(Some(Json.Null))
              .withOrder(Schema.Field.Order.Descending)
              .withProps(JsonObject("custom" -> Json.True))
          )
            .withDoc(Some("A person"))
            .withAliases(Vector("Human"))
            .withProps(JsonObject("custom" -> Json.fromInt(1)))

        assert {
          schema.toString ==
            """{"type":"record","name":"Person","namespace":"com.example","doc":"A person","fields":[{"name":"name","type":"string","doc":"The full name","aliases":["fullName"]},{"name":"age","type":["null","int"],"default":null,"order":"descending","custom":true}],"custom":1,"aliases":["Human"]}"""
        }
      }

      it("should print enums in the same order as the Java Avro library") {
        val schema =
          Schema
            .Enum(Schema.Name("Suit", Some("com.example")), Vector("hearts", "spades"))
            .withDefault(Some("spades"))
            .withDoc(Some("A suit"))
            .withAliases(Vector("Color"))
            .withProps(JsonObject("custom" -> Json.True))

        assert {
          schema.toString ==
            """{"type":"enum","name":"Suit","namespace":"com.example","doc":"A suit","symbols":["hearts","spades"],"default":"spades","custom":true,"aliases":["Color"]}"""
        }
      }

      it("should print fixed in the same order as the Java Avro library") {
        val schema =
          Schema
            .Fixed(Schema.Name("Id", Some("com.example")), 16)
            .withLogicalType(LogicalType.uuid)
            .withDoc(Some("An id"))
            .withAliases(Vector("Identifier"))

        assert {
          schema.toString ==
            """{"type":"fixed","name":"Id","namespace":"com.example","doc":"An id","size":16,"logicalType":"uuid","aliases":["Identifier"]}"""
        }
      }

      it("should only include namespaces which differ from the enclosing namespace") {
        val sameNamespace =
          record(
            "Outer",
            Some("com.example"),
            Schema.Field("inner", record("Inner", Some("com.example")))
          )

        assert {
          sameNamespace.toString ==
            """{"type":"record","name":"Outer","namespace":"com.example","fields":[{"name":"inner","type":{"type":"record","name":"Inner","fields":[]}}]}"""
        }

        val otherNamespace =
          record(
            "Outer",
            Some("com.example"),
            Schema.Field("inner", record("Inner", Some("org.other")))
          )

        assert {
          otherNamespace.toString ==
            """{"type":"record","name":"Outer","namespace":"com.example","fields":[{"name":"inner","type":{"type":"record","name":"Inner","namespace":"org.other","fields":[]}}]}"""
        }
      }

      it("should use an empty namespace for the null namespace inside another namespace") {
        val schema =
          record("Outer", Some("com.example"), Schema.Field("inner", record("Inner", None)))

        assert {
          schema.toString ==
            """{"type":"record","name":"Outer","namespace":"com.example","fields":[{"name":"inner","type":{"type":"record","name":"Inner","namespace":"","fields":[]}}]}"""
        }
      }

      it("should reference named schemas by name after their definition") {
        val fixed = Schema.Fixed(Schema.Name("Hash", Some("com.example")), 4)
        val schema =
          record(
            "Hashes",
            Some("com.example"),
            Schema.Field("first", fixed),
            Schema.Field("second", fixed)
          )

        assert {
          schema.toString ==
            """{"type":"record","name":"Hashes","namespace":"com.example","fields":[{"name":"first","type":{"type":"fixed","name":"Hash","size":4}},{"name":"second","type":"Hash"}]}"""
        }
      }

      it("should reference named schemas by full name in other namespaces") {
        val fixed = Schema.Fixed(Schema.Name("Hash", Some("org.other")), 4)
        val schema =
          record(
            "Hashes",
            Some("com.example"),
            Schema.Field("first", fixed),
            Schema.Field("second", fixed)
          )

        assert {
          schema.toString ==
            """{"type":"record","name":"Hashes","namespace":"com.example","fields":[{"name":"first","type":{"type":"fixed","name":"Hash","namespace":"org.other","size":4}},{"name":"second","type":"org.other.Hash"}]}"""
        }
      }

      it("should reference named schemas by full name when named after Avro types") {
        val fixed = Schema.Fixed(Schema.Name("int", Some("com.example")), 4)
        val schema =
          record(
            "Hashes",
            Some("com.example"),
            Schema.Field("first", fixed),
            Schema.Field("second", fixed)
          )

        assert {
          schema.toString ==
            """{"type":"record","name":"Hashes","namespace":"com.example","fields":[{"name":"first","type":{"type":"fixed","name":"int","size":4}},{"name":"second","type":"com.example.int"}]}"""
        }
      }

      it("should print references by name") {
        val name = Schema.Name("Node", Some("com.example"))
        val schema =
          Schema.Record(
            name,
            Vector(
              Schema.Field("value", Schema.Int),
              Schema.Field("next", Schema.Union(Vector(Schema.Null, Schema.Ref(name))))
            )
          )

        assert {
          schema.toString ==
            """{"type":"record","name":"Node","namespace":"com.example","fields":[{"name":"value","type":"int"},{"name":"next","type":["null","Node"]}]}"""
        }
      }

      it("should qualify aliases relative to the namespace and remove duplicates") {
        val schema =
          record("Person", Some("com.example")).withAliases(
            Vector("Human", "com.example.Human", "org.other.Human", "com.example.Being")
          )

        assert {
          schema.toString ==
            """{"type":"record","name":"Person","namespace":"com.example","fields":[],"aliases":["Human","org.other.Human","Being"]}"""
        }
      }

      it("should not print reserved properties") {
        val props = JsonObject("type" -> Json.fromString("long"), "custom" -> Json.True)
        assert(Schema.Int.withProps(props).toString == """{"type":"int","custom":true}""")

        val field =
          Schema.Field("value", Schema.Int).withProps(JsonObject("default" -> Json.fromInt(1)))

        assert {
          record("Test", None, field).toString ==
            """{"type":"record","name":"Test","fields":[{"name":"value","type":"int"}]}"""
        }
      }

      it("should escape strings in the same way as the Java Avro library") {
        val backslash = "\\"
        val quote = "\""
        val unescaped = List(0x7f.toChar, 0x80.toChar, '/').mkString

        val doc =
          List(quote, backslash, "\b", "\t", "\n", "\f", "\r").mkString +
            List(0x01.toChar, 0x1f.toChar).mkString + unescaped

        val escaped =
          List(
            backslash + quote,
            backslash + backslash,
            backslash + "b",
            backslash + "t",
            backslash + "n",
            backslash + "f",
            backslash + "r",
            backslash + "u0001",
            backslash + "u001F"
          ).mkString + unescaped

        val schema = Schema.Enum(Schema.Name("Escaped"), Vector("A")).withDoc(Some(doc))

        assert {
          schema.toString == s"""{"type":"enum","name":"Escaped","doc":"$escaped","symbols":["A"]}"""
        }
      }
    }
  }
}
