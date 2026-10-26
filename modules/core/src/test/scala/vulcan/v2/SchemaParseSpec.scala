/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import io.circe.{Json, JsonObject}
import vulcan.BaseSpec

final class SchemaParseSpec extends BaseSpec {
  private def parse(json: String): Schema =
    Schema.parse(json).value

  private def parseError(json: String): String =
    Schema.parse(json) match {
      case Left(error)   => error.message
      case Right(schema) => fail(s"Expected an error, but parsed $schema")
    }

  private def record(name: Schema.Name, fields: Schema.Field*): Schema.Record =
    Schema.Record(name, fields.toVector)

  private def fixed(name: Schema.Name): Schema.Fixed =
    Schema.Fixed(name, 1)

  describe("Schema") {
    describe("parse") {
      it("should parse printed schemas") {
        forAll(SchemaGenerators.genSchema, minSuccessful(1000)) { schema =>
          assert(Schema.parse(schema.toString).map(_.toString) == Right(schema.toString))
        }
      }

      it("should parse primitives as type names and objects") {
        assert(parse("\"int\"") eq Schema.Int)
        assert(parse("""{"type":"string"}""") eq Schema.String)
      }

      it("should keep props of primitives, including logical types") {
        assert {
          parse("""{"type":"string","avro.java.string":"String"}""") ==
            Schema.String.withProps(JsonObject("avro.java.string" -> Json.fromString("String")))
        }

        assert {
          parse("""{"type":"bytes","logicalType":"decimal","precision":10,"scale":2}""") ==
            Schema.Bytes.withLogicalType(LogicalType.decimal(10, 2))
        }
      }

      it("should ignore reserved attributes of primitives") {
        assert(parse("""{"type":"int","name":"Int","doc":"documentation"}""") eq Schema.Int)
      }

      it("should parse records") {
        val json =
          """{"type":"record","name":"Person","namespace":"com.example","doc":"A person","fields":[{"name":"name","type":"string","doc":"The name","default":"","order":"DESCENDING","aliases":["fullName","fullName"],"custom":true}],"aliases":["Human"],"custom":1}"""

        val field =
          Schema
            .Field("name", Schema.String)
            .withDoc(Some("The name"))
            .withDefault(Some(Json.fromString("")))
            .withOrder(Schema.Field.Order.Descending)
            .withAliases(Vector("fullName"))
            .withProps(JsonObject("custom" -> Json.True))

        assert {
          parse(json) ==
            record(Schema.Name("Person", Some("com.example")), field)
              .withDoc(Some("A person"))
              .withAliases(Vector("Human"))
              .withProps(JsonObject("custom" -> Json.fromInt(1)))
        }
      }

      it("should parse enums") {
        val json =
          """{"type":"enum","name":"Suit","doc":"A suit","symbols":["Hearts","Spades"],"default":"Spades","aliases":["Color"],"custom":true}"""

        assert {
          parse(json) ==
            Schema
              .Enum(Schema.Name("Suit"), Vector("Hearts", "Spades"))
              .withDefault(Some("Spades"))
              .withDoc(Some("A suit"))
              .withAliases(Vector("Color"))
              .withProps(JsonObject("custom" -> Json.True))
        }
      }

      it("should parse fixed") {
        val json =
          """{"type":"fixed","name":"Id","doc":"An id","size":16,"logicalType":"uuid","aliases":["Key"]}"""

        assert {
          parse(json) ==
            Schema
              .Fixed(Schema.Name("Id"), 16)
              .withDoc(Some("An id"))
              .withAliases(Vector("Key"))
              .withLogicalType(LogicalType.uuid)
        }
      }

      it("should parse arrays, maps, and unions") {
        assert {
          parse("""{"type":"array","items":"int","custom":true}""") ==
            Schema.Array(Schema.Int).withProps(JsonObject("custom" -> Json.True))
        }

        assert(parse("""{"type":"map","values":"long"}""") == Schema.Map(Schema.Long))
        assert(parse("""["null","string"]""") == Schema.Union(Vector(Schema.Null, Schema.String)))
      }

      it("should ignore attributes which are not text if they should be text") {
        val json =
          """{"type":"enum","name":"Suit","namespace":1,"doc":true,"symbols":["Hearts"],"default":null}"""

        assert(parse(json) == Schema.Enum(Schema.Name("Suit"), Vector("Hearts")))
      }

      it("should inherit the namespace of the enclosing named schema") {
        val json =
          """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"id","type":{"type":"fixed","name":"Id","size":1}}]}"""

        assert {
          parse(json) ==
            record(
              Schema.Name("Person", Some("com.example")),
              Schema.Field("id", fixed(Schema.Name("Id", Some("com.example"))))
            )
        }
      }

      it("should use the namespace of full names") {
        val json = """{"type":"fixed","name":"com.example.Id","namespace":"other","size":1}"""
        assert(parse(json) == fixed(Schema.Name("Id", Some("com.example"))))
      }

      it("should use the null namespace for empty namespaces") {
        val json =
          """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"id","type":{"type":"fixed","name":"Id","namespace":"","size":1}}]}"""

        assert {
          parse(json) ==
            record(
              Schema.Name("Person", Some("com.example")),
              Schema.Field("id", fixed(Schema.Name("Id")))
            )
        }
      }

      it("should reference enclosing named schemas") {
        val json =
          """{"type":"record","name":"Node","namespace":"com.example","fields":[{"name":"next","type":["null","Node"]}]}"""

        val name = Schema.Name("Node", Some("com.example"))

        assert {
          parse(json) ==
            record(name, Schema.Field("next", Schema.Union(Vector(Schema.Null, Schema.Ref(name)))))
        }
      }

      it("should define named schemas where they first occur") {
        val json =
          """{"type":"record","name":"Person","fields":[{"name":"first","type":"Name"},{"name":"last","type":{"type":"fixed","name":"Name","size":1}}]}"""

        val name = fixed(Schema.Name("Name"))
        val schema = parse(json)

        assert {
          schema == record(
            Schema.Name("Person"),
            Schema.Field("first", name),
            Schema.Field("last", name)
          )
        }

        assert {
          schema.toString ==
            """{"type":"record","name":"Person","fields":[{"name":"first","type":{"type":"fixed","name":"Name","size":1}},{"name":"last","type":"Name"}]}"""
        }
      }

      it("should fall back to the null namespace for named schemas defined before the reference") {
        val json =
          """{"type":"record","name":"Person","fields":[{"name":"id","type":{"type":"fixed","name":"Id","size":1}},{"name":"user","type":{"type":"record","name":"User","namespace":"com.example","fields":[{"name":"first","type":"Id"},{"name":"second","type":{"type":"fixed","name":"Id","size":2}},{"name":"third","type":"Id"}]}}]}"""

        val id = fixed(Schema.Name("Id"))
        val namespacedId = Schema.Fixed(Schema.Name("Id", Some("com.example")), 2)

        assert {
          parse(json) ==
            record(
              Schema.Name("Person"),
              Schema.Field("id", id),
              Schema.Field(
                "user",
                record(
                  Schema.Name("User", Some("com.example")),
                  Schema.Field("first", id),
                  Schema.Field("second", namespacedId),
                  Schema.Field("third", namespacedId)
                )
              )
            )
        }
      }

      it("should accept equivalent definitions of named schemas") {
        val json =
          """{"type":"record","name":"Person","fields":[{"name":"first","type":{"type":"fixed","name":"Name","size":1}},{"name":"last","type":{"type":"fixed","name":"Name","doc":"A name","aliases":["Other"],"size":1}}]}"""

        val name = fixed(Schema.Name("Name"))

        assert {
          parse(json) ==
            record(Schema.Name("Person"), Schema.Field("first", name), Schema.Field("last", name))
        }
      }

      it("should reject conflicting definitions of named schemas") {
        val json =
          """["null",{"type":"fixed","name":"Id","size":1},{"type":"array","items":{"type":"fixed","name":"Id","size":2}}]"""

        assert(parseError(json) == "Conflicting definitions of named schema Id")
      }

      it("should convert text defaults of floats and doubles to numbers") {
        val json =
          """{"type":"record","name":"Numbers","fields":[{"name":"float","type":"float","default":"1.5"},{"name":"double","type":"double","default":"-Infinity"},{"name":"string","type":"string","default":"1.5"}]}"""

        val defaults =
          parse(json) match {
            case record: Schema.Record => record.fields.map(_.default)
            case schema                => fail(s"Expected a record, but parsed $schema")
          }

        assert {
          defaults == Vector(
            Json.fromDouble(1.5),
            Some(Json.fromString("-Infinity")),
            Some(Json.fromString("1.5"))
          )
        }
      }

      it("should not validate field defaults") {
        val json =
          """{"type":"record","name":"Person","fields":[{"name":"name","type":"string","default":null}]}"""

        assert {
          parse(json) ==
            record(
              Schema.Name("Person"),
              Schema.Field("name", Schema.String).withDefault(Some(Json.Null))
            )
        }
      }

      it("should accept references to named schemas as types") {
        val json =
          """{"type":"record","name":"Person","fields":[{"name":"id","type":{"type":"fixed","name":"Id","size":1}},{"name":"other","type":{"type":"Id"}}]}"""

        val id = fixed(Schema.Name("Id"))

        assert {
          parse(json) ==
            record(Schema.Name("Person"), Schema.Field("id", id), Schema.Field("other", id))
        }
      }

      it("should reject invalid JSON") {
        assert(parseError("""{"type":"int"""").startsWith("Invalid JSON: "))
      }

      it("should reject invalid schemas") {
        List(
          "1" -> "Invalid schema 1",
          "{}" -> "Missing type in schema {}",
          "\"Person\"" -> "Undefined name Person",
          """{"type":"error","name":"Error","fields":[]}""" ->
            """Unsupported type error in schema {"type":"error","name":"Error","fields":[]}""",
          """{"type":"record","name":"Person"}""" ->
            """Missing fields in record {"type":"record","name":"Person"}""",
          """{"type":"record","fields":[]}""" ->
            """Missing name in schema {"type":"record","fields":[]}""",
          """{"type":"record","name":"Person","fields":[{"name":"name"}]}""" ->
            """Missing type in field {"name":"name"}""",
          """{"type":"record","name":"Person","fields":[{"name":"name","type":"string","order":"up"}]}""" ->
            """Invalid order in field {"name":"name","type":"string","order":"up"}""",
          """{"type":"record","name":"Person","aliases":"Human","fields":[]}""" ->
            """Invalid aliases in {"type":"record","name":"Person","aliases":"Human","fields":[]}""",
          """{"type":"record","name":"Person","fields":[{"name":"name","type":"string"},{"name":"name","type":"string"}]}""" ->
            """Duplicate field "name" in record Person""",
          """{"type":"record","name":"1Person","fields":[]}""" ->
            """Invalid name "1Person": must start with a letter or underscore""",
          """{"type":"record","name":"Person","namespace":"com..example","fields":[]}""" ->
            """Invalid namespace part "" of com..example.Person: cannot be empty""",
          """{"type":"enum","name":"Suit","symbols":["Hearts"],"default":"Spades"}""" ->
            """Default "Spades" is not a symbol of enum Suit""",
          """{"type":"enum","name":"Suit","symbols":[1]}""" ->
            """Invalid symbol 1 in enum {"type":"enum","name":"Suit","symbols":[1]}""",
          """{"type":"fixed","name":"Id","size":1.0}""" ->
            """Missing or invalid size in fixed {"type":"fixed","name":"Id","size":1.0}""",
          """{"type":"fixed","name":"Id","size":-1}""" -> "Invalid size -1 for fixed Id",
          """{"type":"fixed","name":"int","size":1}""" ->
            "Named schemas cannot be named after primitive type int",
          """{"type":"array"}""" -> """Missing items in array {"type":"array"}""",
          """{"type":"map"}""" -> """Missing values in map {"type":"map"}""",
          """[["int"]]""" -> """Unions cannot directly contain other unions: [["int"]]""",
          """["int",{"type":"int","logicalType":"date"}]""" ->
            """Duplicate int in union ["int",{"type":"int","logicalType":"date"}]"""
        ).foreach { case (json, message) =>
          assert(parseError(json) == message)
        }
      }
    }

    describe("fromJson") {
      it("should decode the same as parse") {
        forAll(SchemaGenerators.genSchema) { schema =>
          assert(Schema.fromJson(schema.toJson).map(_.toString) == Right(schema.toString))
        }
      }
    }
  }
}
