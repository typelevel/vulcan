/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json
import org.apache.avro.Schema as JavaSchema
import org.scalacheck.Gen
import scala.util.Random
import scala.util.Try

/** Checks schemas against the Java Avro library. */
final class SchemaJavaSpec extends BaseSpec {
  private val backslash: String = "\\"

  private val letter: String = 0xe9.toChar.toString

  /** Schemas accepted by the Java Avro library, by description. */
  private val Accepted: List[(String, String)] =
    List(
      "primitive type names" -> "\"int\"",
      "primitive objects" -> """{"type":"int"}""",
      "primitive props" ->
        """{"type":"string","avro.java.string":"String","custom":{"nested":[1,2]}}""",
      "logical types" -> """{"type":"long","logicalType":"timestamp-millis"}""",
      "decimals" -> """{"type":"bytes","logicalType":"decimal","precision":4,"scale":2}""",
      "invalid decimals" -> """{"type":"bytes","logicalType":"decimal","precision":-1}""",
      "invalid logical types" -> """{"type":"string","logicalType":"date"}""",
      "reserved attributes of primitives" ->
        """{"type":"int","name":"Int","namespace":"com.example","doc":"documentation","aliases":["Integer"],"default":1}""",
      "unions" -> """["null","int","string"]""",
      "arrays" -> """{"type":"array","items":"int","custom":true}""",
      "maps" -> """{"type":"map","values":{"type":"array","items":"long"}}""",
      "records" ->
        """{"type":"record","name":"Person","namespace":"com.example","doc":"A person","fields":[{"name":"name","type":"string","doc":"The name","default":"","order":"DESCENDING","aliases":["fullName","fullName"],"custom":1}],"custom":true,"aliases":["Human","other.Human",".Human"]}""",
      "field orders" ->
        """{"type":"record","name":"Person","fields":[{"name":"first","type":"int","order":"Ignore"},{"name":"second","type":"int","order":"ascending"}]}""",
      "non-text attributes" ->
        """{"type":"record","name":"Person","namespace":1,"doc":true,"fields":[{"name":"name","type":"string","doc":null}]}""",
      "enums" ->
        """{"type":"enum","name":"Suit","doc":"A suit","symbols":["Hearts","Spades"],"default":"Spades","aliases":["Color"],"custom":"value"}""",
      "enums with non-text defaults" ->
        """{"type":"enum","name":"Suit","symbols":["Hearts"],"default":1}""",
      "fixed" ->
        """{"type":"fixed","name":"Id","namespace":"com.example","size":16,"logicalType":"uuid","aliases":["Key"]}""",
      "named schemas named after primitives in namespaces" ->
        """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"first","type":{"type":"fixed","name":"int","size":1}},{"name":"second","type":"com.example.int"}]}""",
      "recursive records" ->
        """{"type":"record","name":"Node","fields":[{"name":"next","type":["null","Node"],"default":null}]}""",
      "recursive records in namespaces" ->
        """{"type":"record","name":"Node","namespace":"com.example","fields":[{"name":"next","type":["null","com.example.Node"]},{"name":"children","type":{"type":"array","items":"Node"}}]}""",
      "mutually recursive records" ->
        """{"type":"record","name":"Tree","fields":[{"name":"node","type":{"type":"record","name":"Node","fields":[{"name":"tree","type":["null","Tree"]}]}},{"name":"other","type":"Node"}]}""",
      "forward references" ->
        """{"type":"record","name":"Person","fields":[{"name":"first","type":"Name"},{"name":"last","type":{"type":"enum","name":"Name","symbols":["Unknown"]}}]}""",
      "forward references in namespaces" ->
        """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"first","type":["null","Name"]},{"name":"last","type":{"type":"fixed","name":"Name","size":2}}]}""",
      "forward references to named schemas within other named schemas" ->
        """{"type":"record","name":"Root","fields":[{"name":"c","type":"C"},{"name":"a","type":{"type":"record","name":"A","fields":[{"name":"c","type":{"type":"record","name":"C","fields":[{"name":"a","type":["null","A"]}]}}]}}]}""",
      "null namespaces within namespaces" ->
        """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"first","type":{"type":"enum","name":"Name","namespace":"","symbols":["Unknown"]}},{"name":"last","type":"Name"}]}""",
      "full names with namespaces" ->
        """{"type":"record","name":"com.example.Person","namespace":"other","fields":[{"name":"id","type":{"type":"fixed","name":"Id","size":1}}]}""",
      "names resolved in document order" ->
        """{"type":"record","name":"Person","fields":[{"name":"id","type":{"type":"fixed","name":"Id","size":1}},{"name":"user","type":{"type":"record","name":"User","namespace":"com.example","fields":[{"name":"first","type":"Id"},{"name":"second","type":{"type":"fixed","name":"Id","size":2}},{"name":"third","type":"Id"}]}}]}""",
      "equivalent definitions" ->
        """{"type":"record","name":"Person","fields":[{"name":"first","type":{"type":"enum","name":"Name","symbols":["Unknown"]}},{"name":"last","type":{"type":"enum","name":"Name","doc":"A name","aliases":["Other"],"symbols":["Unknown"]}}]}""",
      "equivalent definitions of records" ->
        """{"type":"record","name":"Person","fields":[{"name":"first","type":{"type":"record","name":"Name","fields":[{"name":"value","type":"string"}]}},{"name":"last","type":{"type":"record","name":"Name","doc":"A name","fields":[{"name":"value","type":"string","doc":"The value"}]}}]}""",
      "text defaults of floats and doubles" ->
        """{"type":"record","name":"Numbers","fields":[{"name":"a","type":"double","default":"1.5"},{"name":"b","type":"float","default":"NaN"},{"name":"c","type":{"type":"double","custom":1},"default":"-Infinity"},{"name":"d","type":"double","default":"1e3"}]}""",
      "invalid defaults" ->
        """{"type":"record","name":"Defaults","fields":[{"name":"a","type":"string","default":null},{"name":"b","type":"int","default":1.5},{"name":"c","type":["null","int"],"default":"text"}]}""",
      "numbers" -> """{"type":"int","custom":[12345678901234567890,1.5,-1,0,1.0E20]}""",
      "escaped characters" ->
        s"""{"type":"record","name":"Person","doc":"quote \\" backslash \\\\ tab \\t control ${backslash}u0001 letter $letter","fields":[]}""",
      "letters" ->
        s"""{"type":"record","name":"${letter}Person","fields":[{"name":"$letter","type":"int"}]}""",
      "whitespace" -> """ { "type" : "record" , "name" : "Person" , "fields" : [ ] } """,
      "duplicate keys" -> """{"type":"int","type":"long"}"""
    )

  /** Schemas rejected by the Java Avro library, by description. */
  private val Rejected: List[(String, String)] =
    List(
      "empty input" -> "",
      "numbers" -> "1",
      "booleans" -> "true",
      "null" -> "null",
      "objects without type" -> "{}",
      "non-text types" -> """{"type":1}""",
      "union types" -> """{"type":["int"]}""",
      "object types" -> """{"type":{"type":"int"}}""",
      "unknown types" -> """{"type":"union"}""",
      "undefined names" -> "\"Person\"",
      "undefined names in fields" ->
        """{"type":"record","name":"Person","fields":[{"name":"name","type":"Name"}]}""",
      "names with leading dots" ->
        """{"type":"record","name":"Person","fields":[{"name":"first","type":".Name"},{"name":"last","type":{"type":"fixed","name":"Name","size":1}}]}""",
      "records without fields" -> """{"type":"record","name":"Person"}""",
      "records without name" -> """{"type":"record","fields":[]}""",
      "records with non-array fields" -> """{"type":"record","name":"Person","fields":{}}""",
      "non-object fields" -> """{"type":"record","name":"Person","fields":["name"]}""",
      "fields without name" -> """{"type":"record","name":"Person","fields":[{"type":"int"}]}""",
      "fields without type" -> """{"type":"record","name":"Person","fields":[{"name":"name"}]}""",
      "duplicate fields" ->
        """{"type":"record","name":"Person","fields":[{"name":"name","type":"int"},{"name":"name","type":"long"}]}""",
      "invalid field names" ->
        """{"type":"record","name":"Person","fields":[{"name":"first-name","type":"int"}]}""",
      "invalid orders" ->
        """{"type":"record","name":"Person","fields":[{"name":"name","type":"int","order":"up"}]}""",
      "non-text orders" ->
        """{"type":"record","name":"Person","fields":[{"name":"name","type":"int","order":1}]}""",
      "invalid names" -> """{"type":"record","name":"1Person","fields":[]}""",
      "empty names" -> """{"type":"record","name":"com.example.","fields":[]}""",
      "invalid namespaces" ->
        """{"type":"record","name":"Person","namespace":"com.1example","fields":[]}""",
      "empty namespace parts" ->
        """{"type":"record","name":"Person","namespace":"com..example","fields":[]}""",
      "non-array aliases" -> """{"type":"record","name":"Person","aliases":"Human","fields":[]}""",
      "non-text aliases" -> """{"type":"record","name":"Person","aliases":[1],"fields":[]}""",
      "invalid aliases" -> """{"type":"record","name":"Person","aliases":["1Human"],"fields":[]}""",
      "non-array field aliases" ->
        """{"type":"record","name":"Person","fields":[{"name":"name","type":"int","aliases":"other"}]}""",
      "named schemas named after primitives" -> """{"type":"fixed","name":"int","size":1}""",
      "records named after primitives" -> """{"type":"record","name":"string","fields":[]}""",
      "enums without symbols" -> """{"type":"enum","name":"Suit"}""",
      "enums with non-array symbols" -> """{"type":"enum","name":"Suit","symbols":"Hearts"}""",
      "enums with non-text symbols" -> """{"type":"enum","name":"Suit","symbols":[1]}""",
      "enums with duplicate symbols" ->
        """{"type":"enum","name":"Suit","symbols":["Hearts","Hearts"]}""",
      "enums with invalid symbols" -> """{"type":"enum","name":"Suit","symbols":["1Hearts"]}""",
      "enums with defaults which are not symbols" ->
        """{"type":"enum","name":"Suit","symbols":["Hearts"],"default":"Spades"}""",
      "fixed without size" -> """{"type":"fixed","name":"Id"}""",
      "fixed with text size" -> """{"type":"fixed","name":"Id","size":"16"}""",
      "fixed with decimal size" -> """{"type":"fixed","name":"Id","size":16.0}""",
      "fixed with negative size" -> """{"type":"fixed","name":"Id","size":-1}""",
      "fixed with long size" -> """{"type":"fixed","name":"Id","size":2147483648}""",
      "arrays without items" -> """{"type":"array"}""",
      "maps without values" -> """{"type":"map"}""",
      "nested unions" -> """[["int"]]""",
      "duplicate types in unions" -> """["int","int"]""",
      "duplicate primitives in unions" -> """["int",{"type":"int","logicalType":"date"}]""",
      "duplicate arrays in unions" ->
        """[{"type":"array","items":"int"},{"type":"array","items":"long"}]""",
      "duplicate named schemas in unions" ->
        """[{"type":"enum","name":"Suit","symbols":["Hearts"]},"Suit"]""",
      "conflicting definitions" ->
        """{"type":"record","name":"Person","fields":[{"name":"first","type":{"type":"enum","name":"Name","symbols":["Unknown"]}},{"name":"last","type":{"type":"enum","name":"Name","symbols":["Other"]}}]}""",
      "definitions within themselves" ->
        """{"type":"record","name":"Person","fields":[{"name":"person","type":{"type":"record","name":"Person","fields":[]}}]}""",
      "dangling content" -> "\"int\" \"long\"",
      "invalid JSON" -> """{"type":"int""""
    )

  /** Schemas rejected by the Java Avro library which are accepted, with the printed schema. */
  private val Lenient: List[(String, String, String)] =
    List(
      (
        "references using objects, same as earlier versions of the Java Avro library",
        """{"type":"record","name":"Person","fields":[{"name":"id","type":{"type":"fixed","name":"Id","size":1}},{"name":"other","type":{"type":"Id"}}]}""",
        """{"type":"record","name":"Person","fields":[{"name":"id","type":{"type":"fixed","name":"Id","size":1}},{"name":"other","type":"Id"}]}"""
      ),
      (
        "recursive references to the null namespace, same as earlier versions of the Java Avro library",
        """{"type":"record","name":"Node","fields":[{"name":"edge","type":{"type":"record","name":"Edge","namespace":"com.example","fields":[{"name":"node","type":["null","Node"]}]}}]}""",
        """{"type":"record","name":"Node","fields":[{"name":"edge","type":{"type":"record","name":"Edge","namespace":"com.example","fields":[{"name":"node","type":["null","Node"]}]}}]}"""
      ),
      (
        "forward references to the null namespace",
        """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"first","type":"Name"},{"name":"last","type":{"type":"fixed","name":"Name","namespace":"","size":1}}]}""",
        """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"first","type":{"type":"fixed","name":"Name","namespace":"","size":1}},{"name":"last","type":"Name"}]}"""
      ),
      (
        "equivalent definitions of recursive records",
        """{"type":"record","name":"Tree","fields":[{"name":"left","type":{"type":"record","name":"Node","fields":[{"name":"next","type":["null","Node"]}]}},{"name":"right","type":{"type":"record","name":"Node","fields":[{"name":"next","type":["null","Node"]}]}}]}""",
        """{"type":"record","name":"Tree","fields":[{"name":"left","type":{"type":"record","name":"Node","fields":[{"name":"next","type":["null","Node"]}]}},{"name":"right","type":"Node"}]}"""
      ),
      (
        "text defaults of doubles which are not numbers",
        """{"type":"record","name":"Numbers","fields":[{"name":"a","type":"double","default":"one"}]}""",
        """{"type":"record","name":"Numbers","fields":[{"name":"a","type":"double","default":"one"}]}"""
      ),
      (
        "fixed larger than supported by the Java Avro library",
        """{"type":"fixed","name":"Id","size":2147483647}""",
        """{"type":"fixed","name":"Id","size":2147483647}"""
      )
    )

  /** Schemas accepted by the Java Avro library which are rejected, by description. */
  private val Strict: List[(String, String)] =
    List(
      "error types, which are only used in protocols" ->
        """{"type":"error","name":"Error","fields":[]}""",
      "comments" -> """/* comment */ "int"""",
      "definitions with different enum defaults" ->
        """{"type":"record","name":"Person","fields":[{"name":"first","type":{"type":"enum","name":"Name","symbols":["Unknown","Other"],"default":"Unknown"}},{"name":"last","type":{"type":"enum","name":"Name","symbols":["Unknown","Other"],"default":"Other"}}]}"""
    )

  /** Schemas with logical types, which are ignored if invalid for the type. */
  private val LogicalTypes: List[String] =
    List(
      """{"type":"bytes","logicalType":"decimal","precision":4,"scale":2}""",
      """{"type":"bytes","logicalType":"decimal","precision":4}""",
      """{"type":"bytes","logicalType":"decimal","precision":4.0,"scale":2}""",
      """{"type":"bytes","logicalType":"decimal","precision":4e0}""",
      """{"type":"bytes","logicalType":"decimal","precision":4,"scale":2.0}""",
      """{"type":"bytes","logicalType":"decimal","precision":4,"scale":null}""",
      """{"type":"bytes","logicalType":"decimal","precision":"4"}""",
      """{"type":"bytes","logicalType":"decimal","precision":2147483648}""",
      """{"type":"bytes","logicalType":"decimal","precision":2,"scale":3}""",
      """{"type":"fixed","name":"Decimal","size":2,"logicalType":"decimal","precision":4}""",
      """{"type":"fixed","name":"Decimal","size":2,"logicalType":"decimal","precision":5}"""
    )

  /** Parses the schema without validating defaults, since defaults are not validated. */
  private def javaParse(json: String): Try[String] =
    Try(new JavaSchema.Parser().setValidateDefaults(false).parse(json).toString)

  /** Shuffles the keys of all objects, so attributes and props occur in a different order. */
  private def shuffleKeys(json: Json, random: Random): Json =
    json.arrayOrObject(
      json,
      values => Json.fromValues(values.map(shuffleKeys(_, random))),
      obj =>
        Json.fromFields(random.shuffle(obj.toList).map { case (key, value) =>
          key -> shuffleKeys(value, random)
        })
    )

  describe("Schema") {
    describe("logicalType") {
      LogicalTypes.foreach { json =>
        it(s"should be the same as the Java Avro library for $json") {
          val expected = Option(new JavaSchema.Parser().parse(json).getLogicalType).map(_.getName)
          assert(Schema.parse(json).value.logicalType.map(_.name) == expected)
        }
      }
    }

    describe("toString") {
      it("should print the same as the Java Avro library") {
        forAll(SchemaGenerators.genSchema, minSuccessful(1000)) { schema =>
          val printed = schema.toString
          val parsed = new JavaSchema.Parser().parse(printed)
          assert(parsed.toString == printed)
        }
      }
    }

    describe("parse") {
      it("should parse the same as the Java Avro library") {
        val seeds = Gen.choose(Long.MinValue, Long.MaxValue)

        forAll(SchemaGenerators.genSchema, seeds, minSuccessful(1000)) { (schema, seed) =>
          val json = shuffleKeys(schema.toJson, new Random(seed)).spaces2
          val expected = new JavaSchema.Parser().parse(json).toString
          assert(Schema.parse(json).map(_.toString) == Right(expected))
        }
      }

      Accepted.foreach { case (description, json) =>
        it(s"should accept $description, same as the Java Avro library") {
          val expected = javaParse(json)
          assert(expected.isSuccess)
          assert(Schema.parse(json).map(_.toString) == Right(expected.get))
        }
      }

      Rejected.foreach { case (description, json) =>
        it(s"should reject $description, same as the Java Avro library") {
          assert(javaParse(json).isFailure)
          assert(Schema.parse(json).isLeft)
        }
      }

      Lenient.foreach { case (description, json, printed) =>
        it(s"should accept $description") {
          assert(javaParse(json).isFailure)
          assert(Schema.parse(json).map(_.toString) == Right(printed))
        }
      }

      Strict.foreach { case (description, json) =>
        it(s"should reject $description") {
          assert(javaParse(json).isSuccess)
          assert(Schema.parse(json).isLeft)
        }
      }
    }
  }
}
