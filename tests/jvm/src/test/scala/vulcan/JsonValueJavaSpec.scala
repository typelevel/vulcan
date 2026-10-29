/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import org.scalacheck.Gen
import vulcan.internal.BinaryValueDecoder
import vulcan.internal.JsonPrinter
import vulcan.internal.JsonValueDecoder
import vulcan.internal.JsonValueEncoder

/** Checks the JSON encoding of values against the Java Avro library. */
final class JsonValueJavaSpec extends BaseSpec {
  private val genSchemaAndValue: Gen[(Schema, AvroValue)] =
    AvroValueGenerators.genSchemaAndValue.map { case (schema, value) =>
      (Schema.Array(schema), AvroValue.Array(Vector(value)))
    }

  describe("JsonValueEncoder") {
    it("should encode the same as the Java Avro library") {
      forAll(genSchemaAndValue, minSuccessful(1000)) { case (schema, value) =>
        val javaSchema = JavaAvro.schema(schema)
        val javaJson = JavaAvro.writeJson(javaSchema, JavaAvro.toJava(value, schema, javaSchema))
        assert(
          JsonValueEncoder(schema).value.encode(value) == Right(io.circe.jawn.parse(javaJson).value)
        )
      }
    }

    it("should encode values which the Java Avro library decodes") {
      forAll(genSchemaAndValue, minSuccessful(1000)) { case (schema, value) =>
        val javaSchema = JavaAvro.schema(schema)
        val json = JsonPrinter.print(JsonValueEncoder(schema).value.encode(value).value)
        val javaBytes = JavaAvro.writeBinary(javaSchema, JavaAvro.readJson(javaSchema, json))
        assert {
          BinaryValueDecoder(schema).value.decode(javaBytes).map(JavaAvro.sortMaps) ==
            Right(JavaAvro.sortMaps(value))
        }
      }
    }
  }

  describe("JsonValueDecoder") {
    it("should decode values encoded by the Java Avro library") {
      forAll(genSchemaAndValue, minSuccessful(1000)) { case (schema, value) =>
        val javaSchema = JavaAvro.schema(schema)
        val javaJson = JavaAvro.writeJson(javaSchema, JavaAvro.toJava(value, schema, javaSchema))
        val json = io.circe.jawn.parse(javaJson).value
        assert(JsonValueDecoder(schema).value.decode(json) == Right(value))
      }
    }
  }
}
