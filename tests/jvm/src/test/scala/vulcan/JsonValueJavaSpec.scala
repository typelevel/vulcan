/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import org.scalacheck.Gen
import vulcan.internal.JsonPrinter

/** Checks the JSON encoding of values against the Java Avro library. */
object JsonValueJavaSpec extends BaseSuite {
  private val genSchemaAndValue: Gen[(Schema, AvroValue)] =
    AvroValueGenerators.genSchemaAndValue.map { case (schema, value) =>
      (Schema.Array(schema), AvroValue.Array(Vector(value)))
    }

  test("JsonEncoder should encode the same as the Java Avro library") {
    forall.withConfig(checkConfig.withMinimumSuccessful(1000))(genSchemaAndValue) { case (schema, value) =>
      val javaSchema = JavaAvro.schema(schema)
      val javaJson = JavaAvro.writeJson(javaSchema, JavaAvro.toJava(value, schema, javaSchema))
      expect.same(Right(io.circe.jawn.parse(javaJson).value), JsonEncoder(schema).value.encode(value))
    }
  }

  test("JsonEncoder should encode values which the Java Avro library decodes") {
    forall.withConfig(checkConfig.withMinimumSuccessful(1000))(genSchemaAndValue) { case (schema, value) =>
      val javaSchema = JavaAvro.schema(schema)
      val json = JsonPrinter.print(JsonEncoder(schema).value.encode(value).value)
      val javaBytes = JavaAvro.writeBinary(javaSchema, JavaAvro.readJson(javaSchema, json))
      expect.same(
        Right(JavaAvro.sortMaps(value)),
        BinaryDecoder(schema).value.decode(javaBytes).map(JavaAvro.sortMaps)
      )
    }
  }

  test("JsonDecoder should decode values encoded by the Java Avro library") {
    forall.withConfig(checkConfig.withMinimumSuccessful(1000))(genSchemaAndValue) { case (schema, value) =>
      val javaSchema = JavaAvro.schema(schema)
      val javaJson = JavaAvro.writeJson(javaSchema, JavaAvro.toJava(value, schema, javaSchema))
      val json = io.circe.jawn.parse(javaJson).value
      expect.same(Right(value), JsonDecoder(schema).value.decode(json))
    }
  }
}
