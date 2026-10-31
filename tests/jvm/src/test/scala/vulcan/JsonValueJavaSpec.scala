/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import org.scalacheck.Gen
import vulcan.internal.JsonPrinter
import weaver.scalacheck.CheckConfig

/** Checks the JSON encoding of values against the Java Avro library. */
object JsonValueJavaSpec extends BaseSuite {
  override def checkConfig: CheckConfig =
    super.checkConfig.withMinimumSuccessful(1000)

  private val genSchemaAndValue: Gen[(Schema, AvroValue)] =
    AvroValueGenerators.genSchemaAndValue.map { case (schema, value) =>
      (Schema.Array(schema), AvroValue.Array(Vector(value)))
    }

  test("JsonEncoder should encode the same as the Java Avro library") {
    forall(genSchemaAndValue) { case (schema, value) =>
      val javaSchema = JavaAvro.schema(schema)
      val javaJson = JavaAvro.writeJson(javaSchema, JavaAvro.toJava(value, schema, javaSchema))
      whenSuccess(io.circe.jawn.parse(javaJson)) { json =>
        expect.same(Right(json), JsonEncoder(schema).flatMap(_.encode(value)))
      }
    }
  }

  test("JsonEncoder should encode values which the Java Avro library decodes") {
    forall(genSchemaAndValue) { case (schema, value) =>
      val javaSchema = JavaAvro.schema(schema)
      whenSuccess(JsonEncoder(schema).flatMap(_.encode(value))) { json =>
        val javaBytes =
          JavaAvro.writeBinary(javaSchema, JavaAvro.readJson(javaSchema, JsonPrinter.print(json)))
        expect.same(
          Right(JavaAvro.sortMaps(value)),
          BinaryDecoder(schema).flatMap(_.decode(javaBytes)).map(JavaAvro.sortMaps)
        )
      }
    }
  }

  test("JsonDecoder should decode values encoded by the Java Avro library") {
    forall(genSchemaAndValue) { case (schema, value) =>
      val javaSchema = JavaAvro.schema(schema)
      val javaJson = JavaAvro.writeJson(javaSchema, JavaAvro.toJava(value, schema, javaSchema))
      whenSuccess(io.circe.jawn.parse(javaJson)) { json =>
        expect.same(Right(value), JsonDecoder(schema).flatMap(_.decode(json)))
      }
    }
  }
}
