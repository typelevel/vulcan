/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

/** Checks the binary encoding of values against the Java Avro library. */
object BinaryValueJavaSpec extends BaseSuite {
  test("BinaryEncoder should encode the same as the Java Avro library") {
    forall.withConfig(checkConfig.withMinimumSuccessful(1000))(AvroValueGenerators.genSchemaAndValue) {
      case (schema, value) =>
        val javaSchema = JavaAvro.schema(schema)
        val javaBytes = JavaAvro.writeBinary(javaSchema, JavaAvro.toJava(value, schema, javaSchema))
        expect.same(Right(javaBytes), BinaryEncoder(schema).value.encode(value))
    }
  }

  test("BinaryEncoder should encode values which the Java Avro library decodes") {
    forall.withConfig(checkConfig.withMinimumSuccessful(1000))(AvroValueGenerators.genSchemaAndValue) {
      case (schema, value) =>
        val javaSchema = JavaAvro.schema(schema)
        val bytes = BinaryEncoder(schema).value.encode(value).value
        val javaBytes = JavaAvro.writeBinary(javaSchema, JavaAvro.readBinary(javaSchema, bytes))
        expect.same(
          Right(JavaAvro.sortMaps(value)),
          BinaryDecoder(schema).value.decode(javaBytes).map(JavaAvro.sortMaps)
        )
    }
  }

  test("BinaryDecoder should decode values encoded by the Java Avro library") {
    forall.withConfig(checkConfig.withMinimumSuccessful(1000))(AvroValueGenerators.genSchemaAndValue) {
      case (schema, value) =>
        val javaSchema = JavaAvro.schema(schema)
        val javaBytes = JavaAvro.writeBinary(javaSchema, JavaAvro.toJava(value, schema, javaSchema))
        expect.same(Right(value), BinaryDecoder(schema).value.decode(javaBytes))
    }
  }
}
