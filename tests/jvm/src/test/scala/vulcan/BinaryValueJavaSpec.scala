/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import vulcan.internal.BinaryValueDecoder
import vulcan.internal.BinaryValueEncoder

/** Checks the binary encoding of values against the Java Avro library. */
final class BinaryValueJavaSpec extends BaseSpec {
  describe("BinaryValueEncoder") {
    it("should encode the same as the Java Avro library") {
      forAll(AvroValueGenerators.genSchemaAndValue, minSuccessful(1000)) { case (schema, value) =>
        val javaSchema = JavaAvro.schema(schema)
        val javaBytes = JavaAvro.writeBinary(javaSchema, JavaAvro.toJava(value, schema, javaSchema))
        assert(BinaryValueEncoder(schema).value.encode(value) == Right(javaBytes))
      }
    }

    it("should encode values which the Java Avro library decodes") {
      forAll(AvroValueGenerators.genSchemaAndValue, minSuccessful(1000)) { case (schema, value) =>
        val javaSchema = JavaAvro.schema(schema)
        val bytes = BinaryValueEncoder(schema).value.encode(value).value
        val javaBytes = JavaAvro.writeBinary(javaSchema, JavaAvro.readBinary(javaSchema, bytes))
        assert {
          BinaryValueDecoder(schema).value.decode(javaBytes).map(JavaAvro.sortMaps) ==
            Right(JavaAvro.sortMaps(value))
        }
      }
    }
  }

  describe("BinaryValueDecoder") {
    it("should decode values encoded by the Java Avro library") {
      forAll(AvroValueGenerators.genSchemaAndValue, minSuccessful(1000)) { case (schema, value) =>
        val javaSchema = JavaAvro.schema(schema)
        val javaBytes = JavaAvro.writeBinary(javaSchema, JavaAvro.toJava(value, schema, javaSchema))
        assert(BinaryValueDecoder(schema).value.decode(javaBytes) == Right(value))
      }
    }
  }
}
