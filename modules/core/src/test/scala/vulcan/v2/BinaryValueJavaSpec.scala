/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import org.apache.avro.{Schema => JavaSchema}
import org.apache.avro.generic.{GenericData, GenericDatumReader, GenericDatumWriter}
import org.apache.avro.io.{DecoderFactory, EncoderFactory}
import scodec.bits.ByteVector
import vulcan.BaseSpec
import vulcan.v2.internal.{BinaryDecoder, BinaryEncoder}

/** Checks the binary encoding of values against the Java Avro library. */
final class BinaryValueJavaSpec extends BaseSpec {
  private def javaWrite(javaSchema: JavaSchema, datum: Any): ByteVector = {
    val output = new ByteArrayOutputStream()
    val encoder = EncoderFactory.get().binaryEncoder(output, null)
    new GenericDatumWriter[Any](javaSchema).write(datum, encoder)
    encoder.flush()
    ByteVector.view(output.toByteArray)
  }

  private def javaRead(javaSchema: JavaSchema, bytes: ByteVector): Any =
    new GenericDatumReader[Any](javaSchema)
      .read(null, DecoderFactory.get().binaryDecoder(bytes.toArray, null))

  /** Sorts map entries by key, since the Java Avro library reads maps into a `HashMap`. */
  private def sortMaps(value: AvroValue): AvroValue =
    value match {
      case AvroValue.Record(values) => AvroValue.Record(values.map(sortMaps))
      case AvroValue.Array(items)   => AvroValue.Array(items.map(sortMaps))
      case AvroValue.Map(entries)   =>
        AvroValue.Map(entries.map { case (key, value) => (key, sortMaps(value)) }.sortBy(_._1))
      case AvroValue.Union(index, value) => AvroValue.Union(index, sortMaps(value))
      case _                             => value
    }

  /** Converts the value to the generic representation of the Java Avro library. */
  private def toJava(
    value: AvroValue,
    schema: Schema,
    javaSchema: JavaSchema,
    definitions: collection.Map[String, Schema.Named]
  ): Any =
    (value, schema) match {
      case (_, ref: Schema.Ref) =>
        toJava(value, definitions(ref.fullName), javaSchema, definitions)
      case (AvroValue.Null, _)             => null
      case (AvroValue.Boolean(boolean), _) => boolean
      case (AvroValue.Int(int), _)         => int
      case (AvroValue.Long(long), _)       => long
      case (AvroValue.Float(float), _)     => float
      case (AvroValue.Double(double), _)   => double
      case (AvroValue.Bytes(bytes), _)     => ByteBuffer.wrap(bytes.toArray)
      case (AvroValue.String(string), _)   => string
      case (AvroValue.Enum(symbol), _)     => new GenericData.EnumSymbol(javaSchema, symbol)
      case (AvroValue.Fixed(bytes), _)     => new GenericData.Fixed(javaSchema, bytes.toArray)
      case (AvroValue.Record(values), record: Schema.Record) =>
        val javaRecord = new GenericData.Record(javaSchema)
        values.zip(record.fields).zipWithIndex.foreach { case ((fieldValue, field), index) =>
          javaRecord.put(
            index,
            toJava(fieldValue, field.schema, javaSchema.getFields.get(index).schema, definitions)
          )
        }
        javaRecord
      case (AvroValue.Array(items), array: Schema.Array) =>
        val javaArray = new java.util.ArrayList[Any]()
        items.foreach { item =>
          javaArray.add(toJava(item, array.items, javaSchema.getElementType, definitions))
        }
        javaArray
      case (AvroValue.Map(entries), map: Schema.Map) =>
        val javaMap = new java.util.LinkedHashMap[String, Any]()
        entries.foreach { case (key, entryValue) =>
          javaMap.put(key, toJava(entryValue, map.values, javaSchema.getValueType, definitions))
        }
        javaMap
      case (AvroValue.Union(index, branchValue), union: Schema.Union) =>
        toJava(branchValue, union.types(index), javaSchema.getTypes.get(index), definitions)
      case _ =>
        fail(s"Value $value does not match schema $schema")
    }

  /** The bytes written by the Java Avro library for the value. */
  private def javaEncode(schema: Schema, value: AvroValue): (JavaSchema, ByteVector) = {
    val javaSchema = new JavaSchema.Parser().parse(schema.toString)
    val datum = toJava(value, schema, javaSchema, AvroValueGenerators.namedSchemas(schema))
    (javaSchema, javaWrite(javaSchema, datum))
  }

  describe("BinaryEncoder") {
    it("should encode the same as the Java Avro library") {
      forAll(AvroValueGenerators.genSchemaAndValue, minSuccessful(1000)) { case (schema, value) =>
        val (_, javaBytes) = javaEncode(schema, value)
        assert(BinaryEncoder(schema).value.encode(value) == Right(javaBytes))
      }
    }

    it("should encode values which the Java Avro library decodes") {
      forAll(AvroValueGenerators.genSchemaAndValue, minSuccessful(1000)) { case (schema, value) =>
        val javaSchema = new JavaSchema.Parser().parse(schema.toString)
        val bytes = BinaryEncoder(schema).value.encode(value).value
        val javaBytes = javaWrite(javaSchema, javaRead(javaSchema, bytes))
        assert(
          BinaryDecoder(schema).value.decode(javaBytes).map(sortMaps) == Right(sortMaps(value))
        )
      }
    }
  }

  describe("BinaryDecoder") {
    it("should decode values encoded by the Java Avro library") {
      forAll(AvroValueGenerators.genSchemaAndValue, minSuccessful(1000)) { case (schema, value) =>
        val (_, javaBytes) = javaEncode(schema, value)
        assert(BinaryDecoder(schema).value.decode(javaBytes) == Right(value))
      }
    }
  }
}
