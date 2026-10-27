/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import org.apache.avro.{Schema => JavaSchema}
import org.apache.avro.generic.{GenericData, GenericDatumReader, GenericDatumWriter}
import org.apache.avro.io.{DecoderFactory, Encoder, EncoderFactory}
import scodec.bits.ByteVector

/** Values and encodings of the Java Avro library, for checking against the library. */
object JavaAvro {
  def schema(schema: Schema): JavaSchema =
    new JavaSchema.Parser().parse(schema.toString)

  /** Converts the value to the generic representation of the Java Avro library. */
  def toJava(value: AvroValue, schema: Schema, javaSchema: JavaSchema): Any =
    toJava(value, schema, javaSchema, AvroValueGenerators.namedSchemas(schema))

  def writeBinary(javaSchema: JavaSchema, datum: Any): ByteVector =
    ByteVector.view(write(javaSchema, datum, EncoderFactory.get().binaryEncoder(_, null)))

  def readBinary(javaSchema: JavaSchema, bytes: ByteVector): Any =
    new GenericDatumReader[Any](javaSchema)
      .read(null, DecoderFactory.get().binaryDecoder(bytes.toArray, null))

  def writeJson(javaSchema: JavaSchema, datum: Any): String = {
    val bytes = write(javaSchema, datum, EncoderFactory.get().jsonEncoder(javaSchema, _))
    new String(bytes, StandardCharsets.UTF_8)
  }

  def readJson(javaSchema: JavaSchema, json: String): Any =
    new GenericDatumReader[Any](javaSchema)
      .read(null, DecoderFactory.get().jsonDecoder(javaSchema, json))

  /** Sorts map entries by key, since the Java Avro library reads maps into a `HashMap`. */
  def sortMaps(value: AvroValue): AvroValue =
    value match {
      case AvroValue.Record(values) => AvroValue.Record(values.map(sortMaps))
      case AvroValue.Array(items)   => AvroValue.Array(items.map(sortMaps))
      case AvroValue.Map(entries)   =>
        AvroValue.Map(entries.map { case (key, value) => (key, sortMaps(value)) }.sortBy(_._1))
      case AvroValue.Union(index, value) => AvroValue.Union(index, sortMaps(value))
      case _                             => value
    }

  private def write(
    javaSchema: JavaSchema,
    datum: Any,
    encoder: ByteArrayOutputStream => Encoder
  ): Array[Byte] = {
    val output = new ByteArrayOutputStream()
    val javaEncoder = encoder(output)
    new GenericDatumWriter[Any](javaSchema).write(datum, javaEncoder)
    javaEncoder.flush()
    output.toByteArray
  }

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
        throw new IllegalArgumentException(s"Value $value does not match schema $schema")
    }
}
