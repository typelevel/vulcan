/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.benchmarks

import java.io.ByteArrayOutputStream
import java.util.concurrent.TimeUnit
import org.apache.avro.Schema
import org.apache.avro.generic.GenericDatumReader
import org.apache.avro.generic.GenericDatumWriter
import org.apache.avro.io.DecoderFactory
import org.apache.avro.io.EncoderFactory
import org.openjdk.jmh.annotations.*
import vulcan.AvroError
import vulcan.Codec

/**
  * Encodes and decodes an order with its codec using version 1.x,
  * for comparing with the same benchmark for the current version
  * in the `benchmarks` module.
  *
  * Also encodes and decodes with derived codecs, and decodes an order
  * written using a writer schema with an additional field.
  *
  * Run using `sbt "benchmarks1x/Jmh/run -prof gc CodecBenchmark"`.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class CodecBenchmark {
  private val schema: Schema =
    Order.codec.schema.fold(error => throw error.throwable, identity)

  private val bytes: Array[Byte] =
    Codec.toBinary(Order.example).fold(error => throw error.throwable, identity)

  private val writer: GenericDatumWriter[Any] =
    new GenericDatumWriter[Any](schema)

  private val reader: GenericDatumReader[Any] =
    new GenericDatumReader[Any](schema)

  private val derivedSchema: Schema =
    DerivedOrder.codec.schema.fold(error => throw error.throwable, identity)

  private val derivedBytes: Array[Byte] =
    Codec.toBinary(Order.example)(DerivedOrder.codec).fold(error => throw error.throwable, identity)

  private val derivedWriter: GenericDatumWriter[Any] =
    new GenericDatumWriter[Any](derivedSchema)

  private val derivedReader: GenericDatumReader[Any] =
    new GenericDatumReader[Any](derivedSchema)

  private val writerSchema: Schema =
    WriterOrder.codec.schema.fold(error => throw error.throwable, identity)

  private val writerBytes: Array[Byte] =
    Codec.toBinary(WriterOrder.example).fold(error => throw error.throwable, identity)

  private val evolvedReader: GenericDatumReader[Any] =
    new GenericDatumReader[Any](writerSchema, schema)

  /** Checks the decoders decode the example, so the benchmarks do not measure errors. */
  @Setup
  def check(): Unit =
    List(fromBinary(), datumReader(), derivedDatumReader(), evolvedDatumReader())
      .foreach(decoded => require(decoded == Right(Order.example), decoded))

  @Benchmark
  def toBinary(): Either[AvroError, Array[Byte]] =
    Codec.toBinary(Order.example)

  @Benchmark
  def fromBinary(): Either[AvroError, Order] =
    Codec.fromBinary[Order](bytes, schema)

  /** Encodes reusing the datum writer, like Kafka serializers do. */
  @Benchmark
  def datumWriter(): Either[AvroError, Array[Byte]] =
    Order.codec.encode(Order.example).map(write(writer, _))

  /** Decodes reusing the datum reader, like Kafka deserializers do. */
  @Benchmark
  def datumReader(): Either[AvroError, Order] =
    Order.codec.decode(read(reader, bytes), schema)

  /** Encodes with the derived codec, reusing the datum writer. */
  @Benchmark
  def derivedDatumWriter(): Either[AvroError, Array[Byte]] =
    DerivedOrder.codec.encode(Order.example).map(write(derivedWriter, _))

  /** Decodes with the derived codec, reusing the datum reader. */
  @Benchmark
  def derivedDatumReader(): Either[AvroError, Order] =
    DerivedOrder.codec.decode(read(derivedReader, derivedBytes), derivedSchema)

  /** Decodes reusing a datum reader for a writer schema with an additional field. */
  @Benchmark
  def evolvedDatumReader(): Either[AvroError, Order] =
    Order.codec.decode(read(evolvedReader, writerBytes), schema)

  private def write(writer: GenericDatumWriter[Any], value: Any): Array[Byte] = {
    val output = new ByteArrayOutputStream()
    val encoder = EncoderFactory.get().binaryEncoder(output, null)
    writer.write(value, encoder)
    encoder.flush()
    output.toByteArray
  }

  private def read(reader: GenericDatumReader[Any], bytes: Array[Byte]): Any =
    reader.read(null, DecoderFactory.get().binaryDecoder(bytes, null))
}
