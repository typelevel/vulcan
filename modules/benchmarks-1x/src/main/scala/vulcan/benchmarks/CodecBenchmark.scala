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

  @Benchmark
  def toBinary(): Either[AvroError, Array[Byte]] =
    Codec.toBinary(Order.example)

  @Benchmark
  def fromBinary(): Either[AvroError, Order] =
    Codec.fromBinary[Order](bytes, schema)

  /** Encodes reusing the datum writer, like Kafka serializers do. */
  @Benchmark
  def datumWriter(): Either[AvroError, Array[Byte]] =
    Order.codec.encode(Order.example).map { value =>
      val output = new ByteArrayOutputStream()
      val encoder = EncoderFactory.get().binaryEncoder(output, null)
      writer.write(value, encoder)
      encoder.flush()
      output.toByteArray
    }

  /** Decodes reusing the datum reader, like Kafka deserializers do. */
  @Benchmark
  def datumReader(): Either[AvroError, Order] =
    Order.codec.decode(reader.read(null, DecoderFactory.get().binaryDecoder(bytes, null)), schema)
}
