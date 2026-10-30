/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.benchmarks

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.*
import scodec.bits.ByteVector
import vulcan.AvroException
import vulcan.BinaryDecoder
import vulcan.BinaryEncoder
import vulcan.Schema

/**
  * Encodes and decodes an order with its codec, for comparing with
  * the same benchmark for version 1.x in the `benchmarks1x` module.
  *
  * Also encodes and decodes with derived codecs, and decodes an order
  * written using a writer schema with an additional field.
  *
  * Run using `sbt "benchmarks/Jmh/run -prof gc CodecBenchmark"`.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class CodecBenchmark {
  private val schema: Schema =
    Order.codec.schema.fold(error => throw error, identity)

  private val encoder: BinaryEncoder[Order] =
    Order.codec.binaryEncoder.fold(error => throw error, identity)

  private val decoder: BinaryDecoder[Order] =
    Order.codec.binaryDecoder(schema).fold(error => throw error, identity)

  private val bytes: ByteVector =
    encoder.encode(Order.example).fold(error => throw error, identity)

  private val derivedEncoder: BinaryEncoder[Order] =
    DerivedOrder.codec.binaryEncoder.fold(error => throw error, identity)

  private val derivedDecoder: BinaryDecoder[Order] =
    DerivedOrder.codec.schema
      .flatMap(DerivedOrder.codec.binaryDecoder)
      .fold(error => throw error, identity)

  private val derivedBytes: ByteVector =
    derivedEncoder.encode(Order.example).fold(error => throw error, identity)

  private val evolvedDecoder: BinaryDecoder[Order] =
    WriterOrder.codec.schema.flatMap(Order.codec.binaryDecoder).fold(error => throw error, identity)

  private val writerBytes: ByteVector =
    WriterOrder.codec.binaryEncoder
      .flatMap(_.encode(WriterOrder.example))
      .fold(error => throw error, identity)

  /** Checks the decoders decode the example, so the benchmarks do not measure errors. */
  @Setup
  def check(): Unit =
    List(decoder.decode(bytes), derivedDecoder.decode(derivedBytes), evolvedDecoder.decode(writerBytes))
      .foreach(decoded => require(decoded == Right(Order.example), decoded))

  /** Encodes with an encoder compiled once for the schema. */
  @Benchmark
  def binaryEncoder(): Either[AvroException, ByteVector] =
    encoder.encode(Order.example)

  /** Decodes with a decoder compiled once for the writer schema. */
  @Benchmark
  def binaryDecoder(): Either[AvroException, Order] =
    decoder.decode(bytes)

  /** Encodes with an encoder of the derived codec. */
  @Benchmark
  def derivedBinaryEncoder(): Either[AvroException, ByteVector] =
    derivedEncoder.encode(Order.example)

  /** Decodes with a decoder of the derived codec. */
  @Benchmark
  def derivedBinaryDecoder(): Either[AvroException, Order] =
    derivedDecoder.decode(derivedBytes)

  /** Decodes with a decoder for a writer schema with an additional field. */
  @Benchmark
  def evolvedBinaryDecoder(): Either[AvroException, Order] =
    evolvedDecoder.decode(writerBytes)
}
