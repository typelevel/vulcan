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

  /** Encodes with an encoder compiled once for the schema. */
  @Benchmark
  def binaryEncoder(): Either[AvroException, ByteVector] =
    encoder.encode(Order.example)

  /** Decodes with a decoder compiled once for the writer schema. */
  @Benchmark
  def binaryDecoder(): Either[AvroException, Order] =
    decoder.decode(bytes)
}
