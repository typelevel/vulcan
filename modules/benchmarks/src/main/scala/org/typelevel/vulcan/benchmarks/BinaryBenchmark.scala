/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.benchmarks

import java.io.ByteArrayOutputStream
import java.util.concurrent.TimeUnit
import org.apache.avro.Schema as JavaSchema
import org.apache.avro.generic.GenericDatumReader
import org.apache.avro.generic.GenericDatumWriter
import org.apache.avro.generic.GenericRecord
import org.apache.avro.io.BinaryDecoder as JavaBinaryDecoder
import org.apache.avro.io.BinaryEncoder as JavaBinaryEncoder
import org.apache.avro.io.DecoderFactory
import org.apache.avro.io.EncoderFactory
import org.openjdk.jmh.annotations.*
import org.typelevel.vulcan.AvroException
import org.typelevel.vulcan.AvroValue
import org.typelevel.vulcan.BinaryDecoder
import org.typelevel.vulcan.BinaryEncoder
import scodec.bits.ByteVector

/**
  * Compares the binary encoding of values with the generic records of the Java Avro library.
  * Both sides reuse their encoders and decoders.
  *
  * Run using `sbt "benchmarks/Jmh/run -prof gc"` to include allocations.
  */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class BinaryBenchmark {
  private val value: AvroValue =
    Orders.value

  private val encoder: BinaryEncoder[AvroValue] =
    BinaryEncoder(Orders.schema).fold(error => throw error, identity)

  private val decoder: BinaryDecoder[AvroValue] =
    BinaryDecoder(Orders.schema).fold(error => throw error, identity)

  private val bytes: ByteVector =
    encoder.encode(value).fold(error => throw error, identity)

  private val javaSchema: JavaSchema =
    new JavaSchema.Parser().parse(Orders.schema.toString)

  private val javaWriter: GenericDatumWriter[GenericRecord] =
    new GenericDatumWriter[GenericRecord](javaSchema)

  private val javaReader: GenericDatumReader[GenericRecord] =
    new GenericDatumReader[GenericRecord](javaSchema)

  private val javaBytes: Array[Byte] =
    bytes.toArray

  private val javaRecord: GenericRecord =
    javaReader.read(null, DecoderFactory.get().binaryDecoder(javaBytes, null))

  private val javaOutput: ByteArrayOutputStream =
    new ByteArrayOutputStream(1024)

  private var javaEncoder: JavaBinaryEncoder =
    null

  private var javaDecoder: JavaBinaryDecoder =
    null

  @Benchmark
  def encode(): Either[AvroException, ByteVector] =
    encoder.encode(value)

  @Benchmark
  def encodeJava(): Array[Byte] = {
    javaOutput.reset()
    javaEncoder = EncoderFactory.get().binaryEncoder(javaOutput, javaEncoder)
    javaWriter.write(javaRecord, javaEncoder)
    javaEncoder.flush()
    javaOutput.toByteArray
  }

  @Benchmark
  def decode(): Either[AvroException, AvroValue] =
    decoder.decode(bytes)

  @Benchmark
  def decodeJava(): GenericRecord = {
    javaDecoder = DecoderFactory.get().binaryDecoder(javaBytes, javaDecoder)
    javaReader.read(null, javaDecoder)
  }
}
