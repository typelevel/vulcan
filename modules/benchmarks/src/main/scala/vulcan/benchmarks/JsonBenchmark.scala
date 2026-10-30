/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.benchmarks

import io.circe.Json
import io.circe.Printer
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.TimeUnit
import org.apache.avro.Schema as JavaSchema
import org.apache.avro.generic.GenericDatumReader
import org.apache.avro.generic.GenericDatumWriter
import org.apache.avro.generic.GenericRecord
import org.apache.avro.io.DecoderFactory
import org.apache.avro.io.EncoderFactory
import org.openjdk.jmh.annotations.*
import vulcan.AvroException
import vulcan.AvroValue
import vulcan.JsonDecoder
import vulcan.JsonEncoder

/**
  * Encodes and decodes values in the Avro JSON encoding, for the same values as [[BinaryBenchmark]],
  * and compares with the generic records of the Java Avro library.
  *
  * The `encode` and `decode` benchmarks convert between values and circe `Json`, while the other
  * benchmarks also print and parse the JSON as UTF-8 bytes, same as the Java Avro library.
  *
  * Run using `sbt "benchmarks/Jmh/run -prof gc JsonBenchmark"`.
  */
@State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
@Fork(1)
class JsonBenchmark {
  private val value: AvroValue =
    Orders.value

  private val encoder: JsonEncoder[AvroValue] =
    JsonEncoder(Orders.schema).fold(error => throw error, identity)

  private val decoder: JsonDecoder[AvroValue] =
    JsonDecoder(Orders.schema).fold(error => throw error, identity)

  private val json: Json =
    encoder.encode(value).fold(error => throw error, identity)

  private val bytes: Array[Byte] =
    Printer.noSpaces.print(json).getBytes(UTF_8)

  private val javaSchema: JavaSchema =
    new JavaSchema.Parser().parse(Orders.schema.toString)

  private val javaWriter: GenericDatumWriter[GenericRecord] =
    new GenericDatumWriter[GenericRecord](javaSchema)

  private val javaReader: GenericDatumReader[GenericRecord] =
    new GenericDatumReader[GenericRecord](javaSchema)

  private val javaRecord: GenericRecord =
    decodeJava()

  @Setup
  def check(): Unit = {
    require(decodeBytes() == Right(value))
    require(io.circe.jawn.parse(new String(encodeJava(), UTF_8)) == Right(json))
  }

  @Benchmark
  def encode(): Either[AvroException, Json] =
    encoder.encode(value)

  @Benchmark
  def encodeBytes(): Either[AvroException, Array[Byte]] =
    encoder.encode(value).map(json => Printer.noSpaces.print(json).getBytes(UTF_8))

  @Benchmark
  def encodeJava(): Array[Byte] = {
    val output = new ByteArrayOutputStream()
    val javaEncoder = EncoderFactory.get().jsonEncoder(javaSchema, output)
    javaWriter.write(javaRecord, javaEncoder)
    javaEncoder.flush()
    output.toByteArray
  }

  @Benchmark
  def decode(): Either[AvroException, AvroValue] =
    decoder.decode(json)

  @Benchmark
  def decodeBytes(): Either[Exception, AvroValue] =
    io.circe.jawn.parse(new String(bytes, UTF_8)).flatMap(decoder.decode)

  @Benchmark
  def decodeJava(): GenericRecord =
    javaReader.read(null, DecoderFactory.get().jsonDecoder(javaSchema, new ByteArrayInputStream(bytes)))
}
