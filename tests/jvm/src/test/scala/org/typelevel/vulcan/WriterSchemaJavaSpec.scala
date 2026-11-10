/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan

import weaver.Expectations
import weaver.SourceLocation
import org.typelevel.vulcan.WriterSchemaSpec.*

object WriterSchemaJavaSpec extends BaseSuite {
  def expectSameAsJava[A](a: A, writerSchemaJson: String)(implicit
    codec: Codec[A],
    loc: SourceLocation
  ): Expectations = {
    val written =
      for {
        schema <- codec.schema
        writerSchema <- Schema.parse(writerSchemaJson)
        value <- codec.encode(a)
        bytes <- codec.binaryEncoder(writerSchema).flatMap(_.encode(a))
      } yield {
        val javaSchema = JavaAvro.schema(schema)
        val javaWriterSchema = JavaAvro.schema(writerSchema)
        val javaBytes = JavaAvro.writeBinary(javaSchema, JavaAvro.toJava(value, schema, javaSchema))
        val resolved = JavaAvro.readBinary(javaSchema, javaWriterSchema, javaBytes)
        (bytes, JavaAvro.writeBinary(javaWriterSchema, resolved))
      }

    whenSuccess(written) { case (bytes, javaBytes) =>
      expect.same(javaBytes, bytes)
    }
  }

  pureTest("writes records like the Java Avro library resolves them") {
    expectSameAsJava(
      Person("a"),
      """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"name","type":"string"},{"name":"age","type":"int","default":42}]}"""
    ) and expectSameAsJava(
      PersonWithAge("a", 1),
      """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"name","type":"string"}]}"""
    ) and expectSameAsJava(
      PersonWithAge("a", 1),
      """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"age","type":"long"},{"name":"name","type":"string"}]}"""
    ) and expectSameAsJava(
      Person("a"),
      """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"name","type":"string"},{"name":"extra","type":{"type":"record","name":"Extra","fields":[{"name":"tags","type":{"type":"array","items":"string"}},{"name":"note","type":["null","string"]},{"name":"data","type":"bytes"}]},"default":{"tags":["x"],"note":null,"data":"ÿ"}}]}"""
    ) and expectSameAsJava(Outer(1, Some(Inner(2))), recursiveWriterSchema)
  }

  pureTest("writes promotions and unions like the Java Avro library resolves them") {
    expectSameAsJava(1, "\"long\"") and
      expectSameAsJava(1L, "\"double\"") and
      expectSameAsJava("a", "\"bytes\"") and
      expectSameAsJava(1, """["null","int"]""") and
      expectSameAsJava(Option(1), """["null","long"]""") and
      expectSameAsJava[Either[Int, String]](Right("a"), """["string","int"]""") and
      expectSameAsJava[Either[Int, Long]](Left(1), "\"long\"")
  }

  pureTest("writes enums like the Java Avro library resolves them") {
    expectSameAsJava[Fruit](
      Cherry,
      """{"type":"enum","name":"Fruit","namespace":"com.example","symbols":["Banana","Apple"],"default":"Apple"}"""
    )(fruitCodec(List("Apple", "Banana", "Cherry")), implicitly)
  }
}
