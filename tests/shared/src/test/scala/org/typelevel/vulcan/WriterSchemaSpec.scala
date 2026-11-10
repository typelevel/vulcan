/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan

import cats.syntax.all.*
import java.time.Instant
import weaver.Expectations
import weaver.SourceLocation

/** Tests for [[Codec#binaryEncoder]] with writer schemas other than the schema of the codec. */
object WriterSchemaSpec extends BaseSuite {
  final case class Person(name: String)

  final case class PersonWithAge(name: String, age: Int)

  sealed trait Fruit

  case object Apple extends Fruit

  case object Banana extends Fruit

  case object Cherry extends Fruit

  /** Record with the same name as [[Outer]] in another namespace, since codecs cannot be recursive. */
  final case class Inner(value: Int)

  final case class Outer(value: Int, next: Option[Inner])

  implicit val personCodec: Codec[Person] =
    Codec.record(name = "Person", namespace = "com.example") { field =>
      field("name", _.name).map(Person(_))
    }

  implicit val personWithAgeCodec: Codec[PersonWithAge] =
    Codec.record(name = "Person", namespace = "com.example") { field =>
      (field("name", _.name), field("age", _.age)).mapN(PersonWithAge(_, _))
    }

  def fruitCodec(symbols: List[String], default: Option[Fruit] = None): Codec[Fruit] =
    Codec.enumeration[Fruit](
      name = "Fruit",
      namespace = "com.example",
      symbols = symbols,
      encode = {
        case Apple => "Apple"
        case Banana => "Banana"
        case Cherry => "Cherry"
      },
      decode = {
        case "Apple" => Right(Apple)
        case "Banana" => Right(Banana)
        case "Cherry" => Right(Cherry)
        case other => Left(AvroException(s"Unknown fruit $other"))
      },
      default = default
    )

  implicit val innerCodec: Codec[Inner] =
    Codec.record(name = "Node", namespace = "com.example.inner") { field =>
      field("value", _.value).map(Inner(_))
    }

  implicit val outerCodec: Codec[Outer] =
    Codec.record(name = "Node", namespace = "com.example") { field =>
      (field("value", _.value), field("next", _.next)).mapN(Outer(_, _))
    }

  /** Recursive writer schema, which [[Outer]] and [[Inner]] can be written as. */
  val recursiveWriterSchema: String =
    """{"type":"record","name":"Node","namespace":"com.example","fields":[{"name":"value","type":"long"},{"name":"next","type":["null","Node"],"default":null},{"name":"label","type":"string","default":""}]}"""

  /**
    * Expects `a` to be written using the writer schema, and then decoded as `expected`
    * using the decoder of the codec for the expected value.
    */
  def expectWritten[A, B](a: A, writerSchemaJson: String, expected: B)(implicit
    codec: Codec[A],
    expectedCodec: Codec[B],
    loc: SourceLocation
  ): Expectations =
    whenSuccess(Schema.parse(writerSchemaJson)) { writerSchema =>
      val decoded =
        for {
          encoder <- codec.binaryEncoder(writerSchema)
          bytes <- encoder.encode(a)
          decoder <- expectedCodec.binaryDecoder(writerSchema)
          decoded <- decoder.decode(bytes)
        } yield decoded

      expect.same(Right(expected), decoded.left.map(_.message))
    }

  /** Expects that values of the codec cannot be written using the writer schema. */
  def expectIncompatible[A](writerSchemaJson: String, message: String)(implicit
    codec: Codec[A],
    loc: SourceLocation
  ): Expectations =
    whenSuccess(Schema.parse(writerSchemaJson)) { writerSchema =>
      expect.same(
        Left(s"Unable to write values using the writer schema: $message"),
        codec.binaryEncoder(writerSchema).void.left.map(_.message)
      )
    }

  pureTest("binaryEncoder with the schema of the codec writes the same as binaryEncoder") {
    val encoded =
      for {
        schema <- Codec[PersonWithAge].schema
        withSchema <- Codec[PersonWithAge].binaryEncoder(schema).flatMap(_.encode(PersonWithAge("a", 1)))
        withoutSchema <- Codec[PersonWithAge].binaryEncoder.flatMap(_.encode(PersonWithAge("a", 1)))
      } yield (withSchema, withoutSchema)

    whenSuccess(encoded) { case (withSchema, withoutSchema) =>
      expect.same(withoutSchema, withSchema)
    }
  }

  pureTest("fields missing in the schema use the defaults of the writer schema") {
    expectWritten(
      Person("a"),
      """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"name","type":"string"},{"name":"age","type":"int","default":42}]}""",
      PersonWithAge("a", 42)
    )
  }

  pureTest("fields missing in the writer schema are left out") {
    expectWritten(
      PersonWithAge("a", 1),
      """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"name","type":"string"}]}""",
      Person("a")
    )
  }

  pureTest("fields of the writer schema are written in the order of the writer schema") {
    expectWritten(
      PersonWithAge("a", 1),
      """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"age","type":"int"},{"name":"name","type":"string"}]}""",
      PersonWithAge("a", 1)
    )
  }

  pureTest("fields of the writer schema use fields with the name of an alias") {
    expectWritten(
      PersonWithAge("a", 1),
      """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"fullName","type":"string","aliases":["name"]},{"name":"age","type":"int"}]}""",
      Person("a")
    )(
      personWithAgeCodec,
      Codec.record(name = "Person", namespace = "com.example") { field =>
        field("fullName", _.name).map(Person(_))
      },
      implicitly
    )
  }

  pureTest("defaults of nested records, unions and bytes are decoded") {
    val writerSchema =
      """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"name","type":"string"},{"name":"extra","type":{"type":"record","name":"Extra","fields":[{"name":"tags","type":{"type":"array","items":"string"}},{"name":"note","type":["null","string"]},{"name":"data","type":"bytes"}]},"default":{"tags":["x"],"note":null,"data":"ÿ"}}]}"""

    whenSuccess(Schema.parse(writerSchema)) { schema =>
      val value =
        for {
          bytes <- Codec[Person].binaryEncoder(schema).flatMap(_.encode(Person("a")))
          value <- BinaryDecoder(schema).flatMap(_.decode(bytes))
        } yield value

      expect.same(
        Right(
          AvroValue.Record(
            Vector(
              AvroValue.String("a"),
              AvroValue.Record(
                Vector(
                  AvroValue.Array(Vector(AvroValue.String("x"))),
                  AvroValue.Union(0, AvroValue.Null),
                  AvroValue.Bytes(scodec.bits.ByteVector(0xff))
                )
              )
            )
          )
        ),
        value
      )
    }
  }

  pureTest("fields missing in the schema without defaults are incompatible") {
    expectIncompatible[Person](
      """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"name","type":"string"},{"name":"age","type":"int"}]}""",
      """Field "age" of record com.example.Person in writer schema is missing in the schema and has no default"""
    )
  }

  pureTest("records with other names are incompatible, unless the writer schema has an alias") {
    expectIncompatible[Person](
      """{"type":"record","name":"Human","namespace":"com.example","fields":[{"name":"name","type":"string"}]}""",
      "record com.example.Person cannot be written as record com.example.Human in writer schema"
    ) and expectWritten(
      Person("a"),
      """{"type":"record","name":"Human","namespace":"com.example","aliases":["Person"],"fields":[{"name":"name","type":"string"}]}""",
      Person("a")
    )(
      personCodec,
      Codec.record(name = "Human", namespace = "com.example") { field =>
        field("name", _.name).map(Person(_))
      },
      implicitly
    )
  }

  pureTest("values are promoted to the types of the writer schema") {
    expectWritten(1, "\"long\"", 1L) and
      expectWritten(1, "\"double\"", 1.0) and
      expectWritten(1L, "\"float\"", 1.0f) and
      expectWritten(1.5f, "\"double\"", 1.5) and
      expectWritten("a", "\"bytes\"", scodec.bits.ByteVector('a'.toByte)) and
      expectWritten(scodec.bits.ByteVector('a'.toByte), "\"string\"", "a")
  }

  pureTest("values which cannot be promoted are incompatible") {
    expectIncompatible[Long]("\"int\"", "long cannot be written as int in writer schema")
  }

  pureTest("values are written as union types of the writer schema") {
    expectWritten(1, """["null","int"]""", Option(1)) and
      expectWritten(Option(1), """["null","long"]""", Option(1L)) and
      expectWritten[Either[Int, String], Either[String, Int]](Left(1), """["string","int"]""", Right(1)) and
      expectWritten[Either[Int, String], Either[String, Int]](Right("a"), """["string","int"]""", Left("a"))
  }

  pureTest("unions are written as the type of the writer schema if all union types are compatible") {
    expectWritten[Either[Int, Long], Long](Left(1), "\"long\"", 1L) and
      expectIncompatible[Option[Int]]("\"int\"", "null cannot be written as int in writer schema") and
      expectIncompatible[Either[Int, String]]("""["null","int"]""", "Missing type for string in union")
  }

  pureTest("enum symbols missing in the writer schema use the default of the writer schema") {
    expectWritten[Fruit, Fruit](
      Cherry,
      """{"type":"enum","name":"Fruit","namespace":"com.example","symbols":["Apple","Banana"],"default":"Apple"}""",
      Apple
    )(fruitCodec(List("Apple", "Banana", "Cherry")), fruitCodec(List("Apple", "Banana")), implicitly) and
      expectIncompatible[Fruit](
        """{"type":"enum","name":"Fruit","namespace":"com.example","symbols":["Apple","Banana"]}""",
        "Symbol Cherry of enum com.example.Fruit is missing in writer schema, which has no default"
      )(fruitCodec(List("Apple", "Banana", "Cherry")), implicitly)
  }

  pureTest("logical types have to be the same") {
    expectIncompatible[Instant](
      "\"long\"",
      "long with logical type timestamp-millis has no logical type in writer schema"
    ) and expectIncompatible[Long](
      """{"type":"long","logicalType":"timestamp-millis"}""",
      "long has logical type timestamp-millis in writer schema"
    ) and expectWritten(
      Instant.ofEpochMilli(1L),
      """{"type":"long","logicalType":"timestamp-millis","owner":"team"}""",
      Instant.ofEpochMilli(1L)
    )
  }

  pureTest("fixed schemas with other sizes are incompatible") {
    val codec =
      Codec.fixed[scodec.bits.ByteVector](
        name = "Hash",
        namespace = "com.example",
        size = 2,
        encode = identity,
        decode = Right(_)
      )

    expectIncompatible(
      """{"type":"fixed","name":"Hash","namespace":"com.example","size":4}""",
      "Fixed com.example.Hash with size 2 has size 4 in writer schema"
    )(codec, implicitly)
  }

  pureTest("values are written using recursive writer schemas") {
    whenSuccess(Schema.parse(recursiveWriterSchema)) { schema =>
      val value =
        for {
          bytes <- Codec[Outer].binaryEncoder(schema).flatMap(_.encode(Outer(1, Some(Inner(2)))))
          value <- BinaryDecoder(schema).flatMap(_.decode(bytes))
        } yield value

      expect.same(
        Right(
          AvroValue.Record(
            Vector(
              AvroValue.Long(1L),
              AvroValue.Union(
                1,
                AvroValue.Record(
                  Vector(AvroValue.Long(2L), AvroValue.Union(0, AvroValue.Null), AvroValue.String(""))
                )
              ),
              AvroValue.String("")
            )
          )
        ),
        value
      )
    }
  }

  pureTest("invalid writer schemas are errors") {
    expect.same(
      Left("Invalid default \"a\" for field \"age\" with schema \"int\" in record com.example.Person"),
      Schema
        .parse(
          """{"type":"record","name":"Person","namespace":"com.example","fields":[{"name":"age","type":"int","default":"a"}]}"""
        )
        .flatMap(Codec[Person].binaryEncoder(_))
        .void
        .left
        .map(_.message)
    )
  }
}
