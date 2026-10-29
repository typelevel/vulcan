/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.syntax.all.*
import io.circe.Json
import java.time.Instant
import org.apache.avro.generic.GenericData
import org.apache.avro.generic.GenericRecord
import scala.collection.mutable
import scodec.bits.ByteVector
import vulcan.Fs2KafkaSpec.*

/**
  * Tests for the uses of fs2-kafka-vulcan. Avro binary is framed with a magic byte and the schema id,
  * like Confluent serializers do, and writer schemas are looked up as JSON from a schema registry.
  */
final class Fs2KafkaSpec extends BaseSpec {
  describe("serializer") {
    it("should register the schema of the codec") {
      val registry = new Registry
      val _ = serialize[Either[Int, Boolean]](registry, Right(true)).value
      assert(registry.schema(1).map(_.noSpaces) == Some("""["int","boolean"]"""))
    }

    it("should error for invalid schemas") {
      val codec = Codec.decimal(-1, -1)
      assert {
        serialize(new Registry, BigDecimal(1))(codec).swap.value.message ==
          "Invalid decimal precision -1: must be positive"
      }
    }

    it("should write values the Java Avro library reads") {
      val bytes = ByteVector.view(serialize(new Registry, Test2("test", 42)).value)
      val writerSchema = JavaAvro.schema(Codec[Test2].schema.value)
      val readerSchema = JavaAvro.schema(Codec[Test].schema.value)
      val record = JavaAvro.readBinary(writerSchema, readerSchema, bytes.drop(5)).asInstanceOf[GenericRecord]
      assert(record.get("name").toString == "test")
    }
  }

  describe("deserializer") {
    it("should roundtrip values") {
      val registry = new Registry
      val deserialize = deserializer[Either[Test, Int]](registry).value

      List(Left(Test("test")), Right(1)).foreach { value =>
        assert(deserialize(serialize[Either[Test, Int]](registry, value).value) == Right(value))
      }
    }

    it("should decode using compatible schemas") {
      val registry = new Registry
      val bytes = serialize(registry, Test2("test", 42)).value
      assert(deserializer[Test](registry).value(bytes) == Right(Test("test")))
    }

    it("should error for mismatching logical types") {
      val registry = new Registry
      val bytes = serialize(registry, 42L).value
      assert {
        deserializer[Instant](registry).value(bytes).swap.value.message ==
          "Error decoding Instant: Got unexpected missing logical type"
      }
    }

    it("should error for invalid schemas") {
      val codec = Codec.decimal(-1, -1)
      assert {
        deserializer(new Registry)(codec).swap.value.message ==
          "Invalid decimal precision -1: must be positive"
      }
    }

    it("should error for values missing after the schema id") {
      val registry = new Registry
      val id = registry.register(Codec[Int].schema.value)
      assert {
        deserializer[Int](registry).value(frame(id, ByteVector.empty)).swap.value.message ==
          "Unexpected end of input at position 0"
      }
    }

    it("should decode values the Java Avro library writes") {
      val registry = new Registry
      val schema = Codec[Test2].schema.value
      val javaSchema = JavaAvro.schema(schema)
      val value = Codec[Test2].encoder.value.encode(Test2("test", 42)).value
      val datum = JavaAvro.toJava(value, schema, javaSchema)
      val bytes = frame(registry.register(schema), JavaAvro.writeBinary(javaSchema, datum))
      assert(deserializer[Test](registry).value(bytes) == Right(Test("test")))
    }

    it("should decode values with a decoder compiled once for each writer schema") {
      val registry = new Registry
      val deserialize = deserializer[Test](registry).value
      val bytes = List("a", "b", "c").map(name => serialize(registry, Test2(name, 1)).value)

      assert(bytes.traverse(deserialize) == Right(List(Test("a"), Test("b"), Test("c"))))
      assert(registry.lookups == 1)
    }
  }

  describe("schemas") {
    it("should be the same after printing and parsing") {
      val schemas = List(
        Codec[Test].schema,
        Codec[Test2].schema,
        Codec[Either[Test, Int]].schema,
        Codec[Instant].schema,
        Codec.decimal(10, 2).schema
      )

      schemas.foreach { schema =>
        assert(Schema.fromJson(schema.value.toJson) == Right(schema.value))
        assert(JavaAvro.schema(schema.value).toString == schema.value.toString)
      }
    }

    it("should decode using schemas with features from other libraries") {
      // Includes properties written by the Java Avro library for generated classes
      val json =
        """{"type":"record","name":"Test","namespace":"fs2.kafka.vulcan","fields":[{"name":"name","type":{"type":"string","avro.java.string":"String"}},{"name":"id","type":{"type":"string","logicalType":"custom-id"},"default":"none"},{"name":"next","type":["null","Test"],"default":null}],"aliases":["Other"]}"""

      val writerSchema = Schema.parse(json).value
      assert(writerSchema.toString == json)

      val javaSchema = JavaAvro.schema(writerSchema)
      assert(javaSchema.toString == json)

      val record = new GenericData.Record(javaSchema)
      record.put("name", "test")
      record.put("id", "1")
      record.put("next", null)

      val bytes = JavaAvro.writeBinary(javaSchema, record)
      assert(Codec[Test].binaryDecoder(writerSchema).flatMap(_.decode(bytes)) == Right(Test("test")))
    }
  }
}

object Fs2KafkaSpec {
  final case class Test(name: String)

  object Test {
    implicit val codec: Codec[Test] =
      Codec.record(
        name = "Test",
        namespace = "fs2.kafka.vulcan"
      ) { field =>
        field("name", _.name).map(Test(_))
      }
  }

  final case class Test2(name: String, number: Int)

  object Test2 {
    implicit val codec: Codec[Test2] =
      Codec.record(
        name = "Test",
        namespace = "fs2.kafka.vulcan"
      ) { field =>
        (
          field("name", _.name),
          field("number", _.number)
        ).mapN(Test2(_, _))
      }
  }

  /** A schema registry with schemas as JSON by id. */
  final class Registry {
    private val schemas: mutable.ArrayBuffer[Json] = mutable.ArrayBuffer.empty

    /** The number of times schemas were looked up by id. */
    var lookups: Int = 0

    def register(schema: Schema): Int = {
      val json = schema.toJson
      val index = schemas.indexOf(json)
      if (index >= 0) index + 1
      else {
        schemas += json
        schemas.size
      }
    }

    def schema(id: Int): Option[Json] = {
      lookups += 1
      schemas.lift(id - 1)
    }
  }

  /** Avro binary framed with the magic byte and the schema id, as Confluent serializers write it. */
  def frame(id: Int, bytes: ByteVector): Array[Byte] =
    (ByteVector(0) ++ ByteVector.fromInt(id) ++ bytes).toArray

  def serialize[A](registry: Registry, a: A)(implicit codec: Codec[A]): Either[AvroError, Array[Byte]] =
    for {
      schema <- codec.schema
      encoder <- codec.binaryEncoder
      bytes <- encoder.encode(a)
    } yield frame(registry.register(schema), bytes)

  /**
    * Returns a deserializer, if the reader schema is valid, which looks up the writer schema and
    * compiles a decoder once for each schema id, and decodes the bytes after the schema id.
    */
  def deserializer[A](
    registry: Registry
  )(implicit codec: Codec[A]): Either[AvroError, Array[Byte] => Either[AvroError, A]] =
    codec.schema.map { _ =>
      val decoders = mutable.Map.empty[Int, Either[AvroError, BinaryDecoder[A]]]

      def decoder(id: Int): Either[AvroError, BinaryDecoder[A]] =
        decoders.getOrElseUpdate(
          id,
          registry
            .schema(id)
            .toRight(AvroError(s"Unknown schema id $id"))
            .flatMap(Schema.fromJson)
            .flatMap(codec.binaryDecoder)
        )

      bytes => {
        val input = ByteVector.view(bytes)
        if (input.size < 5 || input.head != 0) Left(AvroError("Missing magic byte and schema id"))
        else decoder(input.slice(1, 5).toInt()).flatMap(_.decode(input.drop(5)))
      }
    }
}
