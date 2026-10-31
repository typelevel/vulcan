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

/**
  * Tests for the uses of fs2-kafka-vulcan. Avro binary is framed with a magic byte and the schema id,
  * like Confluent serializers do, and writer schemas are looked up as JSON from a schema registry.
  */
object Fs2KafkaSpec extends BaseSuite {
  pureTest("serializer should register the schema of the codec") {
    val registry = new Registry
    whenSuccess(serialize[Either[Int, Boolean]](registry, Right(true))) { _ =>
      expect.eql(Some("""["int","boolean"]"""), registry.schema(1).map(_.noSpaces))
    }
  }

  pureTest("serializer should error for invalid schemas") {
    val codec = Codec.decimal(-1, -1)
    expect.same(
      Left("Invalid decimal precision -1: must be positive"),
      serialize(new Registry, BigDecimal(1))(codec).left.map(_.message)
    )
  }

  pureTest("serializer should write values the Java Avro library reads") {
    val inputs = (serialize(new Registry, Test2("test", 42)), Codec[Test2].schema, Codec[Test].schema)
    whenSuccess(inputs.tupled) { case (bytes, writerSchema, readerSchema) =>
      val datum =
        JavaAvro.readBinary(
          JavaAvro.schema(writerSchema),
          JavaAvro.schema(readerSchema),
          ByteVector.view(bytes).drop(5)
        )

      matches(datum) { case record: GenericRecord =>
        expect.eql("test", record.get("name").toString)
      }
    }
  }

  pureTest("deserializer should roundtrip values") {
    val registry = new Registry
    whenSuccess(deserializer[Either[Test, Int]](registry)) { deserialize =>
      forEach(List[Either[Test, Int]](Left(Test("test")), Right(1))) { value =>
        expect.same(Right(value), serialize[Either[Test, Int]](registry, value).flatMap(deserialize))
      }
    }
  }

  pureTest("deserializer should decode using compatible schemas") {
    val registry = new Registry
    whenSuccess(serialize(registry, Test2("test", 42))) { bytes =>
      expect.same(Right(Test("test")), deserializer[Test](registry).flatMap(_(bytes)))
    }
  }

  pureTest("deserializer should error for mismatching logical types") {
    val registry = new Registry
    whenSuccess(serialize(registry, 42L)) { bytes =>
      expect.same(
        Left("Error decoding Instant: Got unexpected missing logical type"),
        deserializer[Instant](registry).flatMap(_(bytes)).left.map(_.message)
      )
    }
  }

  pureTest("deserializer should error for invalid schemas") {
    val codec = Codec.decimal(-1, -1)
    expect.same(
      Left("Invalid decimal precision -1: must be positive"),
      deserializer(new Registry)(codec).left.map(_.message)
    )
  }

  pureTest("deserializer should error for values missing after the schema id") {
    val registry = new Registry
    whenSuccess(Codec[Int].schema) { schema =>
      val id = registry.register(schema)
      expect.same(
        Left("Unexpected end of input at position 0"),
        deserializer[Int](registry).flatMap(_(frame(id, ByteVector.empty))).left.map(_.message)
      )
    }
  }

  pureTest("deserializer should decode values the Java Avro library writes") {
    val registry = new Registry
    val encoded = (Codec[Test2].schema, Codec[Test2].encoder.flatMap(_.encode(Test2("test", 42))))
    whenSuccess(encoded.tupled) { case (schema, value) =>
      val javaSchema = JavaAvro.schema(schema)
      val datum = JavaAvro.toJava(value, schema, javaSchema)
      val bytes = frame(registry.register(schema), JavaAvro.writeBinary(javaSchema, datum))
      expect.same(Right(Test("test")), deserializer[Test](registry).flatMap(_(bytes)))
    }
  }

  pureTest("deserializer should decode values with a decoder compiled once for each writer schema") {
    val registry = new Registry
    whenSuccess(deserializer[Test](registry)) { deserialize =>
      val bytes = List("a", "b", "c").traverse(name => serialize(registry, Test2(name, 1)))
      expect.same(Right(List(Test("a"), Test("b"), Test("c"))), bytes.flatMap(_.traverse(deserialize))) &&
      expect.eql(1, registry.lookups)
    }
  }

  pureTest("schemas should be the same after printing and parsing") {
    val schemas = List(
      Codec[Test].schema,
      Codec[Test2].schema,
      Codec[Either[Test, Int]].schema,
      Codec[Instant].schema,
      Codec.decimal(10, 2).schema
    )

    forEach(schemas) { result =>
      whenSuccess(result) { schema =>
        expect.same(Right(schema), Schema.fromJson(schema.toJson)) &&
        expect.eql(schema.toString, JavaAvro.schema(schema).toString)
      }
    }
  }

  pureTest("schemas should decode using schemas with features from other libraries") {
    // Includes properties written by the Java Avro library for generated classes
    val json =
      """{"type":"record","name":"Test","namespace":"fs2.kafka.vulcan","fields":[{"name":"name","type":{"type":"string","avro.java.string":"String"}},{"name":"id","type":{"type":"string","logicalType":"custom-id"},"default":"none"},{"name":"next","type":["null","Test"],"default":null}],"aliases":["Other"]}"""

    whenSuccess(Schema.parse(json)) { writerSchema =>
      val javaSchema = JavaAvro.schema(writerSchema)

      val record = new GenericData.Record(javaSchema)
      record.put("name", "test")
      record.put("id", "1")
      record.put("next", null)

      val bytes = JavaAvro.writeBinary(javaSchema, record)
      expect.eql(json, writerSchema.toString) &&
      expect.eql(json, javaSchema.toString) &&
      expect.same(Right(Test("test")), Codec[Test].binaryDecoder(writerSchema).flatMap(_.decode(bytes)))
    }
  }

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

  def serialize[A](registry: Registry, a: A)(implicit codec: Codec[A]): Either[AvroException, Array[Byte]] =
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
  )(implicit codec: Codec[A]): Either[AvroException, Array[Byte] => Either[AvroException, A]] =
    codec.schema.map { _ =>
      val decoders = mutable.Map.empty[Int, Either[AvroException, BinaryDecoder[A]]]

      def decoder(id: Int): Either[AvroException, BinaryDecoder[A]] =
        decoders.getOrElseUpdate(
          id,
          registry
            .schema(id)
            .toRight(AvroException(s"Unknown schema id $id"))
            .flatMap(Schema.fromJson)
            .flatMap(codec.binaryDecoder)
        )

      bytes => {
        val input = ByteVector.view(bytes)
        if (input.size < 5 || input.head != 0) Left(AvroException("Missing magic byte and schema id"))
        else decoder(input.slice(1, 5).toInt()).flatMap(_.decode(input.drop(5)))
      }
    }
}
