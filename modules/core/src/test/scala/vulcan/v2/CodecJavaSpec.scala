/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import java.io.EOFException
import java.nio.ByteBuffer
import org.scalatest.Assertion
import scala.jdk.CollectionConverters._
import scala.util.Try
import scodec.bits.ByteVector
import vulcan.BaseSpec
import vulcan.v2.CodecJavaSpec.{same, Reader}

/** Checks decoding values against schema resolution in the Java Avro library. */
final class CodecJavaSpec extends BaseSpec {
  private val primitiveReaders: List[Reader[_]] =
    List(
      Reader[Unit](Codec.unit, _ => ()),
      Reader[Boolean](Codec.boolean, _.asInstanceOf[java.lang.Boolean].booleanValue),
      Reader[Int](Codec.int, _.asInstanceOf[java.lang.Integer].intValue),
      Reader[Long](Codec.long, _.asInstanceOf[java.lang.Long].longValue),
      Reader[Float](Codec.float, _.asInstanceOf[java.lang.Float].floatValue),
      Reader[Double](Codec.double, _.asInstanceOf[java.lang.Double].doubleValue),
      Reader[ByteVector](Codec.bytes, bytes => ByteVector(bytes.asInstanceOf[ByteBuffer])),
      Reader[String](Codec.string, _.toString)
    )

  private val primitiveSchemas: List[Schema] =
    List(
      Schema.Null,
      Schema.Boolean,
      Schema.Int,
      Schema.Long,
      Schema.Float,
      Schema.Double,
      Schema.Bytes,
      Schema.String
    )

  describe("Codec") {
    it("should decode primitives the same as the Java Avro library resolves schemas") {
      primitiveSchemas.foreach { writerSchema =>
        primitiveReaders.foreach { reader =>
          forAll(AvroValueGenerators.genValue(writerSchema)) { value =>
            assertSameAsJava(reader, writerSchema, value)
          }
        }
      }
    }

    it("should decode arrays the same as the Java Avro library resolves schemas") {
      primitiveSchemas.foreach { items =>
        primitiveReaders.foreach { reader =>
          val writerSchema = Schema.Array(items)
          forAll(AvroValueGenerators.genValue(writerSchema)) { value =>
            assertSameAsJava(Reader.vector(reader), writerSchema, value)
          }
        }
      }
    }

    it("should decode maps the same as the Java Avro library resolves schemas") {
      primitiveSchemas.foreach { values =>
        primitiveReaders.foreach { reader =>
          val writerSchema = Schema.Map(values)
          forAll(AvroValueGenerators.genValue(writerSchema)) { value =>
            assertSameAsJava(Reader.map(reader), writerSchema, value)
          }
        }
      }
    }
  }

  /** Checks that decoding succeeds and fails the same way as for the Java Avro library. */
  private def assertSameAsJava[A](
    reader: Reader[A],
    writerSchema: Schema,
    value: AvroValue
  ): Assertion = {
    val javaWriterSchema = JavaAvro.schema(writerSchema)
    val javaReaderSchema = JavaAvro.schema(reader.codec.schema.value)
    val bytes =
      JavaAvro.writeBinary(javaWriterSchema, JavaAvro.toJava(value, writerSchema, javaWriterSchema))

    val decoded = Codec.fromBinary(bytes, writerSchema)(reader.codec)
    val javaDecoded =
      Try(JavaAvro.readBinary(javaWriterSchema, javaReaderSchema, bytes)).toEither
        .map(reader.fromJava)

    (decoded, javaDecoded) match {
      case (Right(a), Right(javaA)) => assert(same(a, javaA))
      case (Left(_), Left(_))       => succeed
      // The Java Avro library checks the remaining bytes using the minimum size of reader items,
      // so rejects collections of items promoted to larger types, for example from int to float
      case (Right(_), Left(error: EOFException)) if isReaderItemSizeCheck(error) => succeed
      case _ => fail(s"Decoded $decoded from $writerSchema, Java Avro decoded $javaDecoded")
    }
  }

  private def isReaderItemSizeCheck(error: EOFException): Boolean =
    error.getMessage.startsWith("Collection claims") || error.getMessage.startsWith("Map claims")
}

object CodecJavaSpec {

  /** A codec, together with a conversion from values read by the Java Avro library. */
  final case class Reader[A](codec: Codec[A], fromJava: Any => A)

  object Reader {
    def vector[A](items: Reader[A]): Reader[Vector[A]] =
      Reader(
        Codec.vector(items.codec),
        _.asInstanceOf[java.util.List[Any]].asScala.toVector.map(items.fromJava)
      )

    def map[A](values: Reader[A]): Reader[Map[String, A]] =
      Reader(
        Codec.map(values.codec),
        _.asInstanceOf[java.util.Map[Any, Any]].asScala.iterator
          .map { case (key, value) =>
            (key.toString, values.fromJava(value))
          }
          .toMap
      )
  }

  /** Compares boxed values, so NaN is equal to itself, and 0.0 and -0.0 are different. */
  def same(a: Any, b: Any): Boolean =
    (a, b) match {
      case (as: Vector[_], bs: Vector[_]) =>
        as.size == bs.size && as.lazyZip(bs).forall(same)

      case (as: Map[_, _], bs: Map[_, _]) =>
        as.keySet == bs.keySet && as.forall { case (key, value) =>
          same(value, bs.asInstanceOf[Map[Any, Any]](key))
        }

      case _ =>
        a.asInstanceOf[AnyRef].equals(b)
    }
}
