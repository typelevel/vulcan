/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import java.nio.ByteBuffer
import org.scalatest.Assertion
import scala.util.Try
import scodec.bits.ByteVector
import vulcan.BaseSpec
import vulcan.v2.CodecJavaSpec.Reader

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
      // Compares boxed values, so NaN is equal to itself, and 0.0 and -0.0 are different
      case (Right(a), Right(javaA)) => assert(a.asInstanceOf[AnyRef].equals(javaA))
      case (Left(_), Left(_))       => succeed
      case _ => fail(s"Decoded $decoded from $writerSchema, Java Avro decoded $javaDecoded")
    }
  }
}

object CodecJavaSpec {

  /** A codec, together with a conversion from values read by the Java Avro library. */
  final case class Reader[A](codec: Codec[A], fromJava: Any => A)
}
