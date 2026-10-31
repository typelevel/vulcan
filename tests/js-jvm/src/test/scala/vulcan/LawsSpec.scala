/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Eq
import cats.kernel.laws.discipline.HashTests
import cats.laws.discipline.ContravariantTests
import cats.laws.discipline.InvariantTests
import cats.laws.discipline.MonadErrorTests
import io.circe.Json
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.rng.Seed
import scodec.bits.ByteVector
import weaver.FunSuite
import weaver.discipline.Discipline

// Not run on Scala Native, where the test process intermittently segfaults
// in these tests: https://github.com/scala-native/scala-native/issues/5086
object LawsSpec extends FunSuite with Discipline {
  implicit val avroExceptionEq: Eq[AvroException] =
    Eq.by(_.message)

  implicit val avroExceptionArbitrary: Arbitrary[AvroException] =
    Arbitrary(Arbitrary.arbitrary[String].map(AvroException(_)))

  implicit val avroExceptionCogen: Cogen[AvroException] =
    Cogen[String].contramap(_.message)

  implicit val avroValueArbitrary: Arbitrary[AvroValue] =
    Arbitrary(AvroValueGenerators.genSchemaAndValue.map(_._2))

  implicit val avroValueCogen: Cogen[AvroValue] =
    Cogen[String].contramap(_.toString)

  implicit val byteVectorArbitrary: Arbitrary[ByteVector] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(ByteVector(_)))

  implicit val byteVectorCogen: Cogen[ByteVector] =
    Cogen[Array[Byte]].contramap(_.toArray)

  implicit val byteVectorEq: Eq[ByteVector] =
    Eq.fromUniversalEquals

  implicit val jsonArbitrary: Arbitrary[Json] =
    Arbitrary(SchemaGenerators.genJson)

  implicit val jsonCogen: Cogen[Json] =
    Cogen[String].contramap(_.noSpaces)

  implicit val schemaArbitrary: Arbitrary[Schema] =
    Arbitrary(SchemaGenerators.genSchema)

  implicit val schemaCogen: Cogen[Schema] =
    Cogen[String].contramap(_.toString)

  /** Schemas together with a value which is valid for the schema. */
  implicit val schemaAndValueArbitrary: Arbitrary[(Schema, AvroValue)] =
    Arbitrary(AvroValueGenerators.genSchemaAndValue)

  private def arbitraryFunction[F, I: Cogen, O: Arbitrary](f: (I => O) => F): Arbitrary[F] =
    Arbitrary(Arbitrary.arbitrary[I => O].map(f))

  private def sampleEq[F, I: Arbitrary, O: Eq](f: F => I => O): Eq[F] = {
    val inputs = Gen.listOfN(50, Arbitrary.arbitrary[I]).pureApply(Gen.Parameters.default, Seed(0L))
    Eq.instance { (x, y) =>
      val (fx, fy) = (f(x), f(y))
      inputs.forall(input => Eq[O].eqv(fx(input), fy(input)))
    }
  }

  checkAll("AvroValue", HashTests[AvroValue].hash)

  implicit def binaryDecoderArbitrary[A: Arbitrary]: Arbitrary[BinaryDecoder[A]] =
    arbitraryFunction[BinaryDecoder[A], ByteVector, Either[AvroException, A]](f => bytes => f(bytes))

  implicit def binaryDecoderEq[A: Eq]: Eq[BinaryDecoder[A]] =
    sampleEq[BinaryDecoder[A], ByteVector, Either[AvroException, A]](decoder => decoder.decode)

  checkAll("BinaryDecoder", MonadErrorTests[BinaryDecoder, AvroException].monadError[Int, Int, String])

  implicit def binaryEncoderArbitrary[A: Cogen]: Arbitrary[BinaryEncoder[A]] =
    arbitraryFunction[BinaryEncoder[A], A, Either[AvroException, ByteVector]](f => a => f(a))

  implicit def binaryEncoderEq[A: Arbitrary]: Eq[BinaryEncoder[A]] =
    sampleEq[BinaryEncoder[A], A, Either[AvroException, ByteVector]](encoder => encoder.encode)

  checkAll("BinaryEncoder", ContravariantTests[BinaryEncoder].contravariant[Int, Int, String])

  implicit def codecArbitrary[A: Arbitrary: Cogen]: Arbitrary[Codec[A]] =
    Arbitrary {
      for {
        schema <- Arbitrary.arbitrary[Either[AvroException, Schema]]
        encoder <- Arbitrary.arbitrary[Encoder[A]]
        decoder <- Arbitrary.arbitrary[Schema => Either[AvroException, Decoder[A]]]
      } yield Codec.instance(schema, encoder, decoder)
    }

  implicit def codecEq[A: Arbitrary: Eq]: Eq[Codec[A]] =
    Eq.and(
      Eq.by[Codec[A], Either[AvroException, Schema]](_.schema),
      Eq.and(
        sampleEq[Codec[A], A, Either[AvroException, AvroValue]] { codec =>
          val encoder = codec.encoder
          a => encoder.flatMap(_.encode(a))
        },
        sampleEq[Codec[A], (Schema, AvroValue), Either[AvroException, A]] { codec =>
          { case (writerSchema, value) => codec.decoder(writerSchema).flatMap(_.decode(value)) }
        }
      )
    )

  checkAll("Codec", InvariantTests[Codec].invariant[Int, Int, String])

  implicit def decoderArbitrary[A: Arbitrary]: Arbitrary[Decoder[A]] =
    arbitraryFunction[Decoder[A], AvroValue, Either[AvroException, A]](f => value => f(value))

  implicit def decoderEq[A: Eq]: Eq[Decoder[A]] =
    sampleEq[Decoder[A], AvroValue, Either[AvroException, A]](decoder => decoder.decode)

  checkAll("Decoder", MonadErrorTests[Decoder, AvroException].monadError[Int, Int, String])

  implicit def encoderArbitrary[A: Cogen]: Arbitrary[Encoder[A]] =
    arbitraryFunction[Encoder[A], A, Either[AvroException, AvroValue]](f => a => f(a))

  implicit def encoderEq[A: Arbitrary]: Eq[Encoder[A]] =
    sampleEq[Encoder[A], A, Either[AvroException, AvroValue]](encoder => encoder.encode)

  checkAll("Encoder", ContravariantTests[Encoder].contravariant[Int, Int, String])

  implicit def jsonDecoderArbitrary[A: Arbitrary]: Arbitrary[JsonDecoder[A]] =
    arbitraryFunction[JsonDecoder[A], Json, Either[AvroException, A]](f => json => f(json))

  implicit def jsonDecoderEq[A: Eq]: Eq[JsonDecoder[A]] =
    sampleEq[JsonDecoder[A], Json, Either[AvroException, A]](decoder => decoder.decode)

  checkAll("JsonDecoder", MonadErrorTests[JsonDecoder, AvroException].monadError[Int, Int, String])

  implicit def jsonEncoderArbitrary[A: Cogen]: Arbitrary[JsonEncoder[A]] =
    arbitraryFunction[JsonEncoder[A], A, Either[AvroException, Json]](f => a => f(a))

  implicit def jsonEncoderEq[A: Arbitrary]: Eq[JsonEncoder[A]] =
    sampleEq[JsonEncoder[A], A, Either[AvroException, Json]](encoder => encoder.encode)

  checkAll("JsonEncoder", ContravariantTests[JsonEncoder].contravariant[Int, Int, String])

  /** Few distinct logical types, so equal ones are generated often. */
  implicit val logicalTypeArbitrary: Arbitrary[LogicalType] =
    Arbitrary {
      Gen.oneOf(
        Gen.oneOf(LogicalType.date, LogicalType.timestampMillis, LogicalType.uuid),
        Gen.zip(Gen.choose(1, 2), Gen.choose(0, 1)).map { case (precision, scale) =>
          LogicalType.decimal(precision, scale)
        },
        Gen.oneOf("custom", "other").map(LogicalType(_))
      )
    }

  implicit val logicalTypeCogen: Cogen[LogicalType] =
    Cogen[String].contramap(_.toString)

  checkAll("LogicalType", HashTests[LogicalType].hash)

  checkAll("Schema", HashTests[Schema].hash)
}
