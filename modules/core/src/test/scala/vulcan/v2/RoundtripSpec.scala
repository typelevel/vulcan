/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.Assertion
import scodec.bits.ByteVector
import vulcan.{AvroError, BaseSpec}

final class RoundtripSpec extends BaseSpec with RoundtripHelpers {
  describe("Boolean") { it("roundtrip") { roundtrip[Boolean] } }

  describe("Byte") { it("roundtrip") { roundtrip[Byte] } }

  describe("ByteVector") {
    it("roundtrip") {
      implicit val byteVectorArbitrary: Arbitrary[ByteVector] =
        Arbitrary(arbitrary[Array[Byte]].map(ByteVector(_)))

      implicit val byteVectorEq: Eq[ByteVector] =
        Eq.fromUniversalEquals

      roundtrip[ByteVector]
    }
  }

  describe("Char") { it("roundtrip") { roundtrip[Char] } }

  describe("Double") { it("roundtrip") { roundtrip[Double] } }

  describe("Float") { it("roundtrip") { roundtrip[Float] } }

  describe("Int") { it("roundtrip") { roundtrip[Int] } }

  describe("Long") { it("roundtrip") { roundtrip[Long] } }

  describe("Short") { it("roundtrip") { roundtrip[Short] } }

  describe("String") { it("roundtrip") { roundtrip[String] } }

  describe("Unit") { it("roundtrip") { roundtrip[Unit] } }
}

trait RoundtripHelpers {
  self: BaseSpec =>

  def roundtrip[A](implicit
    codec: Codec[A],
    arbitrary: Arbitrary[A],
    eq: Eq[A]
  ): Assertion = {
    forAll { (a: A) =>
      roundtrip(a)
      binaryRoundtrip(a)
      jsonRoundtrip(a)
    }
  }

  def roundtrip[A](a: A)(implicit
    codec: Codec[A],
    eq: Eq[A]
  ): Assertion = {
    val avroSchema = codec.schema
    assert(avroSchema.isRight)

    val encoded = codec.encode(a)
    assert(encoded.isRight)

    val decoded: Either[AvroError, A] = codec.decode(encoded.value, avroSchema.value)
    withClue(s"Actual: $decoded, Expected: ${Right(a)}") {
      assert(decoded === Right(a))
    }
  }

  def binaryRoundtrip[A](a: A)(implicit
    codec: Codec[A],
    eq: Eq[A]
  ): Assertion = {
    val binary = Codec.toBinary(a)
    assert(binary.isRight)

    val decoded = codec.schema.flatMap(Codec.fromBinary[A](binary.value, _))
    withClue(s"Actual: $decoded, Expected: ${Right(a)}") {
      assert(decoded === Right(a))
    }
  }

  def jsonRoundtrip[A](a: A)(implicit
    codec: Codec[A],
    eq: Eq[A]
  ): Assertion = {
    val json = Codec.toJson(a)
    assert(json.isRight)

    val decoded = codec.schema.flatMap(Codec.fromJson[A](json.value, _))
    withClue(s"Actual: $decoded, Expected: ${Right(a)}") {
      assert(decoded === Right(a))
    }
  }
}
