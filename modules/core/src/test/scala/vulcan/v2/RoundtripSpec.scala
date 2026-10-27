/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import cats.Eq
import cats.data._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.Assertion
import scala.collection.immutable.{SortedMap, SortedSet}
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

  describe("Chain") {
    it("roundtrip") {
      implicit def arbitraryChain[A](implicit
        arbitrary: Arbitrary[A]
      ): Arbitrary[Chain[A]] =
        Arbitrary(Gen.listOf(arbitrary.arbitrary).map(Chain.fromSeq))

      roundtrip[Chain[Int]]
    }
  }

  describe("Char") { it("roundtrip") { roundtrip[Char] } }

  describe("Double") { it("roundtrip") { roundtrip[Double] } }

  describe("Float") { it("roundtrip") { roundtrip[Float] } }

  describe("Int") { it("roundtrip") { roundtrip[Int] } }

  describe("List") { it("roundtrip") { roundtrip[List[Int]] } }

  describe("Long") { it("roundtrip") { roundtrip[Long] } }

  describe("Map") { it("roundtrip") { roundtrip[Map[String, Int]] } }

  describe("NonEmptyChain") {
    it("roundtrip") {
      implicit def arbitraryNonEmptyChain[A](implicit
        arbitrary: Arbitrary[A]
      ): Arbitrary[NonEmptyChain[A]] =
        Arbitrary(
          Gen
            .nonEmptyListOf(arbitrary.arbitrary)
            .map(nel => NonEmptyChain.fromChainUnsafe(Chain.fromSeq(nel)))
        )

      roundtrip[NonEmptyChain[Int]]
    }
  }

  describe("NonEmptyList") {
    it("roundtrip") {
      implicit def arbitraryNonEmptyList[A](implicit
        arbitrary: Arbitrary[A]
      ): Arbitrary[NonEmptyList[A]] =
        Arbitrary(
          Gen
            .nonEmptyListOf(arbitrary.arbitrary)
            .map(NonEmptyList.fromListUnsafe)
        )

      roundtrip[NonEmptyList[Int]]
    }
  }

  describe("NonEmptyMap") {
    it("roundtrip") {
      implicit def arbitraryNonEmptyMap[A](implicit
        arbitrary: Arbitrary[A]
      ): Arbitrary[NonEmptyMap[String, A]] =
        Arbitrary(
          Gen
            .nonEmptyListOf(Arbitrary.arbitrary[(String, A)])
            .map(entries => NonEmptyMap.fromMapUnsafe(SortedMap(entries: _*)))
        )

      roundtrip[NonEmptyMap[String, Int]]
    }
  }

  describe("NonEmptySet") {
    it("roundtrip") {
      implicit def arbitraryNonEmptySet[A](implicit
        arbitrary: Arbitrary[A],
        ordering: Ordering[A]
      ): Arbitrary[NonEmptySet[A]] =
        Arbitrary(
          Gen
            .nonEmptyListOf(arbitrary.arbitrary)
            .map(nel => NonEmptySet.fromSetUnsafe(SortedSet(nel: _*)))
        )

      roundtrip[NonEmptySet[Int]]
    }
  }

  describe("NonEmptyVector") {
    it("roundtrip") {
      implicit def arbitraryNonEmptyVector[A](implicit
        arbitrary: Arbitrary[A]
      ): Arbitrary[NonEmptyVector[A]] =
        Arbitrary(
          Gen
            .nonEmptyListOf(arbitrary.arbitrary)
            .map(nel => NonEmptyVector.fromVectorUnsafe(nel.toVector))
        )

      roundtrip[NonEmptyVector[Int]]
    }
  }

  describe("Seq") {
    it("roundtrip") {
      implicit def seqArbitrary[A](implicit
        arbitrary: Arbitrary[A]
      ): Arbitrary[Seq[A]] =
        Arbitrary(Gen.listOf(arbitrary.arbitrary))

      implicit def seqEq[A](implicit
        eq: Eq[A]
      ): Eq[Seq[A]] =
        Eq.instance { (a, b) =>
          a.size == b.size && a.zip(b).forall { case (a1, a2) => eq.eqv(a1, a2) }
        }

      roundtrip[Seq[Int]]
    }
  }

  describe("Set") { it("roundtrip") { roundtrip[Set[Int]] } }

  describe("Short") { it("roundtrip") { roundtrip[Short] } }

  describe("String") { it("roundtrip") { roundtrip[String] } }

  describe("Unit") { it("roundtrip") { roundtrip[Unit] } }

  describe("Vector") { it("roundtrip") { roundtrip[Vector[Int]] } }
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
