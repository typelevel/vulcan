/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import cats.Eq
import cats.data._
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZoneOffset}
import java.util.UUID
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.Assertion
import scala.collection.immutable.{SortedMap, SortedSet}
import scodec.bits.ByteVector
import vulcan.{AvroError, BaseSpec}
import vulcan.v2.Codec.InstantMicros
import vulcan.v2.examples._

final class RoundtripSpec extends BaseSpec with RoundtripHelpers {
  describe("BigDecimal") {
    it("roundtrip") {
      implicit val bigDecimalCodec: Codec[BigDecimal] =
        Codec.decimal(precision = 10, scale = 5)

      implicit val bigDecimalArbitrary: Arbitrary[BigDecimal] =
        Arbitrary(Gen.chooseNum(-9999999999L, 9999999999L).map(BigDecimal(_, 5)))

      roundtrip[BigDecimal]
    }
  }

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

  describe("Either") { it("roundtrip") { roundtrip[Either[Int, String]] } }

  describe("Enum") { it("roundtrip") { roundtrip[SealedTraitEnum] } }

  describe("Fixed") { it("roundtrip") { roundtrip[FixedBoolean] } }

  describe("Float") { it("roundtrip") { roundtrip[Float] } }

  describe("Instant") {
    it("roundtrip") {
      implicit val instantArbitrary: Arbitrary[Instant] =
        Arbitrary(arbitrary[Long].map(Instant.ofEpochMilli))

      implicit val instantEq: Eq[Instant] =
        Eq.fromUniversalEquals

      roundtrip[Instant]
    }
  }

  describe("InstantMicros") {
    it("roundtrip") {
      implicit val instantMicrosArbitrary: Arbitrary[InstantMicros] =
        Arbitrary(arbitrary[Long].map(InstantMicros.fromEpochMicro))

      implicit val instantMicrosEq: Eq[InstantMicros] =
        Eq.fromUniversalEquals

      roundtrip[InstantMicros]
    }
  }

  describe("Int") { it("roundtrip") { roundtrip[Int] } }

  describe("List") { it("roundtrip") { roundtrip[List[Int]] } }

  describe("LocalDate") {
    it("roundtrip") {
      implicit val localDateArbitrary: Arbitrary[LocalDate] =
        Arbitrary(arbitrary[Int].map(days => LocalDate.ofEpochDay(days.toLong)))

      implicit val localDateEq: Eq[LocalDate] =
        Eq.fromUniversalEquals

      roundtrip[LocalDate]
    }
  }

  describe("LocalDateTime") {
    it("roundtrip") {
      implicit val localDateTimeArbitrary: Arbitrary[LocalDateTime] =
        Arbitrary(arbitrary[Long].map { millis =>
          LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
        })

      implicit val localDateTimeEq: Eq[LocalDateTime] =
        Eq.fromUniversalEquals

      roundtrip[LocalDateTime]
    }
  }

  describe("LocalTime") {
    implicit val localTimeEq: Eq[LocalTime] =
      Eq.fromUniversalEquals

    it("roundtrip micros") {
      implicit val localTimeCodec: Codec[LocalTime] =
        Codec.localTimeMicros

      implicit val localTimeArbitrary: Arbitrary[LocalTime] =
        Arbitrary(
          Gen.chooseNum(0L, 86399999999L).map(micros => LocalTime.ofNanoOfDay(micros * 1000L))
        )

      roundtrip[LocalTime]
    }

    it("roundtrip millis") {
      implicit val localTimeCodec: Codec[LocalTime] =
        Codec.localTimeMillis

      implicit val localTimeArbitrary: Arbitrary[LocalTime] =
        Arbitrary(
          Gen.chooseNum(0L, 86399999L).map(millis => LocalTime.ofNanoOfDay(millis * 1000000L))
        )

      roundtrip[LocalTime]
    }
  }

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

  describe("Option") {
    it("roundtrip") { roundtrip[Option[Int]] }

    it("roundtrip with the types of unions") { roundtrip[Option[Either[Int, String]]] }
  }

  describe("Record") { it("roundtrip") { roundtrip[CaseClassField] } }

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

  describe("Union") { it("roundtrip") { roundtrip[SealedTraitCaseClass] } }

  describe("Unit") { it("roundtrip") { roundtrip[Unit] } }

  describe("UUID") {
    it("roundtrip") {
      implicit val uuidArbitrary: Arbitrary[UUID] =
        Arbitrary(Gen.zip(arbitrary[Long], arbitrary[Long]).map { case (most, least) =>
          new UUID(most, least)
        })

      implicit val uuidEq: Eq[UUID] =
        Eq.fromUniversalEquals

      roundtrip[UUID]
    }
  }

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
