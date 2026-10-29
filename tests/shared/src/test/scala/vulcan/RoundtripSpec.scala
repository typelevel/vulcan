/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Eq
import cats.data.*
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
import java.util.UUID
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.Assertion
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scodec.bits.ByteVector
import vulcan.Codec.InstantMicros
import vulcan.examples.*

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

  describe("Double") {
    it("roundtrip") { roundtrip[Double] }

    it("roundtrip negative zero") {
      val codec = Codec[Double]
      val bits = java.lang.Double.doubleToRawLongBits(-0.0d)
      val decoded = List(
        codec.binaryEncoder.value.encode(-0.0d).flatMap(codec.binaryDecoder(Schema.Double).value.decode),
        codec.jsonEncoder.value.encode(-0.0d).flatMap(codec.jsonDecoder(Schema.Double).value.decode)
      )
      decoded.foreach(double => assert(double.map(java.lang.Double.doubleToRawLongBits) == Right(bits)))
    }
  }

  describe("Either") { it("roundtrip") { roundtrip[Either[Int, String]] } }

  describe("Enum") { it("roundtrip") { roundtrip[SealedTraitEnum] } }

  describe("Fixed") { it("roundtrip") { roundtrip[FixedBoolean] } }

  describe("Float") {
    it("roundtrip") { roundtrip[Float] }

    it("roundtrip negative zero") {
      val codec = Codec[Float]
      val bits = java.lang.Float.floatToRawIntBits(-0.0f)
      val decoded = List(
        codec.binaryEncoder.value.encode(-0.0f).flatMap(codec.binaryDecoder(Schema.Float).value.decode),
        codec.jsonEncoder.value.encode(-0.0f).flatMap(codec.jsonDecoder(Schema.Float).value.decode)
      )
      decoded.foreach(float => assert(float.map(java.lang.Float.floatToRawIntBits) == Right(bits)))
    }
  }

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

    val encoded = codec.encoder.flatMap(_.encode(a))
    assert(encoded.isRight)

    val decoded = codec.decoder(avroSchema.value).flatMap(_.decode(encoded.value))
    withClue(s"Actual: $decoded, Expected: ${Right(a)}") {
      assert(decoded.left.map(_.message) === Right(a))
    }
  }

  def binaryRoundtrip[A](a: A)(implicit
    codec: Codec[A],
    eq: Eq[A]
  ): Assertion = {
    val binary = codec.binaryEncoder.flatMap(_.encode(a))
    assert(binary.isRight)

    val decoded = codec.schema.flatMap(codec.binaryDecoder).flatMap(_.decode(binary.value))
    withClue(s"Actual: $decoded, Expected: ${Right(a)}") {
      assert(decoded.left.map(_.message) === Right(a))
    }
  }

  def jsonRoundtrip[A](a: A)(implicit
    codec: Codec[A],
    eq: Eq[A]
  ): Assertion = {
    val json = codec.jsonEncoder.flatMap(_.encode(a))
    assert(json.isRight)

    val decoded = codec.schema.flatMap(codec.jsonDecoder).flatMap(_.decode(json.value))
    withClue(s"Actual: $decoded, Expected: ${Right(a)}") {
      assert(decoded.left.map(_.message) === Right(a))
    }
  }
}
