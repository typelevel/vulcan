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
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scodec.bits.ByteVector
import vulcan.Codec.InstantMicros
import vulcan.examples.*

object RoundtripSpec extends BaseSuite {
  test("BigDecimal roundtrip") {
    implicit val bigDecimalCodec: Codec[BigDecimal] =
      Codec.decimal(precision = 10, scale = 5)

    implicit val bigDecimalArbitrary: Arbitrary[BigDecimal] =
      Arbitrary(Gen.chooseNum(-9999999999L, 9999999999L).map(BigDecimal(_, 5)))

    forallRoundtrip[BigDecimal]
  }

  test("Boolean roundtrip") { forallRoundtrip[Boolean] }

  test("Byte roundtrip") { forallRoundtrip[Byte] }

  test("ByteVector roundtrip") {
    implicit val byteVectorArbitrary: Arbitrary[ByteVector] =
      Arbitrary(arbitrary[Array[Byte]].map(ByteVector(_)))

    implicit val byteVectorEq: Eq[ByteVector] =
      Eq.fromUniversalEquals

    forallRoundtrip[ByteVector]
  }

  test("Chain roundtrip") {
    implicit def arbitraryChain[A](implicit
      arbitrary: Arbitrary[A]
    ): Arbitrary[Chain[A]] =
      Arbitrary(Gen.listOf(arbitrary.arbitrary).map(Chain.fromSeq))

    forallRoundtrip[Chain[Int]]
  }

  test("Char roundtrip") { forallRoundtrip[Char] }

  test("Double roundtrip") { forallRoundtrip[Double] }

  pureTest("Double roundtrip negative zero") {
    implicit val doubleBitsEq: Eq[Double] =
      Eq.by(java.lang.Double.doubleToRawLongBits(_))

    expectRoundtrip(-0.0d)
  }

  test("Either roundtrip") { forallRoundtrip[Either[Int, String]] }

  test("Enum roundtrip") { forallRoundtrip[SealedTraitEnum] }

  test("Fixed roundtrip") { forallRoundtrip[FixedBoolean] }

  test("Float roundtrip") { forallRoundtrip[Float] }

  pureTest("Float roundtrip negative zero") {
    implicit val floatBitsEq: Eq[Float] =
      Eq.by(java.lang.Float.floatToRawIntBits(_))

    expectRoundtrip(-0.0f)
  }

  test("Instant roundtrip") {
    implicit val instantArbitrary: Arbitrary[Instant] =
      Arbitrary(arbitrary[Long].map(Instant.ofEpochMilli))

    implicit val instantEq: Eq[Instant] =
      Eq.fromUniversalEquals

    forallRoundtrip[Instant]
  }

  test("InstantMicros roundtrip") {
    implicit val instantMicrosArbitrary: Arbitrary[InstantMicros] =
      Arbitrary(arbitrary[Long].map(InstantMicros.fromEpochMicro))

    implicit val instantMicrosEq: Eq[InstantMicros] =
      Eq.fromUniversalEquals

    forallRoundtrip[InstantMicros]
  }

  test("Int roundtrip") { forallRoundtrip[Int] }

  test("List roundtrip") { forallRoundtrip[List[Int]] }

  test("LocalDate roundtrip") {
    implicit val localDateArbitrary: Arbitrary[LocalDate] =
      Arbitrary(arbitrary[Int].map(days => LocalDate.ofEpochDay(days.toLong)))

    implicit val localDateEq: Eq[LocalDate] =
      Eq.fromUniversalEquals

    forallRoundtrip[LocalDate]
  }

  test("LocalDateTime roundtrip") {
    implicit val localDateTimeArbitrary: Arbitrary[LocalDateTime] =
      Arbitrary(arbitrary[Long].map { millis =>
        LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
      })

    implicit val localDateTimeEq: Eq[LocalDateTime] =
      Eq.fromUniversalEquals

    forallRoundtrip[LocalDateTime]
  }

  locally {
    implicit val localTimeEq: Eq[LocalTime] =
      Eq.fromUniversalEquals

    test("LocalTime roundtrip micros") {
      implicit val localTimeCodec: Codec[LocalTime] =
        Codec.localTimeMicros

      implicit val localTimeArbitrary: Arbitrary[LocalTime] =
        Arbitrary(
          Gen.chooseNum(0L, 86399999999L).map(micros => LocalTime.ofNanoOfDay(micros * 1000L))
        )

      forallRoundtrip[LocalTime]
    }

    test("LocalTime roundtrip millis") {
      implicit val localTimeCodec: Codec[LocalTime] =
        Codec.localTimeMillis

      implicit val localTimeArbitrary: Arbitrary[LocalTime] =
        Arbitrary(
          Gen.chooseNum(0L, 86399999L).map(millis => LocalTime.ofNanoOfDay(millis * 1000000L))
        )

      forallRoundtrip[LocalTime]
    }
  }

  test("Long roundtrip") { forallRoundtrip[Long] }

  test("Map roundtrip") { forallRoundtrip[Map[String, Int]] }

  test("NonEmptyChain roundtrip") {
    implicit def arbitraryNonEmptyChain[A](implicit
      arbitrary: Arbitrary[A]
    ): Arbitrary[NonEmptyChain[A]] =
      Arbitrary(
        Gen
          .nonEmptyListOf(arbitrary.arbitrary)
          .map(nel => NonEmptyChain.fromChainUnsafe(Chain.fromSeq(nel)))
      )

    forallRoundtrip[NonEmptyChain[Int]]
  }

  test("NonEmptyList roundtrip") {
    implicit def arbitraryNonEmptyList[A](implicit
      arbitrary: Arbitrary[A]
    ): Arbitrary[NonEmptyList[A]] =
      Arbitrary(
        Gen
          .nonEmptyListOf(arbitrary.arbitrary)
          .map(NonEmptyList.fromListUnsafe)
      )

    forallRoundtrip[NonEmptyList[Int]]
  }

  test("NonEmptyMap roundtrip") {
    implicit def arbitraryNonEmptyMap[A](implicit
      arbitrary: Arbitrary[A]
    ): Arbitrary[NonEmptyMap[String, A]] =
      Arbitrary(
        Gen
          .nonEmptyListOf(Arbitrary.arbitrary[(String, A)])
          .map(entries => NonEmptyMap.fromMapUnsafe(SortedMap(entries: _*)))
      )

    forallRoundtrip[NonEmptyMap[String, Int]]
  }

  test("NonEmptySet roundtrip") {
    implicit def arbitraryNonEmptySet[A](implicit
      arbitrary: Arbitrary[A],
      ordering: Ordering[A]
    ): Arbitrary[NonEmptySet[A]] =
      Arbitrary(
        Gen
          .nonEmptyListOf(arbitrary.arbitrary)
          .map(nel => NonEmptySet.fromSetUnsafe(SortedSet(nel: _*)))
      )

    forallRoundtrip[NonEmptySet[Int]]
  }

  test("NonEmptyVector roundtrip") {
    implicit def arbitraryNonEmptyVector[A](implicit
      arbitrary: Arbitrary[A]
    ): Arbitrary[NonEmptyVector[A]] =
      Arbitrary(
        Gen
          .nonEmptyListOf(arbitrary.arbitrary)
          .map(nel => NonEmptyVector.fromVectorUnsafe(nel.toVector))
      )

    forallRoundtrip[NonEmptyVector[Int]]
  }

  test("Option roundtrip") { forallRoundtrip[Option[Int]] }

  test("Option roundtrip with the types of unions") { forallRoundtrip[Option[Either[Int, String]]] }

  test("Record roundtrip") { forallRoundtrip[CaseClassField] }

  test("Seq roundtrip") {
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

    forallRoundtrip[Seq[Int]]
  }

  test("Set roundtrip") { forallRoundtrip[Set[Int]] }

  test("Short roundtrip") { forallRoundtrip[Short] }

  test("String roundtrip") { forallRoundtrip[String] }

  test("Union roundtrip") { forallRoundtrip[SealedTraitCaseClass] }

  test("Unit roundtrip") { forallRoundtrip[Unit] }

  test("UUID roundtrip") {
    implicit val uuidArbitrary: Arbitrary[UUID] =
      Arbitrary(Gen.zip(arbitrary[Long], arbitrary[Long]).map { case (most, least) =>
        new UUID(most, least)
      })

    implicit val uuidEq: Eq[UUID] =
      Eq.fromUniversalEquals

    forallRoundtrip[UUID]
  }

  test("Vector roundtrip") { forallRoundtrip[Vector[Int]] }
}
