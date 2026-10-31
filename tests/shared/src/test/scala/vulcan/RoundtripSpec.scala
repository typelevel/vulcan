/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Eq
import cats.data.*
import cats.laws.discipline.arbitrary.*
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
import java.util.UUID
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
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

    forallRoundtrip[ByteVector]
  }

  test("Chain roundtrip") { forallRoundtrip[Chain[Int]] }

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

    forallRoundtrip[Instant]
  }

  test("InstantMicros roundtrip") {
    implicit val instantMicrosArbitrary: Arbitrary[InstantMicros] =
      Arbitrary(arbitrary[Long].map(InstantMicros.fromEpochMicro))

    forallRoundtrip[InstantMicros]
  }

  test("Int roundtrip") { forallRoundtrip[Int] }

  test("List roundtrip") { forallRoundtrip[List[Int]] }

  test("LocalDate roundtrip") {
    implicit val localDateArbitrary: Arbitrary[LocalDate] =
      Arbitrary(arbitrary[Int].map(days => LocalDate.ofEpochDay(days.toLong)))

    forallRoundtrip[LocalDate]
  }

  test("LocalDateTime roundtrip") {
    implicit val localDateTimeArbitrary: Arbitrary[LocalDateTime] =
      Arbitrary(arbitrary[Long].map { millis =>
        LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
      })

    forallRoundtrip[LocalDateTime]
  }

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

  test("Long roundtrip") { forallRoundtrip[Long] }

  test("Map roundtrip") { forallRoundtrip[Map[String, Int]] }

  test("NonEmptyChain roundtrip") { forallRoundtrip[NonEmptyChain[Int]] }

  test("NonEmptyList roundtrip") { forallRoundtrip[NonEmptyList[Int]] }

  test("NonEmptyMap roundtrip") { forallRoundtrip[NonEmptyMap[String, Int]] }

  test("NonEmptySet roundtrip") { forallRoundtrip[NonEmptySet[Int]] }

  test("NonEmptyVector roundtrip") { forallRoundtrip[NonEmptyVector[Int]] }

  test("Option roundtrip") { forallRoundtrip[Option[Int]] }

  test("Option roundtrip with the types of unions") { forallRoundtrip[Option[Either[Int, String]]] }

  test("Record roundtrip") { forallRoundtrip[CaseClassField] }

  test("Seq roundtrip") { forallRoundtrip[Seq[Int]] }

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

    forallRoundtrip[UUID]
  }

  test("Vector roundtrip") { forallRoundtrip[Vector[Int]] }
}
