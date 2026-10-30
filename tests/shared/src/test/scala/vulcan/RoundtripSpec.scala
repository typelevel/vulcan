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

object RoundtripSpec extends BaseSuite with RoundtripHelpers {
  test("BigDecimal roundtrip") {
    implicit val bigDecimalCodec: Codec[BigDecimal] =
      Codec.decimal(precision = 10, scale = 5)

    implicit val bigDecimalArbitrary: Arbitrary[BigDecimal] =
      Arbitrary(Gen.chooseNum(-9999999999L, 9999999999L).map(BigDecimal(_, 5)))

    roundtrip[BigDecimal]
  }

  test("Boolean roundtrip") { roundtrip[Boolean] }

  test("Byte roundtrip") { roundtrip[Byte] }

  test("ByteVector roundtrip") {
    implicit val byteVectorArbitrary: Arbitrary[ByteVector] =
      Arbitrary(arbitrary[Array[Byte]].map(ByteVector(_)))

    implicit val byteVectorEq: Eq[ByteVector] =
      Eq.fromUniversalEquals

    roundtrip[ByteVector]
  }

  test("Chain roundtrip") {
    implicit def arbitraryChain[A](implicit
      arbitrary: Arbitrary[A]
    ): Arbitrary[Chain[A]] =
      Arbitrary(Gen.listOf(arbitrary.arbitrary).map(Chain.fromSeq))

    roundtrip[Chain[Int]]
  }

  test("Char roundtrip") { roundtrip[Char] }

  test("Double roundtrip") { roundtrip[Double] }

  pureTest("Double roundtrip negative zero") {
    val codec = Codec[Double]
    val bits = java.lang.Double.doubleToRawLongBits(-0.0d)
    val decoded = List(
      codec.binaryEncoder.value.encode(-0.0d).flatMap(codec.binaryDecoder(Schema.Double).value.decode),
      codec.jsonEncoder.value.encode(-0.0d).flatMap(codec.jsonDecoder(Schema.Double).value.decode)
    )
    forEach(decoded)(double => expect.same(Right(bits), double.map(java.lang.Double.doubleToRawLongBits)))
  }

  test("Either roundtrip") { roundtrip[Either[Int, String]] }

  test("Enum roundtrip") { roundtrip[SealedTraitEnum] }

  test("Fixed roundtrip") { roundtrip[FixedBoolean] }

  test("Float roundtrip") { roundtrip[Float] }

  pureTest("Float roundtrip negative zero") {
    val codec = Codec[Float]
    val bits = java.lang.Float.floatToRawIntBits(-0.0f)
    val decoded = List(
      codec.binaryEncoder.value.encode(-0.0f).flatMap(codec.binaryDecoder(Schema.Float).value.decode),
      codec.jsonEncoder.value.encode(-0.0f).flatMap(codec.jsonDecoder(Schema.Float).value.decode)
    )
    forEach(decoded)(float => expect.same(Right(bits), float.map(java.lang.Float.floatToRawIntBits)))
  }

  test("Instant roundtrip") {
    implicit val instantArbitrary: Arbitrary[Instant] =
      Arbitrary(arbitrary[Long].map(Instant.ofEpochMilli))

    implicit val instantEq: Eq[Instant] =
      Eq.fromUniversalEquals

    roundtrip[Instant]
  }

  test("InstantMicros roundtrip") {
    implicit val instantMicrosArbitrary: Arbitrary[InstantMicros] =
      Arbitrary(arbitrary[Long].map(InstantMicros.fromEpochMicro))

    implicit val instantMicrosEq: Eq[InstantMicros] =
      Eq.fromUniversalEquals

    roundtrip[InstantMicros]
  }

  test("Int roundtrip") { roundtrip[Int] }

  test("List roundtrip") { roundtrip[List[Int]] }

  test("LocalDate roundtrip") {
    implicit val localDateArbitrary: Arbitrary[LocalDate] =
      Arbitrary(arbitrary[Int].map(days => LocalDate.ofEpochDay(days.toLong)))

    implicit val localDateEq: Eq[LocalDate] =
      Eq.fromUniversalEquals

    roundtrip[LocalDate]
  }

  test("LocalDateTime roundtrip") {
    implicit val localDateTimeArbitrary: Arbitrary[LocalDateTime] =
      Arbitrary(arbitrary[Long].map { millis =>
        LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
      })

    implicit val localDateTimeEq: Eq[LocalDateTime] =
      Eq.fromUniversalEquals

    roundtrip[LocalDateTime]
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

      roundtrip[LocalTime]
    }

    test("LocalTime roundtrip millis") {
      implicit val localTimeCodec: Codec[LocalTime] =
        Codec.localTimeMillis

      implicit val localTimeArbitrary: Arbitrary[LocalTime] =
        Arbitrary(
          Gen.chooseNum(0L, 86399999L).map(millis => LocalTime.ofNanoOfDay(millis * 1000000L))
        )

      roundtrip[LocalTime]
    }
  }

  test("Long roundtrip") { roundtrip[Long] }

  test("Map roundtrip") { roundtrip[Map[String, Int]] }

  test("NonEmptyChain roundtrip") {
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

  test("NonEmptyList roundtrip") {
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

  test("NonEmptyMap roundtrip") {
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

    roundtrip[NonEmptySet[Int]]
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

    roundtrip[NonEmptyVector[Int]]
  }

  test("Option roundtrip") { roundtrip[Option[Int]] }

  test("Option roundtrip with the types of unions") { roundtrip[Option[Either[Int, String]]] }

  test("Record roundtrip") { roundtrip[CaseClassField] }

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

    roundtrip[Seq[Int]]
  }

  test("Set roundtrip") { roundtrip[Set[Int]] }

  test("Short roundtrip") { roundtrip[Short] }

  test("String roundtrip") { roundtrip[String] }

  test("Union roundtrip") { roundtrip[SealedTraitCaseClass] }

  test("Unit roundtrip") { roundtrip[Unit] }

  test("UUID roundtrip") {
    implicit val uuidArbitrary: Arbitrary[UUID] =
      Arbitrary(Gen.zip(arbitrary[Long], arbitrary[Long]).map { case (most, least) =>
        new UUID(most, least)
      })

    implicit val uuidEq: Eq[UUID] =
      Eq.fromUniversalEquals

    roundtrip[UUID]
  }

  test("Vector roundtrip") { roundtrip[Vector[Int]] }
}
