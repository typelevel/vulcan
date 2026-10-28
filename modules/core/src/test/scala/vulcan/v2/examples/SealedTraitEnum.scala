/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.examples

import cats.Eq
import io.circe.Json
import io.circe.JsonObject
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import vulcan.AvroError
import vulcan.v2.Codec

sealed trait SealedTraitEnum

object SealedTraitEnum {
  implicit final val arbitrary: Arbitrary[SealedTraitEnum] =
    Arbitrary(Gen.oneOf(FirstInSealedTraitEnum, SecondInSealedTraitEnum))

  implicit final val eq: Eq[SealedTraitEnum] =
    Eq.fromUniversalEquals

  implicit final val codec: Codec[SealedTraitEnum] =
    Codec.enumeration(
      name = "SealedTraitEnum",
      namespace = "vulcan.examples",
      symbols = List("first", "second"),
      aliases = List("first", "second"),
      doc = Some("documentation"),
      default = Some(FirstInSealedTraitEnum),
      encode = {
        case FirstInSealedTraitEnum => "first"
        case SecondInSealedTraitEnum => "second"
      },
      decode = {
        case "first" => Right(FirstInSealedTraitEnum)
        case "second" => Right(SecondInSealedTraitEnum)
        case other => Left(AvroError(other))
      },
      props = JsonObject("custom1" -> Json.fromInt(10), "custom2" -> Json.fromString("value2"))
    )
}

case object FirstInSealedTraitEnum extends SealedTraitEnum

case object SecondInSealedTraitEnum extends SealedTraitEnum
