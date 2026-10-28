/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import vulcan.AvroError
import vulcan.Codec
import vulcan.generic.*

@AvroNamespace("com.example")
sealed trait SealedTraitEnumDerived

object SealedTraitEnumDerived {
  implicit final val arbitrary: Arbitrary[SealedTraitEnumDerived] =
    Arbitrary(Gen.oneOf(FirstInSealedTraitEnumDerived, SecondInSealedTraitEnumDerived))

  implicit final val eq: Eq[SealedTraitEnumDerived] =
    Eq.fromUniversalEquals

  implicit final val codec: Codec[SealedTraitEnumDerived] =
    deriveEnum(
      symbols = List("first", "second"),
      encode = {
        case FirstInSealedTraitEnumDerived => "first"
        case SecondInSealedTraitEnumDerived => "second"
      },
      decode = {
        case "first" => Right(FirstInSealedTraitEnumDerived)
        case "second" => Right(SecondInSealedTraitEnumDerived)
        case other => Left(AvroError(other))
      }
    )
}

case object FirstInSealedTraitEnumDerived extends SealedTraitEnumDerived

case object SecondInSealedTraitEnumDerived extends SealedTraitEnumDerived
