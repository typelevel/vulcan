/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.typelevel.vulcan.AvroException
import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

@AvroNamespace("com.example")
sealed trait SealedTraitEnumDerived

object SealedTraitEnumDerived {
  implicit final val arbitrary: Arbitrary[SealedTraitEnumDerived] =
    Arbitrary(Gen.oneOf(FirstInSealedTraitEnumDerived, SecondInSealedTraitEnumDerived))

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
        case other => Left(AvroException(other))
      }
    )
}

case object FirstInSealedTraitEnumDerived extends SealedTraitEnumDerived

case object SecondInSealedTraitEnumDerived extends SealedTraitEnumDerived
