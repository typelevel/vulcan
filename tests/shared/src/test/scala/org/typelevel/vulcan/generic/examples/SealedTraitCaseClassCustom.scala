/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

sealed trait SealedTraitCaseClassCustom

final case class FirstInSealedTraitCaseClassCustom(value: Int) extends SealedTraitCaseClassCustom

final case class SecondInSealedTraitCaseClassCustom(value: String) extends SealedTraitCaseClassCustom

object SecondInSealedTraitCaseClassCustom {
  implicit val codec: Codec[SecondInSealedTraitCaseClassCustom] =
    Codec[String].imap(apply)(_.value)
}

object SealedTraitCaseClassCustom {
  implicit val sealedTraitCaseClassCustomArbitrary: Arbitrary[SealedTraitCaseClassCustom] =
    Arbitrary(
      Gen.oneOf(
        arbitrary[Int].map(FirstInSealedTraitCaseClassCustom(_)),
        arbitrary[String].map(SecondInSealedTraitCaseClassCustom(_))
      )
    )

  implicit val codec: Codec[SealedTraitCaseClassCustom] =
    Codec.derive
}
