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

sealed trait SealedTraitRecursive

final case class LiteralInSealedTraitRecursive(value: Int) extends SealedTraitRecursive

final case class AddInSealedTraitRecursive(left: SealedTraitRecursive, right: SealedTraitRecursive)
  extends SealedTraitRecursive

object SealedTraitRecursive {
  implicit val sealedTraitRecursiveArbitrary: Arbitrary[SealedTraitRecursive] = {
    def gen(depth: Int): Gen[SealedTraitRecursive] =
      if (depth == 0) arbitrary[Int].map(LiteralInSealedTraitRecursive(_))
      else
        Gen.oneOf(
          arbitrary[Int].map(LiteralInSealedTraitRecursive(_)),
          Gen.zip(gen(depth - 1), gen(depth - 1)).map { case (left, right) =>
            AddInSealedTraitRecursive(left, right)
          }
        )

    Arbitrary(Gen.choose(0, 4).flatMap(gen))
  }

  implicit val codec: Codec[SealedTraitRecursive] =
    Codec.derive
}
