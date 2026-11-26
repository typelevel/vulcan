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

final case class CaseClassRecursive(value: Int, next: Option[CaseClassRecursive])

object CaseClassRecursive {
  implicit val caseClassRecursiveArbitrary: Arbitrary[CaseClassRecursive] = {
    def gen(depth: Int): Gen[CaseClassRecursive] =
      for {
        value <- arbitrary[Int]
        next <- if (depth > 0) Gen.option(gen(depth - 1)) else Gen.const(None)
      } yield CaseClassRecursive(value, next)

    Arbitrary(Gen.choose(0, 5).flatMap(gen))
  }

  implicit lazy val codec: Codec[CaseClassRecursive] =
    Codec.derive
}
