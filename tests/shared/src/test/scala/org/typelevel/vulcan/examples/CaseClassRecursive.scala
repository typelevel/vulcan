/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.examples

import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.typelevel.vulcan.Codec

final case class CaseClassRecursive(value: Int, next: Option[CaseClassRecursive])

object CaseClassRecursive {
  implicit val codec: Codec[CaseClassRecursive] =
    Codec.record("CaseClassRecursive", "com.example") { field =>
      (
        field("value", _.value),
        field("next", _.next, default = Some(None))
      ).mapN(CaseClassRecursive(_, _))
    }

  implicit val caseClassRecursiveArbitrary: Arbitrary[CaseClassRecursive] = {
    def gen(depth: Int): Gen[CaseClassRecursive] =
      for {
        value <- arbitrary[Int]
        next <- if (depth > 0) Gen.option(gen(depth - 1)) else Gen.const(None)
      } yield CaseClassRecursive(value, next)

    Arbitrary(Gen.choose(0, 5).flatMap(gen))
  }
}
