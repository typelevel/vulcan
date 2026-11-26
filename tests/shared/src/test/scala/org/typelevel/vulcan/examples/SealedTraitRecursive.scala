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

sealed trait SealedTraitRecursive

object SealedTraitRecursive {
  final case class Literal(value: Int) extends SealedTraitRecursive

  object Literal {
    implicit val codec: Codec[Literal] =
      Codec.record("Literal", "com.example") { field =>
        field("value", _.value).map(Literal(_))
      }
  }

  final case class Negate(expression: SealedTraitRecursive) extends SealedTraitRecursive

  object Negate {
    implicit val codec: Codec[Negate] =
      Codec.record("Negate", "com.example") { field =>
        field("expression", _.expression).map(Negate(_))
      }
  }

  final case class Add(left: SealedTraitRecursive, right: SealedTraitRecursive) extends SealedTraitRecursive

  object Add {
    implicit val codec: Codec[Add] =
      Codec.record("Add", "com.example") { field =>
        (field("left", _.left), field("right", _.right)).mapN(Add(_, _))
      }
  }

  implicit val codec: Codec[SealedTraitRecursive] =
    Codec.union(alt => alt[Literal] ++ alt[Negate] ++ alt[Add])

  implicit val sealedTraitRecursiveArbitrary: Arbitrary[SealedTraitRecursive] = {
    def gen(depth: Int): Gen[SealedTraitRecursive] =
      if (depth == 0) arbitrary[Int].map(Literal(_))
      else
        Gen.oneOf(
          arbitrary[Int].map(Literal(_)),
          gen(depth - 1).map(Negate(_)),
          Gen.zip(gen(depth - 1), gen(depth - 1)).map { case (left, right) => Add(left, right) }
        )

    Arbitrary(Gen.choose(0, 4).flatMap(gen))
  }
}
