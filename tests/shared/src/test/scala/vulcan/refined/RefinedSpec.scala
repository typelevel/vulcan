/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.refined

import cats.Show
import eu.timepit.refined.api.Refined
import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.NonPosInt
import eu.timepit.refined.types.numeric.PosInt
import vulcan.BaseSuite
import vulcan.Codec

object RefinedSpec extends BaseSuite {
  implicit def refinedShow[T, P]: Show[Refined[T, P]] =
    Show.fromToString

  test("Refined should succeed for values conforming to predicate") {
    forall { (posInt: PosInt) =>
      val codec = Codec[PosInt]
      val decoded =
        for {
          schema <- codec.schema
          encoded <- codec.encoder.flatMap(_.encode(posInt))
          decoded <- codec.decoder(schema).flatMap(_.decode(encoded))
        } yield decoded

      expect.same(Right(posInt), decoded)
    }
  }

  test("Refined should fail for values not conforming to predicate") {
    forall { (nonPosInt: NonPosInt) =>
      val codec = Codec[PosInt]
      val decoded =
        for {
          schema <- codec.schema
          encoded <- Codec[Int].encoder.flatMap(_.encode(nonPosInt.value))
          decoded <- codec.decoder(schema).flatMap(_.decode(encoded))
        } yield decoded

      expect.same(Left(s"Predicate failed: ($nonPosInt > 0)."), decoded.left.map(_.message))
    }
  }
}
