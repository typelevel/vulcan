/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.refined

import cats.Show
import eu.timepit.refined.api.Refined
import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.NonPosInt
import eu.timepit.refined.types.numeric.PosInt
import org.typelevel.vulcan.BaseSuite
import org.typelevel.vulcan.Codec

object RefinedSpec extends BaseSuite {
  implicit def refinedShow[T, P]: Show[Refined[T, P]] =
    Show.fromToString

  test("Refined should succeed for values conforming to predicate") {
    forallRoundtrip[PosInt]
  }

  test("Refined should fail for values not conforming to predicate") {
    forall { (nonPosInt: NonPosInt) =>
      expectDecodeError[PosInt](
        Codec[Int].encode(nonPosInt.value),
        s"Predicate failed: ($nonPosInt > 0)."
      )
    }
  }
}
