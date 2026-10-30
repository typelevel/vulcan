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
      val schema = codec.schema.value
      val encoded = codec.encoder.value.encode(posInt).value
      val decoded = codec.decoder(schema).value.decode(encoded).value
      expect.same(posInt, decoded)
    }
  }

  test("Refined should fail for values not conforming to predicate") {
    forall { (nonPosInt: NonPosInt) =>
      val codec = Codec[PosInt]
      val schema = codec.schema.value
      val encoded = Codec[Int].encoder.value.encode(nonPosInt.value).value
      val error = codec.decoder(schema).value.decode(encoded).swap.map(_.message).value
      expect.eql(s"Predicate failed: ($nonPosInt > 0).", error)
    }
  }
}
