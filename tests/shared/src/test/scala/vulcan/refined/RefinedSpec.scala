/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.refined

import eu.timepit.refined.scalacheck.numeric.*
import eu.timepit.refined.types.numeric.NonPosInt
import eu.timepit.refined.types.numeric.PosInt
import munit.ScalaCheckSuite
import org.scalacheck.Prop.*
import vulcan.Codec

final class RefinedSpec extends ScalaCheckSuite with EitherValues {
  property("Refined should succeed for values conforming to predicate") {
    forAll { (posInt: PosInt) =>
      val codec = Codec[PosInt]
      val schema = codec.schema.value
      val encoded = codec.encoder.value.encode(posInt).value
      val decoded = codec.decoder(schema).value.decode(encoded).value
      decoded == posInt
    }
  }

  property("Refined should fail for values not conforming to predicate") {
    forAll { (nonPosInt: NonPosInt) =>
      val codec = Codec[PosInt]
      val schema = codec.schema.value
      val encoded = Codec[Int].encoder.value.encode(nonPosInt.value).value
      val error = codec.decoder(schema).value.decode(encoded).swap.map(_.message).value
      error == s"Predicate failed: ($nonPosInt > 0)."
    }
  }
}
