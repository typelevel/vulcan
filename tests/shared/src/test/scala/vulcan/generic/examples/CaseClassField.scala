/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import vulcan.Codec
import vulcan.generic.*

final case class CaseClassField(value: Int)

object CaseClassField {
  implicit val codec: Codec[CaseClassField] =
    Codec.derive

  implicit val caseClassFieldArbitrary: Arbitrary[CaseClassField] =
    Arbitrary(arbitrary[Int].map(CaseClassField(_)))
}
