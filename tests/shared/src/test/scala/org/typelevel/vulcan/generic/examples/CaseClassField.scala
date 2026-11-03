/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

final case class CaseClassField(value: Int)

object CaseClassField {
  implicit val codec: Codec[CaseClassField] =
    Codec.derive

  implicit val caseClassFieldArbitrary: Arbitrary[CaseClassField] =
    Arbitrary(arbitrary[Int].map(CaseClassField(_)))
}
