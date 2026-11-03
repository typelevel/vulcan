/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

sealed trait SealedTraitCaseClass

final case class CaseClassInSealedTrait(value: Int) extends SealedTraitCaseClass

object CaseClassInSealedTrait {
  implicit val codec: Codec[CaseClassInSealedTrait] =
    Codec.derive
}

object SealedTraitCaseClass {
  implicit val codec: Codec[SealedTraitCaseClass] =
    Codec.derive
}
