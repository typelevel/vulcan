/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

sealed trait SealedTraitCaseObject

case object CaseObjectInSealedTrait extends SealedTraitCaseObject

object SealedTraitCaseObject {
  implicit val codec: Codec[SealedTraitCaseObject] =
    Codec.derive
}
