/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import vulcan.generic.*
import vulcan.v2.Codec

sealed trait SealedTraitCaseObject

case object CaseObjectInSealedTrait extends SealedTraitCaseObject

object SealedTraitCaseObject {
  implicit val codec: Codec[SealedTraitCaseObject] =
    Codec.derive
}
