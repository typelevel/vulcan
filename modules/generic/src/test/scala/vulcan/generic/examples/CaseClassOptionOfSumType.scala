/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import vulcan.generic.*
import vulcan.v2.Codec

case class CaseClassOptionOfSumType(field1: Option[SealedTraitCaseClassCustom])

object CaseClassOptionOfSumType {
  implicit val codec: Codec[CaseClassOptionOfSumType] = Codec.derive
}
