/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

case class CaseClassOptionOfSumType(field1: Option[SealedTraitCaseClassCustom])

object CaseClassOptionOfSumType {
  implicit val codec: Codec[CaseClassOptionOfSumType] = Codec.derive
}
