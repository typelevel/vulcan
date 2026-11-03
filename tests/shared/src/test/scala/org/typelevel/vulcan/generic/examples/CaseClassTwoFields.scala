/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

final case class CaseClassTwoFields(name: String, age: Int)

object CaseClassTwoFields {
  implicit val codec: Codec[CaseClassTwoFields] =
    Codec.derive
}
