/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

@AvroName("CaseClassOtherName")
final case class CaseClassAvroName(
  @AvroName("otherValue")
  value: Option[String]
)

object CaseClassAvroName {
  implicit val codec: Codec[CaseClassAvroName] = Codec.derive
}
