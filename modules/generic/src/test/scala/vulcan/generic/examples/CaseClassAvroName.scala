/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import vulcan.generic.*
import vulcan.v2.Codec

@AvroName("CaseClassOtherName")
final case class CaseClassAvroName(
  @AvroName("otherValue")
  value: Option[String]
)

object CaseClassAvroName {
  implicit val codec: Codec[CaseClassAvroName] = Codec.derive
}
