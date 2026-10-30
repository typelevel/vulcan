/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import vulcan.Codec
import vulcan.generic.*

@AvroAlias("CaseClassOtherAlias")
@AvroAlias("CaseClassSecondAlias")
final case class CaseClassAvroAlias(
  @AvroAlias("otherValueAlias")
  @AvroAlias("secondValueAlias")
  value: Option[String]
)

object CaseClassAvroAlias {
  implicit val codec: Codec[CaseClassAvroAlias] = Codec.derive
}
