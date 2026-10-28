/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import vulcan.generic.*
import vulcan.v2.Codec

final case class CaseClassFieldAvroDoc(@AvroDoc("documentation") name: String)

object CaseClassFieldAvroDoc {
  implicit val codec: Codec[CaseClassFieldAvroDoc] =
    Codec.derive
}
