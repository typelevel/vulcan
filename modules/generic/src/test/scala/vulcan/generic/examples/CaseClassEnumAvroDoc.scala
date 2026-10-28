/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import vulcan.generic.*
import vulcan.v2.Codec

@AvroDoc("documentation")
final case class CaseClassEnumAvroDoc(value: Option[String])

object CaseClassEnumAvroDoc {
  implicit val codec: Codec[CaseClassEnumAvroDoc] =
    deriveEnum(
      symbols = List("first"),
      encode = _ => "first",
      decode = _ => Right(CaseClassEnumAvroDoc(None))
    )
}
