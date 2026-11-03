/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

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
