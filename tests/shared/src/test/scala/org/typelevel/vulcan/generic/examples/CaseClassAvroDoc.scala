/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

@AvroDoc("documentation")
final case class CaseClassAvroDoc(value: Option[String])

object CaseClassAvroDoc {
  implicit val codec: Codec[CaseClassAvroDoc] =
    Codec.derive
}
