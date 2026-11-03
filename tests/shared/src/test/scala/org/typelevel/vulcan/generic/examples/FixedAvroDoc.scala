/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*
import scodec.bits.ByteVector

@AvroDoc("Some documentation")
final case class FixedAvroDoc(bytes: ByteVector)

object FixedAvroDoc {
  implicit val codec: Codec[FixedAvroDoc] =
    deriveFixed(
      size = 1,
      encode = _.bytes,
      decode = bytes => Right(apply(bytes))
    )
}
