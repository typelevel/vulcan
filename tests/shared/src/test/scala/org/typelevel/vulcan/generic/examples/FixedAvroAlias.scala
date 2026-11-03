/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*
import scodec.bits.ByteVector

@AvroAlias("FixedOtherAlias")
@AvroAlias("FixedSecondAlias")
final case class FixedAvroAlias(bytes: ByteVector)

object FixedAvroAlias {
  implicit val codec: Codec[FixedAvroAlias] =
    deriveFixed(
      size = 1,
      encode = _.bytes,
      decode = bytes => Right(apply(bytes))
    )
}
