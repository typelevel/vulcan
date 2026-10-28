/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import scodec.bits.ByteVector
import vulcan.generic.*
import vulcan.v2.Codec

@AvroName("FixedOtherName")
final case class FixedAvroName(bytes: ByteVector)

object FixedAvroName {
  implicit val codec: Codec[FixedAvroName] =
    deriveFixed(
      size = 1,
      encode = _.bytes,
      decode = bytes => Right(apply(bytes))
    )
}
