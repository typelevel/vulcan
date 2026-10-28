/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import scodec.bits.ByteVector
import vulcan.generic.*
import vulcan.v2.Codec

@AvroAlias("FixedOtherAlias")
final case class FixedAvroAlias(bytes: ByteVector)

object FixedAvroAlias {
  implicit val codec: Codec[FixedAvroAlias] =
    deriveFixed(
      size = 1,
      encode = _.bytes,
      decode = bytes => Right(apply(bytes))
    )
}
