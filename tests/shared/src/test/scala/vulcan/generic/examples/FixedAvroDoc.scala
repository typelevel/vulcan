/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import scodec.bits.ByteVector
import vulcan.Codec
import vulcan.generic.*

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
