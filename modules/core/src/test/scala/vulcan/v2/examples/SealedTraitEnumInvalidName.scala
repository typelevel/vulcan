/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.examples

import vulcan.AvroError
import vulcan.v2.Codec

sealed trait `-SealedTraitEnumInvalidName`

object `-SealedTraitEnumInvalidName` {
  implicit val codec: Codec[`-SealedTraitEnumInvalidName`] =
    Codec.enumeration(
      "-SealedTraitEnumInvalidName",
      symbols = Seq("value"),
      encode = _ => "value",
      decode = _ => Left(AvroError("error")),
      namespace = ""
    )
}

case object FirstInSealedTraitEnumInvalidName extends `-SealedTraitEnumInvalidName`

case object SecondInSealedTraitEnumInvalidName extends `-SealedTraitEnumInvalidName`
