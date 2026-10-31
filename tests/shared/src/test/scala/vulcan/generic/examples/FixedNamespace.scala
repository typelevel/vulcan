/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import scodec.bits.ByteVector
import vulcan.Codec
import vulcan.generic.*

@AvroDoc("Some documentation")
@AvroNamespace("vulcan.generic.examples.overridden")
final case class FixedNamespace(bytes: ByteVector)
object FixedNamespace {
  implicit val codec: Codec[FixedNamespace] =
    deriveFixed(
      size = 1,
      encode = _.bytes,
      decode = bytes => Right(FixedNamespace(bytes))
    )

  implicit val fixedNamespaceArbitrary: Arbitrary[FixedNamespace] =
    Arbitrary {
      Gen
        .choose(Byte.MinValue, Byte.MaxValue)
        .map(byte => FixedNamespace(ByteVector(byte)))
    }
}
