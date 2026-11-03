/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*
import scodec.bits.ByteVector

@AvroDoc("Some documentation")
@AvroNamespace("org.typelevel.vulcan.generic.examples.overridden")
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
