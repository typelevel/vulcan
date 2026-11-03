/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

sealed trait SealedTraitNestedUnion

object SealedTraitNestedUnion {
  implicit val codec: Codec[SealedTraitNestedUnion] =
    Codec.derive
}

final case class NestedUnionInSealedTrait(value: Option[Int]) extends SealedTraitNestedUnion

object NestedUnionInSealedTrait {
  implicit final val codec: Codec[NestedUnionInSealedTrait] =
    Codec.option[Int].imap(apply)(_.value)
}
