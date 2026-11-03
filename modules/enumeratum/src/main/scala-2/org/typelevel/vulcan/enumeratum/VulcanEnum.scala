/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.enumeratum

import enumeratum.Enum
import enumeratum.EnumEntry
import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.internal.Names

trait VulcanEnum[A <: EnumEntry] { this: Enum[A] =>
  implicit def vulcanCodec(implicit names: Names[A]): Codec[A] =
    Vulcan.enumCodec(this)
}
