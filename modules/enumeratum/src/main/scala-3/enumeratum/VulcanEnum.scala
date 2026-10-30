/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package enumeratum

import vulcan.Codec
import vulcan.internal.Names

trait VulcanEnum[A <: EnumEntry] { this: Enum[A] =>
  given vulcanCodec(using names: Names[A]): Codec[A] = Vulcan.enumCodec(this)
}
