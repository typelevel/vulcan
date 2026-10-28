/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package enumeratum

import vulcan.internal.Names
import vulcan.v2.Codec

trait VulcanEnum[A <: EnumEntry] { this: Enum[A] =>
  given (using names: Names[A]): Codec[A] = Vulcan.enumCodec(this)
}
