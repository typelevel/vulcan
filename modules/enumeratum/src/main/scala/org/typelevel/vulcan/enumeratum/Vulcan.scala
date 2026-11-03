/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.enumeratum

import enumeratum.Enum
import enumeratum.EnumEntry
import org.typelevel.vulcan.AvroException
import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.deriveEnum
import org.typelevel.vulcan.internal.Names

object Vulcan {
  def enumCodec[A <: EnumEntry](
    `enum`: Enum[A]
  )(implicit names: Names[A]): Codec[A] = {
    lazy val entries = `enum`.values.map(_.entryName).mkString(", ")
    val notFound = (name: String) => AvroException(s"$name is not a member of ${names.typeName} ($entries)")

    deriveEnum(
      symbols = `enum`.values.map(_.entryName),
      encode = _.entryName,
      decode = name => `enum`.withNameOption(name).toRight(notFound(name))
    )
  }
}
