/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package enumeratum.values

import vulcan.AvroException
import vulcan.Codec
import vulcan.generic.deriveEnum
import vulcan.internal.Names

object Vulcan {
  def codec[ValueType, EntryType <: ValueEnumEntry[ValueType]](
    `enum`: ValueEnum[ValueType, EntryType]
  )(implicit
    codec: Codec[ValueType],
    names: Names[EntryType]
  ): Codec[EntryType] = {
    lazy val entries = `enum`.values.map(_.value).mkString(", ")
    val notFound = (value: ValueType) =>
      AvroException(s"$value is not a member of ${names.typeName} ($entries)")

    codec.imapError(value => `enum`.withValueOpt(value).toRight(notFound(value)))(_.value)
  }

  def enumCodec[EntryType <: ValueEnumEntry[String]](
    `enum`: ValueEnum[String, EntryType]
  )(implicit names: Names[EntryType]): Codec[EntryType] = {
    lazy val entries = `enum`.values.map(_.value).mkString(", ")
    val notFound = (value: String) => AvroException(s"$value is not a member of ${names.typeName} ($entries)")

    deriveEnum(
      symbols = `enum`.values.map(_.value),
      encode = _.value,
      decode = value => `enum`.withValueOpt(value).toRight(notFound(value))
    )
  }
}
