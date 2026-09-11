/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import vulcan.internal.NamesMacro

sealed abstract class Names[A] {
  def typeName: String

  def namespace: String

  def doc: Option[String]

  def aliasOf: Option[String]
}

object Names {
  private final case class NamesImpl[A](
    override val typeName: String,
    override val namespace: String,
    override val doc: Option[String],
    override val aliasOf: Option[String]
  ) extends Names[A]

  private[internal] def apply[A](
    typeName: String,
    namespace: String,
    doc: Option[String],
    aliasOf: Option[String]
  ): Names[A] =
    NamesImpl(
      typeName = typeName,
      namespace = namespace,
      doc = doc,
      aliasOf = aliasOf
    )

  inline given [A]: Names[A] = NamesMacro.names[A]
}
