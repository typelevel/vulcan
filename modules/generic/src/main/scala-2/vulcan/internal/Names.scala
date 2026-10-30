/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

sealed abstract class Names[A] {
  def typeName: String

  def namespace: String

  def doc: Option[String]

  def aliases: Seq[String]
}

object Names {
  private final case class NamesImpl[A](
    override val typeName: String,
    override val namespace: String,
    override val doc: Option[String],
    override val aliases: Seq[String]
  ) extends Names[A]

  // Public since the macro expansion calls it at the call site
  def apply[A](
    typeName: String,
    namespace: String,
    doc: Option[String],
    aliases: Seq[String]
  ): Names[A] =
    NamesImpl(
      typeName = typeName,
      namespace = namespace,
      doc = doc,
      aliases = aliases
    )

  implicit def names[A]: Names[A] = macro NamesMacro.names[A]
}
