/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.internal

import vulcan.v2.Schema

private[v2] object SchemaNames {

  /** Resolve as full name if it includes a dot, otherwise relative to the namespace. */
  def resolve(name: String, namespace: Option[String]): Schema.Name =
    name.lastIndexOf('.') match {
      case -1      => Schema.Name(name, namespace)
      case lastDot => Schema.Name(name.substring(lastDot + 1), Some(name.substring(0, lastDot)))
    }

  /** The reason the name is invalid, if it is invalid, matching the Java Avro library. */
  def invalid(name: String): Option[String] =
    if (name.isEmpty) Some("cannot be empty")
    else if (!isNameStart(name.charAt(0))) Some("must start with a letter or underscore")
    else if (!name.forall(isNamePart)) Some("can only contain letters, digits, and underscores")
    else None

  /** The key for distinguishing types in unions, being the full name for named schemas. */
  def unionKey(schema: Schema): String =
    schema match {
      case named: Schema.Named => named.fullName
      case ref: Schema.Ref     => ref.fullName
      case _: Schema.Null      => "null"
      case _: Schema.Boolean   => "boolean"
      case _: Schema.Int       => "int"
      case _: Schema.Long      => "long"
      case _: Schema.Float     => "float"
      case _: Schema.Double    => "double"
      case _: Schema.Bytes     => "bytes"
      case _: Schema.String    => "string"
      case _: Schema.Array     => "array"
      case _: Schema.Map       => "map"
      case _: Schema.Union     => "union"
    }

  val PrimitiveNames: Set[String] =
    Set("boolean", "bytes", "double", "float", "int", "long", "null", "string")

  private def isNameStart(char: Char): Boolean =
    Character.isLetter(char) || char == '_'

  private def isNamePart(char: Char): Boolean =
    Character.isLetterOrDigit(char) || char == '_'
}
