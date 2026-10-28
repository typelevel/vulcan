/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.internal

import scala.collection.mutable
import vulcan.v2.Schema

private[v2] object SchemaNames {

  /** The named schemas defined in the schema, by full name, in the order they are defined. */
  def definitions(schema: Schema): collection.Map[String, Schema.Named] = {
    val definitions = mutable.LinkedHashMap.empty[String, Schema.Named]

    def collect(schema: Schema): Unit =
      schema match {
        case named: Schema.Named if !definitions.contains(named.fullName) =>
          definitions.update(named.fullName, named)
          named match {
            case record: Schema.Record => record.fields.foreach(field => collect(field.schema))
            case _ => ()
          }

        case array: Schema.Array => collect(array.items)
        case map: Schema.Map => collect(map.values)
        case union: Schema.Union => union.types.foreach(collect)
        case _ => ()
      }

    collect(schema)
    definitions
  }

  /** Resolve as full name if it includes a dot, otherwise relative to the namespace. */
  def resolve(name: String, namespace: Option[String]): Schema.Name =
    name.lastIndexOf('.') match {
      case -1 => Schema.Name(name, namespace)
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
      case ref: Schema.Ref => ref.fullName
      case _: Schema.Null => "null"
      case _: Schema.Boolean => "boolean"
      case _: Schema.Int => "int"
      case _: Schema.Long => "long"
      case _: Schema.Float => "float"
      case _: Schema.Double => "double"
      case _: Schema.Bytes => "bytes"
      case _: Schema.String => "string"
      case _: Schema.Array => "array"
      case _: Schema.Map => "map"
      case _: Schema.Union => "union"
    }

  /** The type of the schema, for use in error messages. */
  def typeName(schema: Schema): String =
    schema match {
      case _: Schema.Record => "record"
      case _: Schema.Enum => "enum"
      case _: Schema.Fixed => "fixed"
      case _ => unionKey(schema)
    }

  /** The type of the schema, with full names of named schemas, for use in error messages. */
  def describe(schema: Schema): String =
    schema match {
      case named: Schema.Named => s"${typeName(named)} ${named.fullName}"
      case _ => typeName(schema)
    }

  val PrimitiveNames: Set[String] =
    Set("boolean", "bytes", "double", "float", "int", "long", "null", "string")

  private def isNameStart(char: Char): Boolean =
    Character.isLetter(char) || char == '_'

  private def isNamePart(char: Char): Boolean =
    Character.isLetterOrDigit(char) || char == '_'
}
