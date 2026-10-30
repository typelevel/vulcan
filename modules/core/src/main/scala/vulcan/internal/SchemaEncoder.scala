/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import io.circe.Json
import io.circe.JsonObject
import scala.collection.mutable
import vulcan.Schema

private[vulcan] object SchemaEncoder {

  /**
    * Convert `Schema` to `Json`, matching the Java Avro library for compatibility.
    *
    * Named schemas are defined where they first occur, and are referenced by name afterwards. The
    * namespace of a named schema is only included when it differs from the enclosing namespace.
    */
  def encode(schema: Schema): Json =
    encode(schema, None, mutable.Set.empty)

  private def encode(
    schema: Schema,
    namespace: Option[String],
    knownNames: mutable.Set[String]
  ): Json =
    schema match {
      case named: Schema.Named =>
        if (knownNames.add(named.fullName)) encodeNamed(named, namespace, knownNames)
        else Json.fromString(qualified(named.name, namespace))

      case ref: Schema.Ref =>
        Json.fromString(qualified(ref.name, namespace))

      case union: Schema.Union =>
        Json.fromValues(union.types.map(encode(_, namespace, knownNames)))

      case array: Schema.Array =>
        Json.fromFields(
          List(
            "type" -> Json.fromString("array"),
            "items" -> encode(array.items, namespace, knownNames)
          ) ++ encodeProps(array.props, SchemaReserved)
        )

      case map: Schema.Map =>
        Json.fromFields(
          List(
            "type" -> Json.fromString("map"),
            "values" -> encode(map.values, namespace, knownNames)
          ) ++ encodeProps(map.props, SchemaReserved)
        )

      case _: Schema.Null => primitive("null", schema.props)
      case _: Schema.Boolean => primitive("boolean", schema.props)
      case _: Schema.Int => primitive("int", schema.props)
      case _: Schema.Long => primitive("long", schema.props)
      case _: Schema.Float => primitive("float", schema.props)
      case _: Schema.Double => primitive("double", schema.props)
      case _: Schema.Bytes => primitive("bytes", schema.props)
      case _: Schema.String => primitive("string", schema.props)
    }

  private def encodeNamed(
    named: Schema.Named,
    namespace: Option[String],
    knownNames: mutable.Set[String]
  ): Json =
    named match {
      case record: Schema.Record =>
        val fields = record.fields.map(encodeField(_, record.name.namespace, knownNames))

        Json.fromFields(
          List("type" -> Json.fromString("record")) ++
            encodeName(record.name, namespace) ++
            record.doc.map(doc => "doc" -> Json.fromString(doc)) ++
            List("fields" -> Json.fromValues(fields)) ++
            encodeProps(record.props, SchemaReserved) ++
            aliases(record)
        )

      case enumeration: Schema.Enum =>
        Json.fromFields(
          List("type" -> Json.fromString("enum")) ++
            encodeName(enumeration.name, namespace) ++
            enumeration.doc.map(doc => "doc" -> Json.fromString(doc)) ++
            List("symbols" -> Json.fromValues(enumeration.symbols.map(Json.fromString))) ++
            enumeration.default.map(default => "default" -> Json.fromString(default)) ++
            encodeProps(enumeration.props, EnumReserved) ++
            aliases(enumeration)
        )

      case fixed: Schema.Fixed =>
        Json.fromFields(
          List("type" -> Json.fromString("fixed")) ++
            encodeName(fixed.name, namespace) ++
            fixed.doc.map(doc => "doc" -> Json.fromString(doc)) ++
            List("size" -> Json.fromInt(fixed.size)) ++
            encodeProps(fixed.props, SchemaReserved) ++
            aliases(fixed)
        )
    }

  private def encodeField(
    field: Schema.Field,
    namespace: Option[String],
    knownNames: mutable.Set[String]
  ): Json =
    Json.fromFields(
      List(
        "name" -> Json.fromString(field.name),
        "type" -> encode(field.schema, namespace, knownNames)
      ) ++
        field.doc.map(doc => "doc" -> Json.fromString(doc)) ++
        field.default.map(default => "default" -> default) ++
        order(field.order) ++
        fieldAliases(field.aliases) ++
        encodeProps(field.props, FieldReserved)
    )

  private def encodeName(name: Schema.Name, namespace: Option[String]): List[(String, Json)] =
    ("name" -> Json.fromString(name.name)) :: {
      name.namespace match {
        case Some(space) if !namespace.contains(space) =>
          List("namespace" -> Json.fromString(space))
        case None if namespace.isDefined => List("namespace" -> Json.fromString(""))
        case _ => Nil
      }
    }

  /** The name relative to the namespace, using the full name if the name is an Avro type name. */
  private def qualified(name: Schema.Name, namespace: Option[String]): String =
    if (name.namespace.isDefined && name.namespace == namespace && !TypeNames.contains(name.name))
      name.name
    else name.fullName

  private def aliases(named: Schema.Named): List[(String, Json)] = {
    val names =
      named.aliases
        .map(Schema.Name(_, named.name.namespace))
        .distinctBy(_.fullName)

    if (names.isEmpty) Nil
    else {
      val qualifiedNames = names.map(name => Json.fromString(qualified(name, named.name.namespace)))
      List("aliases" -> Json.fromValues(qualifiedNames))
    }
  }

  private def fieldAliases(aliases: Vector[String]): List[(String, Json)] =
    if (aliases.isEmpty) Nil
    else List("aliases" -> Json.fromValues(aliases.distinct.map(Json.fromString)))

  private def order(order: Schema.Field.Order): List[(String, Json)] =
    order match {
      case Schema.Field.Order.Ascending => Nil
      case Schema.Field.Order.Descending => List("order" -> Json.fromString("descending"))
      case Schema.Field.Order.Ignore => List("order" -> Json.fromString("ignore"))
    }

  private def primitive(typeName: String, props: JsonObject): Json =
    if (props.isEmpty) Json.fromString(typeName)
    else
      Json.fromFields(("type" -> Json.fromString(typeName)) :: encodeProps(props, SchemaReserved))

  private def encodeProps(props: JsonObject, reserved: Set[String]): List[(String, Json)] =
    props.toList.filterNot { case (key, _) => reserved.contains(key) }

  private[vulcan] val SchemaReserved: Set[String] =
    Set(
      "aliases",
      "doc",
      "fields",
      "items",
      "name",
      "namespace",
      "size",
      "symbols",
      "type",
      "values"
    )

  private[vulcan] val EnumReserved: Set[String] =
    SchemaReserved + "default"

  private[vulcan] val FieldReserved: Set[String] =
    Set("aliases", "default", "doc", "name", "order", "type")

  private val TypeNames: Set[String] =
    Set(
      "array",
      "boolean",
      "bytes",
      "double",
      "enum",
      "fixed",
      "float",
      "int",
      "long",
      "map",
      "null",
      "record",
      "string",
      "union"
    )
}
