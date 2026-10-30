/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Show
import io.circe.Json
import io.circe.JsonObject
import vulcan.AvroException.InvalidSchema
import vulcan.internal.JsonPrinter
import vulcan.internal.SchemaDecoder
import vulcan.internal.SchemaEncoder

/** Immutable Avro schema. Not validated, so not necessarily a valid Avro schema. */
sealed abstract class Schema {

  /** Custom properties in insertion order, including `logicalType` and its parameters. */
  def props: JsonObject

  /** The logical type, if there is one which is valid for the underlying type. */
  final def logicalType: Option[LogicalType] =
    LogicalType.of(this)

  /** Returns `true` for `null`, and for unions including `null`. */
  final def isNullable: Boolean =
    this match {
      case _: Schema.Null => true
      case union: Schema.Union => union.types.exists(_.isInstanceOf[Schema.Null])
      case _ => false
    }

  /** The schema as `Json`. Named schemas are referenced by name after their definition. */
  final def toJson: Json =
    SchemaEncoder.encode(this)

  /** The schema as JSON without whitespace. */
  final override def toString: String =
    JsonPrinter.print(toJson)
}

object Schema {

  /** Parses, decodes and validates the provided schema JSON `String`. */
  def parse(json: Predef.String): Either[AvroException, Schema] =
    io.circe.jawn
      .parse(json)
      .left
      .map(failure => new InvalidSchema(s"Invalid JSON: ${failure.message}", Some(failure)))
      .flatMap(fromJson)

  /** Decodes and validates the provided schema `Json`. */
  def fromJson(json: Json): Either[AvroException, Schema] =
    SchemaDecoder.decode(json)

  implicit final val schemaShow: Show[Schema] =
    Show.fromToString

  sealed abstract class Null extends Schema {
    final def withProps(props: JsonObject): Null =
      if (props.isEmpty) Null else NullImpl(props)
  }

  /** The `null` schema without properties. */
  object Null extends Null {
    override def props: JsonObject =
      JsonObject.empty
  }

  private final case class NullImpl(props: JsonObject) extends Null

  sealed abstract class Boolean extends Schema {
    final def withProps(props: JsonObject): Boolean =
      if (props.isEmpty) Boolean else BooleanImpl(props)
  }

  /** The `boolean` schema without properties. */
  object Boolean extends Boolean {
    override def props: JsonObject =
      JsonObject.empty
  }

  private final case class BooleanImpl(props: JsonObject) extends Boolean

  sealed abstract class Int extends Schema {
    final def withLogicalType(logicalType: LogicalType): Int =
      withProps(addLogicalType(props, logicalType))

    final def withProps(props: JsonObject): Int =
      if (props.isEmpty) Int else IntImpl(props)
  }

  /** The `int` schema without properties. */
  object Int extends Int {
    override def props: JsonObject =
      JsonObject.empty
  }

  private final case class IntImpl(props: JsonObject) extends Int

  sealed abstract class Long extends Schema {
    final def withLogicalType(logicalType: LogicalType): Long =
      withProps(addLogicalType(props, logicalType))

    final def withProps(props: JsonObject): Long =
      if (props.isEmpty) Long else LongImpl(props)
  }

  /** The `long` schema without properties. */
  object Long extends Long {
    override def props: JsonObject =
      JsonObject.empty
  }

  private final case class LongImpl(props: JsonObject) extends Long

  sealed abstract class Float extends Schema {
    final def withProps(props: JsonObject): Float =
      if (props.isEmpty) Float else FloatImpl(props)
  }

  /** The `float` schema without properties. */
  object Float extends Float {
    override def props: JsonObject =
      JsonObject.empty
  }

  private final case class FloatImpl(props: JsonObject) extends Float

  sealed abstract class Double extends Schema {
    final def withProps(props: JsonObject): Double =
      if (props.isEmpty) Double else DoubleImpl(props)
  }

  /** The `double` schema without properties. */
  object Double extends Double {
    override def props: JsonObject =
      JsonObject.empty
  }

  private final case class DoubleImpl(props: JsonObject) extends Double

  sealed abstract class Bytes extends Schema {
    final def withLogicalType(logicalType: LogicalType): Bytes =
      withProps(addLogicalType(props, logicalType))

    final def withProps(props: JsonObject): Bytes =
      if (props.isEmpty) Bytes else BytesImpl(props)
  }

  /** The `bytes` schema without properties. */
  object Bytes extends Bytes {
    override def props: JsonObject =
      JsonObject.empty
  }

  private final case class BytesImpl(props: JsonObject) extends Bytes

  sealed abstract class String extends Schema {
    final def withLogicalType(logicalType: LogicalType): String =
      withProps(addLogicalType(props, logicalType))

    final def withProps(props: JsonObject): String =
      if (props.isEmpty) String else StringImpl(props)
  }

  /** The `string` schema without properties. */
  object String extends String {
    override def props: JsonObject =
      JsonObject.empty
  }

  private final case class StringImpl(props: JsonObject) extends String

  /** Named schema: a record, enum, or fixed. */
  sealed abstract class Named extends Schema {
    def name: Name

    def doc: Option[Predef.String]

    /** Aliases, either full names or names relative to the namespace of the schema. */
    def aliases: Vector[Predef.String]

    final def fullName: Predef.String =
      name.fullName
  }

  sealed abstract class Record extends Named {
    def fields: Vector[Field]

    def withFields(fields: Vector[Field]): Record

    def withDoc(doc: Option[Predef.String]): Record

    def withAliases(aliases: Vector[Predef.String]): Record

    def withProps(props: JsonObject): Record
  }

  object Record {
    def apply(name: Name, fields: Vector[Field]): Record =
      RecordImpl(name, fields, None, Vector.empty, JsonObject.empty)
  }

  private final case class RecordImpl(
    name: Name,
    fields: Vector[Field],
    doc: Option[Predef.String],
    aliases: Vector[Predef.String],
    props: JsonObject
  ) extends Record {
    override def withFields(fields: Vector[Field]): Record =
      copy(fields = fields)

    override def withDoc(doc: Option[Predef.String]): Record =
      copy(doc = doc)

    override def withAliases(aliases: Vector[Predef.String]): Record =
      copy(aliases = aliases)

    override def withProps(props: JsonObject): Record =
      copy(props = props)
  }

  sealed abstract class Enum extends Named {
    def symbols: Vector[Predef.String]

    def default: Option[Predef.String]

    def withSymbols(symbols: Vector[Predef.String]): Enum

    def withDefault(default: Option[Predef.String]): Enum

    def withDoc(doc: Option[Predef.String]): Enum

    def withAliases(aliases: Vector[Predef.String]): Enum

    def withProps(props: JsonObject): Enum
  }

  object Enum {
    def apply(name: Name, symbols: Vector[Predef.String]): Enum =
      EnumImpl(name, symbols, None, None, Vector.empty, JsonObject.empty)
  }

  private final case class EnumImpl(
    name: Name,
    symbols: Vector[Predef.String],
    default: Option[Predef.String],
    doc: Option[Predef.String],
    aliases: Vector[Predef.String],
    props: JsonObject
  ) extends Enum {
    override def withSymbols(symbols: Vector[Predef.String]): Enum =
      copy(symbols = symbols)

    override def withDefault(default: Option[Predef.String]): Enum =
      copy(default = default)

    override def withDoc(doc: Option[Predef.String]): Enum =
      copy(doc = doc)

    override def withAliases(aliases: Vector[Predef.String]): Enum =
      copy(aliases = aliases)

    override def withProps(props: JsonObject): Enum =
      copy(props = props)
  }

  sealed abstract class Fixed extends Named {
    def size: scala.Int

    def withSize(size: scala.Int): Fixed

    def withLogicalType(logicalType: LogicalType): Fixed

    def withDoc(doc: Option[Predef.String]): Fixed

    def withAliases(aliases: Vector[Predef.String]): Fixed

    def withProps(props: JsonObject): Fixed
  }

  object Fixed {
    def apply(name: Name, size: scala.Int): Fixed =
      FixedImpl(name, size, None, Vector.empty, JsonObject.empty)
  }

  private final case class FixedImpl(
    name: Name,
    size: scala.Int,
    doc: Option[Predef.String],
    aliases: Vector[Predef.String],
    props: JsonObject
  ) extends Fixed {
    override def withSize(size: scala.Int): Fixed =
      copy(size = size)

    override def withLogicalType(logicalType: LogicalType): Fixed =
      withProps(addLogicalType(props, logicalType))

    override def withDoc(doc: Option[Predef.String]): Fixed =
      copy(doc = doc)

    override def withAliases(aliases: Vector[Predef.String]): Fixed =
      copy(aliases = aliases)

    override def withProps(props: JsonObject): Fixed =
      copy(props = props)
  }

  sealed abstract class Array extends Schema {
    def items: Schema

    def withItems(items: Schema): Array

    def withProps(props: JsonObject): Array
  }

  object Array {
    def apply(items: Schema): Array =
      ArrayImpl(items, JsonObject.empty)
  }

  private final case class ArrayImpl(items: Schema, props: JsonObject) extends Array {
    override def withItems(items: Schema): Array =
      copy(items = items)

    override def withProps(props: JsonObject): Array =
      copy(props = props)
  }

  sealed abstract class Map extends Schema {
    def values: Schema

    def withValues(values: Schema): Map

    def withProps(props: JsonObject): Map
  }

  object Map {
    def apply(values: Schema): Map =
      MapImpl(values, JsonObject.empty)
  }

  private final case class MapImpl(values: Schema, props: JsonObject) extends Map {
    override def withValues(values: Schema): Map =
      copy(values = values)

    override def withProps(props: JsonObject): Map =
      copy(props = props)
  }

  /** Union of schemas. Unions cannot have properties. */
  sealed abstract class Union extends Schema {
    def types: Vector[Schema]

    def withTypes(types: Vector[Schema]): Union
  }

  object Union {
    def apply(types: Vector[Schema]): Union =
      UnionImpl(types)
  }

  private final case class UnionImpl(types: Vector[Schema]) extends Union {
    override def props: JsonObject =
      JsonObject.empty

    override def withTypes(types: Vector[Schema]): Union =
      copy(types = types)
  }

  /** Reference to an enclosing named schema, used for recursive schemas. */
  sealed abstract class Ref extends Schema {
    def name: Name

    final def fullName: Predef.String =
      name.fullName
  }

  object Ref {
    def apply(name: Name): Ref =
      RefImpl(name)
  }

  private final case class RefImpl(name: Name) extends Ref {
    override def props: JsonObject =
      JsonObject.empty
  }

  sealed abstract class Field {
    def name: Predef.String

    def schema: Schema

    def doc: Option[Predef.String]

    /** The default value, where `Some(Json.Null)` is a `null` default. */
    def default: Option[Json]

    def order: Field.Order

    def aliases: Vector[Predef.String]

    def props: JsonObject

    def withSchema(schema: Schema): Field

    def withDoc(doc: Option[Predef.String]): Field

    def withDefault(default: Option[Json]): Field

    def withOrder(order: Field.Order): Field

    def withAliases(aliases: Vector[Predef.String]): Field

    def withProps(props: JsonObject): Field
  }

  object Field {
    def apply(name: Predef.String, schema: Schema): Field =
      FieldImpl(name, schema, None, None, Order.Ascending, Vector.empty, JsonObject.empty)

    sealed abstract class Order extends Product with Serializable

    object Order {
      case object Ascending extends Order

      case object Descending extends Order

      case object Ignore extends Order
    }
  }

  private final case class FieldImpl(
    name: Predef.String,
    schema: Schema,
    doc: Option[Predef.String],
    default: Option[Json],
    order: Field.Order,
    aliases: Vector[Predef.String],
    props: JsonObject
  ) extends Field {
    override def withSchema(schema: Schema): Field =
      copy(schema = schema)

    override def withDoc(doc: Option[Predef.String]): Field =
      copy(doc = doc)

    override def withDefault(default: Option[Json]): Field =
      copy(default = default)

    override def withOrder(order: Field.Order): Field =
      copy(order = order)

    override def withAliases(aliases: Vector[Predef.String]): Field =
      copy(aliases = aliases)

    override def withProps(props: JsonObject): Field =
      copy(props = props)
  }

  /** Name of a named schema, where the namespace is `None` for the null namespace. */
  sealed abstract class Name {
    def name: Predef.String

    def namespace: Option[Predef.String]

    final def fullName: Predef.String =
      namespace.fold(name)(namespace => s"$namespace.$name")
  }

  object Name {

    /** Returns a new name in the null namespace, unless the name is a full name with dots. */
    def apply(name: Predef.String): Name =
      apply(name, None)

    /**
      * Returns a new name, where a name with dots is a full name, ignoring the namespace,
      * and an empty namespace is the null namespace.
      */
    def apply(name: Predef.String, namespace: Option[Predef.String]): Name =
      name.lastIndexOf('.') match {
        case -1 => NameImpl(name, namespace.filter(_.nonEmpty))
        case lastDot => apply(name.substring(lastDot + 1), Some(name.substring(0, lastDot)))
      }
  }

  private final case class NameImpl(
    name: Predef.String,
    namespace: Option[Predef.String]
  ) extends Name {
    override def toString: Predef.String = s"Name($fullName)"
  }

  private def addLogicalType(props: JsonObject, logicalType: LogicalType): JsonObject =
    logicalType.params.toIterable.foldLeft(
      props.add("logicalType", Json.fromString(logicalType.name))
    ) { case (result, (key, value)) => result.add(key, value) }
}
