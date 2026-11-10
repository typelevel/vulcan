/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.internal

import cats.syntax.all.*
import io.circe.Json
import java.nio.charset.StandardCharsets
import org.typelevel.vulcan.AvroException
import org.typelevel.vulcan.AvroException.IncompatibleWriterSchema
import org.typelevel.vulcan.AvroException.InvalidValue
import org.typelevel.vulcan.AvroValue
import org.typelevel.vulcan.LogicalType
import org.typelevel.vulcan.Schema
import scala.collection.mutable
import scodec.bits.ByteVector

/**
  * Resolves values of a schema into values of a target schema, following Avro's schema resolution,
  * as if values of the schema were read using the target schema as the reader schema.
  *
  * Values cannot be resolved if reading values of the schema using the target schema could fail,
  * for example if a field of the target schema is missing in the schema and has no default, or if
  * the schemas have different logical types. Unknown logical types are ignored.
  */
private[vulcan] object ValueResolver {

  /** Returns a function resolving values of the schema into values of the target schema. */
  def apply(
    schema: Schema,
    target: Schema
  ): Either[AvroException, AvroValue => Either[AvroException, AvroValue]] =
    SchemaValidator.validate(target, validateDefaults = true) >>
      new Compilation(SchemaNames.definitions(schema), SchemaNames.definitions(target))
        .compile(schema, target)
        .map { node => value =>
          try Right(node.resolve(value))
          catch {
            case e: AvroException => Left(e)
          }
        }

  private sealed abstract class Node {

    /** Resolves the value, throwing `AvroException` for values which are not of the schema. */
    def resolve(value: AvroValue): AvroValue
  }

  private final class Compilation(
    definitions: collection.Map[String, Schema.Named],
    targetDefinitions: collection.Map[String, Schema.Named]
  ) {
    private val compiled = mutable.Map.empty[(String, String), Reference]

    def compile(schema: Schema, target: Schema): Either[AvroException, Node] =
      (definition(schema, definitions), definition(target, targetDefinitions)) match {
        case (union: Schema.Union, targetUnion: Schema.Union) =>
          union.types
            .traverse(branch => unionType(branch, targetUnion))
            .map(types => new UnionToUnionNode(types.toArray))

        case (union: Schema.Union, target) =>
          union.types.traverse(compile(_, target)).map(nodes => new FromUnionNode(nodes.toArray))

        case (schema, targetUnion: Schema.Union) =>
          unionType(schema, targetUnion).map { case (index, node) => new ToUnionNode(index, node) }

        case (schema, target) =>
          compileType(schema, target)
      }

    /** The index of the target union type for values of the schema, and the node resolving values. */
    private def unionType(schema: Schema, targetUnion: Schema.Union): Either[AvroException, (Int, Node)] = {
      val resolved = definition(schema, definitions)
      val types = targetUnion.types.map(definition(_, targetDefinitions))

      def find(matches: Schema => Boolean): Option[Int] =
        Some(types.indexWhere(matches)).filter(_ >= 0)

      val index =
        resolved match {
          case named: Schema.Named =>
            find(sameNamedType(named, _)(_.fullName)).orElse(find(sameNamedType(named, _)(_.name)))
          case _ =>
            find(SchemaNames.typeName(_) == SchemaNames.typeName(resolved))
              .orElse(find(isPromotion(resolved, _)))
        }

      index match {
        case Some(index) =>
          compile(resolved, types(index)).tupleLeft(index)
        case None =>
          Left(new IncompatibleWriterSchema(s"Missing type for ${SchemaNames.describe(resolved)} in union"))
      }
    }

    private def compileType(schema: Schema, target: Schema): Either[AvroException, Node] =
      (schema, target) match {
        case (record: Schema.Record, targetRecord: Schema.Record) if sameName(record, targetRecord) =>
          compileNamed(record, targetRecord)(compileRecord(record, targetRecord))

        case (enumeration: Schema.Enum, targetEnum: Schema.Enum) if sameName(enumeration, targetEnum) =>
          compileNamed(enumeration, targetEnum)(compileEnum(enumeration, targetEnum))

        case (fixed: Schema.Fixed, targetFixed: Schema.Fixed) if sameName(fixed, targetFixed) =>
          if (fixed.size != targetFixed.size)
            Left(
              new IncompatibleWriterSchema(
                s"Fixed ${fixed.fullName} with size ${fixed.size} has size ${targetFixed.size} in writer schema"
              )
            )
          else sameLogicalType(fixed, targetFixed).as(IdentityNode)

        case (array: Schema.Array, targetArray: Schema.Array) =>
          compile(array.items, targetArray.items).map(new ArrayNode(_))

        case (map: Schema.Map, targetMap: Schema.Map) =>
          compile(map.values, targetMap.values).map(new MapNode(_))

        case (_: Schema.Named, _) | (_, _: Schema.Named) | (_: Schema.Array, _) | (_: Schema.Map, _) =>
          Left(mismatch(schema, target))

        case _ if SchemaNames.typeName(schema) == SchemaNames.typeName(target) =>
          sameLogicalType(schema, target).as(IdentityNode)

        case _ if isPromotion(schema, target) =>
          noLogicalTypes(schema, target).as(new PromotionNode(target))

        case _ =>
          Left(mismatch(schema, target))
      }

    /** Compiles a named schema once, so recursive schemas refer to the node being compiled. */
    private def compileNamed(named: Schema.Named, target: Schema.Named)(
      compile: => Either[AvroException, Node]
    ): Either[AvroException, Node] = {
      val key = (named.fullName, target.fullName)
      compiled.get(key) match {
        case Some(reference) =>
          Right(reference.resolved)

        case None =>
          val reference = new Reference
          compiled.update(key, reference)
          compile.map { node =>
            reference.node = node
            node
          }
      }
    }

    /**
      * Compiles a record, where target fields use the value of the field with the same name or an
      * alias of the target field, or else the default of the target field.
      */
    private def compileRecord(record: Schema.Record, target: Schema.Record): Either[AvroException, Node] =
      target.fields
        .traverse { targetField =>
          (targetField.name +: targetField.aliases).iterator
            .map(name => record.fields.indexWhere(_.name == name))
            .find(_ >= 0) match {
            case Some(index) =>
              compile(record.fields(index).schema, targetField.schema).map(FieldSource.Value(index, _))

            case None =>
              targetField.default match {
                case Some(default) =>
                  decodeDefault(targetField.schema, default).map(FieldSource.Default(_))
                case None =>
                  Left(
                    new IncompatibleWriterSchema(
                      s"""Field "${targetField.name}" of record ${target.fullName} in writer schema """ +
                        "is missing in the schema and has no default"
                    )
                  )
              }
          }
        }
        .map(sources => new RecordNode(record.fullName, record.fields.size, sources.toArray))

    /** Compiles an enum, where symbols missing in the target enum use the default of the target enum. */
    private def compileEnum(enumeration: Schema.Enum, target: Schema.Enum): Either[AvroException, Node] =
      enumeration.symbols
        .traverse { symbol =>
          if (target.symbols.contains(symbol)) Right(symbol -> symbol)
          else
            target.default
              .map(symbol -> _)
              .toRight(
                new IncompatibleWriterSchema(
                  s"Symbol $symbol of enum ${enumeration.fullName} is missing in writer schema, which has no default"
                )
              )
        }
        .map(symbols => new EnumNode(enumeration.fullName, symbols.toMap))

    /** Decodes the default of a field, where defaults of unions use the first type matching the default. */
    private def decodeDefault(schema: Schema, default: Json): Either[AvroException, AvroValue] = {
      def invalid: Either[AvroException, AvroValue] =
        Left(
          new IncompatibleWriterSchema(s"Invalid default ${default.noSpaces} for $schema in writer schema")
        )

      definition(schema, targetDefinitions) match {
        case _: Schema.Null =>
          if (default.isNull) Right(AvroValue.Null) else invalid
        case _: Schema.Boolean =>
          default.asBoolean.map(AvroValue.Boolean(_)).fold(invalid)(Right(_))
        case _: Schema.Int =>
          SchemaValidator.intValue(default).map(AvroValue.Int(_)).fold(invalid)(Right(_))
        case _: Schema.Long =>
          SchemaValidator.longValue(default).map(AvroValue.Long(_)).fold(invalid)(Right(_))
        case _: Schema.Float =>
          number(default).map(double => AvroValue.Float(double.toFloat)).fold(invalid)(Right(_))
        case _: Schema.Double =>
          number(default).map(AvroValue.Double(_)).fold(invalid)(Right(_))
        case _: Schema.Bytes =>
          default.asString.map(string => AvroValue.Bytes(bytes(string))).fold(invalid)(Right(_))
        case _: Schema.String =>
          default.asString.map(AvroValue.String(_)).fold(invalid)(Right(_))
        case enumeration: Schema.Enum =>
          default.asString.filter(enumeration.symbols.contains).map(AvroValue.Enum(_)).fold(invalid)(Right(_))
        case fixed: Schema.Fixed =>
          default.asString
            .map(bytes)
            .filter(_.size == fixed.size.toLong)
            .map(AvroValue.Fixed(_))
            .fold(invalid)(Right(_))
        case array: Schema.Array =>
          default.asArray.fold(invalid)(_.traverse(decodeDefault(array.items, _)).map(AvroValue.Array(_)))
        case map: Schema.Map =>
          default.asObject.fold(invalid) { obj =>
            obj.toVector
              .traverse { case (key, value) => decodeDefault(map.values, value).tupleLeft(key) }
              .map(AvroValue.Map(_))
          }
        case record: Schema.Record =>
          default.asObject.fold(invalid) { obj =>
            record.fields
              .traverse { field =>
                obj(field.name).orElse(field.default).fold(invalid)(decodeDefault(field.schema, _))
              }
              .map(AvroValue.Record(_))
          }
        case union: Schema.Union =>
          union.types.iterator.zipWithIndex
            .map { case (schema, index) => decodeDefault(schema, default).map(AvroValue.Union(index, _)) }
            .find(_.isRight)
            .getOrElse(invalid)
        case _: Schema.Ref =>
          invalid
      }
    }
  }

  private sealed abstract class FieldSource

  private object FieldSource {

    /** Uses the value of the field with the index in the record. */
    final case class Value(index: Int, node: Node) extends FieldSource

    /** Uses the default of the target field. */
    final case class Default(value: AvroValue) extends FieldSource
  }

  /** A named schema, which is being compiled if references are recursive. */
  private final class Reference extends Node {
    var node: Node = null

    def resolved: Node =
      if (node != null) node else this

    override def resolve(value: AvroValue): AvroValue =
      node.resolve(value)
  }

  private object IdentityNode extends Node {
    override def resolve(value: AvroValue): AvroValue =
      value
  }

  /** Promotes values to the type of the target schema, like the Java Avro library. */
  private final class PromotionNode(target: Schema) extends Node {
    override def resolve(value: AvroValue): AvroValue =
      (value, target) match {
        case (AvroValue.Int(int), _: Schema.Long) => AvroValue.Long(int.toLong)
        case (AvroValue.Int(int), _: Schema.Float) => AvroValue.Float(int.toFloat)
        case (AvroValue.Int(int), _: Schema.Double) => AvroValue.Double(int.toDouble)
        case (AvroValue.Long(long), _: Schema.Float) => AvroValue.Float(long.toFloat)
        case (AvroValue.Long(long), _: Schema.Double) => AvroValue.Double(long.toDouble)
        case (AvroValue.Float(float), _: Schema.Double) => AvroValue.Double(float.toDouble)
        case (AvroValue.String(string), _: Schema.Bytes) =>
          AvroValue.Bytes(ByteVector.view(string.getBytes(StandardCharsets.UTF_8)))
        case (AvroValue.Bytes(bytes), _: Schema.String) =>
          AvroValue.String(new String(bytes.toArray, StandardCharsets.UTF_8))
        case _ =>
          throw new InvalidValue(value, SchemaNames.describe(target))
      }
  }

  private final class RecordNode(fullName: String, size: Int, sources: Array[FieldSource]) extends Node {
    override def resolve(value: AvroValue): AvroValue =
      value match {
        case AvroValue.Record(values) if values.size == size =>
          val builder = Vector.newBuilder[AvroValue]
          builder.sizeHint(sources.length)
          sources.foreach {
            case FieldSource.Value(index, node) => builder += node.resolve(values(index))
            case FieldSource.Default(default) => builder += default
          }
          AvroValue.Record(builder.result())

        case _ =>
          throw new InvalidValue(value, s"record $fullName")
      }
  }

  private final class EnumNode(fullName: String, symbols: Map[String, String]) extends Node {
    override def resolve(value: AvroValue): AvroValue =
      value match {
        case AvroValue.Enum(symbol) if symbols.contains(symbol) => AvroValue.Enum(symbols(symbol))
        case _ => throw new InvalidValue(value, s"enum $fullName")
      }
  }

  private final class ArrayNode(items: Node) extends Node {
    override def resolve(value: AvroValue): AvroValue =
      value match {
        case AvroValue.Array(values) => AvroValue.Array(values.map(items.resolve))
        case _ => throw new InvalidValue(value, "array")
      }
  }

  private final class MapNode(values: Node) extends Node {
    override def resolve(value: AvroValue): AvroValue =
      value match {
        case AvroValue.Map(entries) =>
          AvroValue.Map(entries.map { case (key, value) => (key, values.resolve(value)) })
        case _ =>
          throw new InvalidValue(value, "map")
      }
  }

  /** Resolves values of union types into the target union types at the corresponding index. */
  private final class UnionToUnionNode(types: Array[(Int, Node)]) extends Node {
    override def resolve(value: AvroValue): AvroValue =
      value match {
        case AvroValue.Union(index, value) if index >= 0 && index < types.length =>
          val (targetIndex, node) = types(index)
          AvroValue.Union(targetIndex, node.resolve(value))
        case _ =>
          throw new InvalidValue(value, "union")
      }
  }

  /** Resolves values of union types into the target schema, which is not a union. */
  private final class FromUnionNode(types: Array[Node]) extends Node {
    override def resolve(value: AvroValue): AvroValue =
      value match {
        case AvroValue.Union(index, value) if index >= 0 && index < types.length =>
          types(index).resolve(value)
        case _ =>
          throw new InvalidValue(value, "union")
      }
  }

  /** Resolves values into the type at the index of the target union. */
  private final class ToUnionNode(index: Int, node: Node) extends Node {
    override def resolve(value: AvroValue): AvroValue =
      AvroValue.Union(index, node.resolve(value))
  }

  /** The definition of the named schema, for references to named schemas. */
  private def definition(schema: Schema, definitions: collection.Map[String, Schema.Named]): Schema =
    schema match {
      case ref: Schema.Ref => definitions.getOrElse(ref.fullName, ref)
      case _ => schema
    }

  /**
    * Returns `true` if the named schemas have the same name, or the target schema has the full name of
    * the schema as an alias, like the Java Avro library.
    */
  private def sameName(named: Schema.Named, target: Schema.Named): Boolean =
    named.name.name == target.name.name ||
      target.aliases.exists(alias => Schema.Name(alias, target.name.namespace).fullName == named.fullName)

  /**
    * Returns `true` if the schema is the same kind of named schema as the target, with the name of
    * the target or one of its aliases, comparing names using the function.
    */
  private def sameNamedType(named: Schema.Named, target: Schema)(name: Schema.Name => String): Boolean =
    target match {
      case targetNamed: Schema.Named if SchemaNames.typeName(targetNamed) == SchemaNames.typeName(named) =>
        val aliases = targetNamed.aliases.map(Schema.Name(_, targetNamed.name.namespace))
        (targetNamed.name +: aliases).exists(name(_) == name(named.name))
      case _ =>
        false
    }

  /** Returns `true` if values of the schema can be promoted to the target schema. */
  private def isPromotion(schema: Schema, target: Schema): Boolean =
    (schema, target) match {
      case (_: Schema.Int, _: Schema.Long | _: Schema.Float | _: Schema.Double) => true
      case (_: Schema.Long, _: Schema.Float | _: Schema.Double) => true
      case (_: Schema.Float, _: Schema.Double) => true
      case (_: Schema.String, _: Schema.Bytes) => true
      case (_: Schema.Bytes, _: Schema.String) => true
      case _ => false
    }

  /** The logical type of the schema, ignoring unknown logical types. */
  private def knownLogicalType(schema: Schema): Option[LogicalType] =
    schema.logicalType.filter(logicalType => LogicalType.knownNames.contains(logicalType.name))

  private def sameLogicalType(schema: Schema, target: Schema): Either[AvroException, Unit] =
    (knownLogicalType(schema), knownLogicalType(target)) match {
      case (logicalType, targetLogicalType) if logicalType == targetLogicalType =>
        Right(())
      case (logicalType, targetLogicalType) =>
        Left(
          new IncompatibleWriterSchema(
            s"${describe(schema, logicalType)} has ${describeLogicalType(targetLogicalType)} in writer schema"
          )
        )
    }

  private def noLogicalTypes(schema: Schema, target: Schema): Either[AvroException, Unit] =
    (knownLogicalType(schema), knownLogicalType(target)) match {
      case (None, None) => Right(())
      case _ => Left(mismatch(schema, target))
    }

  private def describe(schema: Schema, logicalType: Option[LogicalType]): String =
    logicalType.fold(SchemaNames.describe(schema))(logicalType =>
      s"${SchemaNames.describe(schema)} with logical type ${logicalType.name}"
    )

  private def describeLogicalType(logicalType: Option[LogicalType]): String =
    logicalType.fold("no logical type")(logicalType => s"logical type ${logicalType.name}")

  private def mismatch(schema: Schema, target: Schema): AvroException =
    new IncompatibleWriterSchema(
      s"${describe(schema, knownLogicalType(schema))} cannot be written as " +
        s"${describe(target, knownLogicalType(target))} in writer schema"
    )

  /** The number of a default, or a string for a non-finite number, like the Java Avro library. */
  private def number(json: Json): Option[Double] =
    json.asNumber.map(_.toDouble).orElse {
      json.asString.collect {
        case "NaN" => Double.NaN
        case "Infinity" => Double.PositiveInfinity
        case "-Infinity" => Double.NegativeInfinity
      }
    }

  /** Bytes from a string with one character per byte, same as the Java Avro library. */
  private def bytes(string: String): ByteVector =
    ByteVector.view(string.getBytes(StandardCharsets.ISO_8859_1))
}
