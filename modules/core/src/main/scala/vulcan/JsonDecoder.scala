/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.syntax.all.*
import io.circe.Json
import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scodec.bits.ByteVector
import vulcan.AvroException.InvalidJson
import vulcan.internal.SchemaNames
import vulcan.internal.SchemaValidator

/**
  * Decodes Avro `Json`, written using a writer schema, to type `A`.
  *
  * Note decoders are compiled once for the writer schema using the
  * [[Codec#jsonDecoder]] function, and can be reused as many times
  * as required, while also supporting concurrent decodes.
  */
trait JsonDecoder[A] {

  /** Attempts to decode the specified Avro `Json`. */
  def decode(json: Json): Either[AvroException, A]
}

object JsonDecoder {

  /** Returns a decoder for values written using the writer schema, if the schema is valid. */
  private[vulcan] def apply(writerSchema: Schema): Either[AvroException, JsonDecoder[AvroValue]] =
    SchemaValidator.validate(writerSchema, validateDefaults = false).map { _ =>
      val root = new Compilation().compile(writerSchema)
      new JsonDecoder[AvroValue] {
        override def decode(json: Json): Either[AvroException, AvroValue] =
          root.decode(json)
      }
    }

  private sealed abstract class Node {
    def decode(json: Json): Either[AvroException, AvroValue]
  }

  private final case class Field(name: String, aliases: Vector[String], node: Node)

  private final class Compilation {
    private val compiled = mutable.Map.empty[String, Reference]

    def compile(schema: Schema): Node =
      schema match {
        case _: Schema.Null => NullNode
        case _: Schema.Boolean => BooleanNode
        case _: Schema.Int => IntNode
        case _: Schema.Long => LongNode
        case _: Schema.Float => FloatNode
        case _: Schema.Double => DoubleNode
        case _: Schema.Bytes => BytesNode
        case _: Schema.String => StringNode
        case array: Schema.Array => new ArrayNode(compile(array.items))
        case map: Schema.Map => new MapNode(compile(map.values))
        case union: Schema.Union =>
          new UnionNode(union.types.map(schema => (SchemaNames.unionKey(schema), compile(schema))))
        case named: Schema.Named =>
          compiled.get(named.fullName).fold(compileNamed(named))(_.resolved)
        case ref: Schema.Ref =>
          compiled(ref.fullName).resolved
      }

    private def compileNamed(named: Schema.Named): Node = {
      val reference = new Reference
      compiled.update(named.fullName, reference)

      val node =
        named match {
          case record: Schema.Record =>
            new RecordNode(
              record.fullName,
              record.fields.map(field => Field(field.name, field.aliases, compile(field.schema)))
            )
          case enumeration: Schema.Enum =>
            new EnumNode(enumeration.fullName, enumeration.symbols.toSet)
          case fixed: Schema.Fixed =>
            new FixedNode(fixed.fullName, fixed.size.toLong)
        }

      reference.node = node
      node
    }
  }

  /** A named schema, which is being compiled if references are recursive. */
  private final class Reference extends Node {
    var node: Node = null

    def resolved: Node =
      if (node != null) node else this

    override def decode(json: Json): Either[AvroException, AvroValue] =
      node.decode(json)
  }

  private object NullNode extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      if (json.isNull) Right(AvroValue.Null) else invalid(json, "null")
  }

  private object BooleanNode extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asBoolean.map(AvroValue.Boolean(_)).toRight(invalidError(json, "boolean"))
  }

  private object IntNode extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asNumber.flatMap(_.toInt).map(AvroValue.Int(_)).toRight(invalidError(json, "int"))
  }

  private object LongNode extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asNumber.flatMap(_.toLong).map(AvroValue.Long(_)).toRight(invalidError(json, "long"))
  }

  private object FloatNode extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asNumber
        .map(_.toFloat)
        .orElse(json.asString.flatMap(nonFinite).map(_.toFloat))
        .map(AvroValue.Float(_))
        .toRight(invalidError(json, "float"))
  }

  private object DoubleNode extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asNumber
        .map(_.toDouble)
        .orElse(json.asString.flatMap(nonFinite))
        .map(AvroValue.Double(_))
        .toRight(invalidError(json, "double"))
  }

  private object BytesNode extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asString
        .map(string => AvroValue.Bytes(bytes(string)))
        .toRight(invalidError(json, "bytes"))
  }

  private object StringNode extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asString.map(AvroValue.String(_)).toRight(invalidError(json, "string"))
  }

  /** Decodes fields by name or alias, where all fields are required and no others are allowed. */
  private final class RecordNode(fullName: String, fields: Vector[Field]) extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asObject match {
        case Some(obj) =>
          fields
            .traverse { field =>
              (field.name +: field.aliases).collectFirstSome(key => obj(key).tupleLeft(key)) match {
                case Some((key, value)) => field.node.decode(value).tupleLeft(key)
                case None => Left(new InvalidJson(s"Missing field ${field.name} for record $fullName"))
              }
            }
            .flatMap { decoded =>
              val keys = decoded.map(_._1).toSet
              obj.keys.filterNot(keys.contains).toList match {
                case Nil =>
                  Right(AvroValue.Record(decoded.map(_._2)))

                case unknown =>
                  Left(new InvalidJson(s"Unknown fields ${unknown.mkString(", ")} for record $fullName"))
              }
            }

        case None =>
          invalid(json, s"record $fullName")
      }
  }

  private final class EnumNode(fullName: String, symbols: Set[String]) extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asString match {
        case Some(symbol) if symbols.contains(symbol) =>
          Right(AvroValue.Enum(symbol))

        case Some(symbol) =>
          Left(new InvalidJson(s"Invalid symbol $symbol for enum $fullName"))

        case None =>
          invalid(json, s"enum $fullName")
      }
  }

  private final class FixedNode(fullName: String, size: Long) extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asString.map(bytes) match {
        case Some(fixed) if fixed.size == size =>
          Right(AvroValue.Fixed(fixed))

        case Some(fixed) =>
          Left(
            new InvalidJson(s"Invalid fixed with ${fixed.size} bytes for fixed $fullName with size $size")
          )

        case None =>
          invalid(json, s"fixed $fullName")
      }
  }

  private final class ArrayNode(items: Node) extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asArray match {
        case Some(values) => values.traverse(items.decode).map(AvroValue.Array(_))
        case None => invalid(json, "array")
      }
  }

  private final class MapNode(values: Node) extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asObject match {
        case Some(obj) =>
          obj.toVector
            .traverse { case (key, value) => values.decode(value).tupleLeft(key) }
            .map(AvroValue.Map(_))

        case None =>
          invalid(json, "map")
      }
  }

  /** Decodes `null`, or objects with the key of the type of the value. */
  private final class UnionNode(types: Vector[(String, Node)]) extends Node {
    override def decode(json: Json): Either[AvroException, AvroValue] =
      json.asObject match {
        case Some(obj) =>
          obj.toList match {
            case (key, value) :: Nil =>
              types.indexWhere(_._1 == key) match {
                case -1 => Left(new InvalidJson(s"Unknown type $key for union"))
                case index => types(index)._2.decode(value).map(AvroValue.Union(index, _))
              }

            case _ =>
              Left(new InvalidJson(s"Invalid object with ${obj.size} keys for union, expected one key"))
          }

        case None if json.isNull =>
          types.indexWhere(_._1 == "null") match {
            case -1 => invalid(json, "union")
            case index => Right(AvroValue.Union(index, AvroValue.Null))
          }

        case None =>
          invalid(json, "union")
      }
  }

  /** Bytes from a string with one character per byte, same as the Java Avro library. */
  private def bytes(string: String): ByteVector =
    ByteVector.view(string.getBytes(StandardCharsets.ISO_8859_1))

  /** Strings for non-finite numbers, same as the Java Avro library. */
  private def nonFinite(string: String): Option[Double] =
    string match {
      case "NaN" => Some(Double.NaN)
      case "Infinity" | "INF" => Some(Double.PositiveInfinity)
      case "-Infinity" | "-INF" => Some(Double.NegativeInfinity)
      case _ => None
    }

  private def invalid(json: Json, schema: String): Either[AvroException, AvroValue] =
    Left(invalidError(json, schema))

  private def invalidError(json: Json, schema: String): AvroException =
    new InvalidJson(s"Invalid ${describe(json)} for $schema")

  private def describe(json: Json): String =
    json.fold(
      "null",
      _ => "boolean",
      number => s"number ${Json.fromJsonNumber(number).noSpaces}",
      _ => "string",
      _ => "array",
      _ => "object"
    )
}
