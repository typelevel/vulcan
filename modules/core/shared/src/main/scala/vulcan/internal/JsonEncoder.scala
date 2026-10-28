/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import cats.syntax.all.*
import io.circe.Json
import java.nio.charset.StandardCharsets
import scala.collection.mutable
import scodec.bits.ByteVector
import vulcan.AvroError
import vulcan.AvroValue
import vulcan.Schema

/** Encodes values in the Avro JSON encoding of a schema, compiled once for the schema. */
private[vulcan] final class JsonEncoder private (root: JsonEncoder.Node) {
  def encode(value: AvroValue): Either[AvroError, Json] =
    root.encode(value)
}

private[vulcan] object JsonEncoder {

  /** Returns an encoder for the schema, if the schema is valid. */
  def apply(schema: Schema): Either[AvroError, JsonEncoder] =
    compile(schema, unionTypes = true)

  /**
    * Returns an encoder which encodes values of unions without the type, same as for defaults in
    * schemas.
    */
  def withoutUnionTypes(schema: Schema): Either[AvroError, JsonEncoder] =
    compile(schema, unionTypes = false)

  private def compile(schema: Schema, unionTypes: Boolean): Either[AvroError, JsonEncoder] =
    SchemaValidator
      .validate(schema, validateDefaults = false)
      .map(_ => new JsonEncoder(new Compilation(unionTypes).compile(schema)))

  /** Bytes as a string with one character per byte, same as the Java Avro library. */
  private def bytesJson(bytes: ByteVector): Json =
    Json.fromString(ByteVectors.decodeString(bytes, StandardCharsets.ISO_8859_1))

  private sealed abstract class Node {
    def encode(value: AvroValue): Either[AvroError, Json]
  }

  private final class Compilation(unionTypes: Boolean) {
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
          new UnionNode(union.types.map { schema =>
            val key = schema match {
              case _: Schema.Null => None
              case _ if unionTypes => Some(SchemaNames.unionKey(schema))
              case _ => None
            }

            (key, compile(schema))
          })
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
              record.fields.map(field => (field.name, compile(field.schema)))
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

    override def encode(value: AvroValue): Either[AvroError, Json] =
      node.encode(value)
  }

  private object NullNode extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Null => Right(Json.Null)
        case _ => invalid(value, "null")
      }
  }

  private object BooleanNode extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Boolean(boolean) => Right(Json.fromBoolean(boolean))
        case _ => invalid(value, "boolean")
      }
  }

  private object IntNode extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Int(int) => Right(Json.fromInt(int))
        case _ => invalid(value, "int")
      }
  }

  private object LongNode extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Long(long) => Right(Json.fromLong(long))
        case _ => invalid(value, "long")
      }
  }

  private object FloatNode extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Float(float) => Right(Json.fromFloatOrString(float))
        case _ => invalid(value, "float")
      }
  }

  private object DoubleNode extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Double(double) => Right(Json.fromDoubleOrString(double))
        case _ => invalid(value, "double")
      }
  }

  private object BytesNode extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Bytes(bytes) => Right(bytesJson(bytes))
        case _ => invalid(value, "bytes")
      }
  }

  private object StringNode extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.String(string) => Right(Json.fromString(string))
        case _ => invalid(value, "string")
      }
  }

  private final class RecordNode(fullName: String, fields: Vector[(String, Node)]) extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Record(values) if values.size == fields.size =>
          values
            .zip(fields)
            .traverse { case (fieldValue, (name, node)) =>
              node.encode(fieldValue).map(name -> _)
            }
            .map(Json.fromFields)

        case AvroValue.Record(values) =>
          Left(
            AvroError(
              s"Invalid record with ${values.size} values for record $fullName with ${fields.size} fields"
            )
          )

        case _ =>
          invalid(value, s"record $fullName")
      }
  }

  private final class EnumNode(fullName: String, symbols: Set[String]) extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Enum(symbol) if symbols.contains(symbol) =>
          Right(Json.fromString(symbol))

        case AvroValue.Enum(symbol) =>
          Left(AvroError(s"Invalid symbol $symbol for enum $fullName"))

        case _ =>
          invalid(value, s"enum $fullName")
      }
  }

  private final class FixedNode(fullName: String, size: Long) extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Fixed(bytes) if bytes.size == size =>
          Right(bytesJson(bytes))

        case AvroValue.Fixed(bytes) =>
          Left(
            AvroError(s"Invalid fixed with ${bytes.size} bytes for fixed $fullName with size $size")
          )

        case _ =>
          invalid(value, s"fixed $fullName")
      }
  }

  private final class ArrayNode(items: Node) extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Array(values) => values.traverse(items.encode).map(Json.fromValues)
        case _ => invalid(value, "array")
      }
  }

  private final class MapNode(values: Node) extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Map(entries) =>
          entries
            .traverse { case (key, entryValue) => values.encode(entryValue).map(key -> _) }
            .map(Json.fromFields)

        case _ =>
          invalid(value, "map")
      }
  }

  /**
    * Encodes the value as an object with the key of the type, unless the value is `null` or the
    * type is not included.
    */
  private final class UnionNode(types: Vector[(Option[String], Node)]) extends Node {
    override def encode(value: AvroValue): Either[AvroError, Json] =
      value match {
        case AvroValue.Union(index, branchValue) if index >= 0 && index < types.size =>
          val (key, node) = types(index)
          node.encode(branchValue).map { json =>
            key.fold(json)(key => Json.obj(key -> json))
          }

        case AvroValue.Union(index, _) =>
          Left(AvroError(s"Invalid index $index for union with ${types.size} types"))

        case _ =>
          invalid(value, "union")
      }
  }

  private def invalid(value: AvroValue, schema: String): Either[AvroError, Json] =
    Left(AvroError(s"Invalid ${ValueNames.typeName(value)} value for $schema"))
}
