/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.internal

import scala.collection.mutable
import scodec.bits.ByteVector
import vulcan.AvroError
import vulcan.v2.{AvroValue, Schema}

/** Decodes values in the Avro binary encoding of a writer schema, compiled once for the schema. */
private[v2] final class BinaryDecoder private (
  root: BinaryDecoder.Node,
  maxZeroByteItems: Long
) {

  /** Reads a value, throwing [[BinaryException]] for invalid input. */
  def read(reader: BinaryReader): AvroValue =
    root.read(new BinaryDecoder.Input(reader, maxZeroByteItems))

  /** Decodes a value from the start of the bytes, ignoring any remaining bytes. */
  def decode(bytes: ByteVector): Either[AvroError, AvroValue] =
    try Right(read(BinaryReader(bytes)))
    catch {
      case e: BinaryException => Left(AvroError(e.getMessage))
    }
}

private[v2] object BinaryDecoder {

  /** The default limit for the total number of array items which are encoded using zero bytes. */
  val DefaultMaxZeroByteItems: Long =
    1000000L

  /** Returns a decoder for the writer schema, if the schema is valid. */
  def apply(
    writerSchema: Schema,
    maxZeroByteItems: Long = DefaultMaxZeroByteItems
  ): Either[AvroError, BinaryDecoder] =
    SchemaValidator.validate(writerSchema, validateDefaults = false).flatMap { _ =>
      val definitions = namedSchemas(writerSchema)

      definitions.values.collectFirst {
        case record: Schema.Record if minSize(record, definitions, Set.empty) == Unbounded =>
          record
      } match {
        case Some(record) =>
          Left(
            AvroError(
              s"Unable to decode record ${record.fullName}, since it always contains itself"
            )
          )

        case None =>
          val root = new Compilation(definitions).compile(writerSchema)
          Right(new BinaryDecoder(root, maxZeroByteItems))
      }
    }

  /** The state of reading a value, including the number of zero byte items read so far. */
  private final class Input(val reader: BinaryReader, val maxZeroByteItems: Long) {
    var zeroByteItems: Long = 0L
  }

  private sealed abstract class Node {
    def read(input: Input): AvroValue
  }

  private final class Compilation(definitions: collection.Map[String, Schema.Named]) {
    private val compiled = mutable.Map.empty[String, Reference]

    def compile(schema: Schema): Node =
      schema match {
        case _: Schema.Null      => NullNode
        case _: Schema.Boolean   => BooleanNode
        case _: Schema.Int       => IntNode
        case _: Schema.Long      => LongNode
        case _: Schema.Float     => FloatNode
        case _: Schema.Double    => DoubleNode
        case _: Schema.Bytes     => BytesNode
        case _: Schema.String    => StringNode
        case array: Schema.Array =>
          new ArrayNode(compile(array.items), minSize(array.items, definitions, Set.empty))
        case map: Schema.Map =>
          // Keys are strings, which are encoded using at least one byte
          new MapNode(compile(map.values), add(1L, minSize(map.values, definitions, Set.empty)))
        case union: Schema.Union =>
          new UnionNode(union.types.map(compile).toArray)
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
            new RecordNode(record.fields.map(field => compile(field.schema)).toArray)
          case enumeration: Schema.Enum =>
            new EnumNode(enumeration.fullName, enumeration.symbols.toArray)
          case fixed: Schema.Fixed =>
            new FixedNode(fixed.size)
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

    override def read(input: Input): AvroValue =
      node.read(input)
  }

  private object NullNode extends Node {
    override def read(input: Input): AvroValue =
      AvroValue.Null
  }

  private object BooleanNode extends Node {
    override def read(input: Input): AvroValue =
      AvroValue.Boolean(input.reader.readBoolean())
  }

  private object IntNode extends Node {
    override def read(input: Input): AvroValue =
      AvroValue.Int(input.reader.readInt())
  }

  private object LongNode extends Node {
    override def read(input: Input): AvroValue =
      AvroValue.Long(input.reader.readLong())
  }

  private object FloatNode extends Node {
    override def read(input: Input): AvroValue =
      AvroValue.Float(input.reader.readFloat())
  }

  private object DoubleNode extends Node {
    override def read(input: Input): AvroValue =
      AvroValue.Double(input.reader.readDouble())
  }

  private object BytesNode extends Node {
    override def read(input: Input): AvroValue =
      AvroValue.Bytes(input.reader.readBytes())
  }

  private object StringNode extends Node {
    override def read(input: Input): AvroValue =
      AvroValue.String(input.reader.readString())
  }

  private final class RecordNode(fields: Array[Node]) extends Node {
    override def read(input: Input): AvroValue = {
      val values = Vector.newBuilder[AvroValue]
      var index = 0
      while (index < fields.length) {
        values += fields(index).read(input)
        index += 1
      }
      AvroValue.Record(values.result())
    }
  }

  private final class EnumNode(fullName: String, symbols: Array[String]) extends Node {
    override def read(input: Input): AvroValue = {
      val begin = input.reader.position
      val index = input.reader.readInt()
      if (index >= 0 && index < symbols.length) AvroValue.Enum(symbols(index))
      else
        throw new BinaryException(
          s"Invalid index $index at position $begin for enum $fullName with ${symbols.length} symbols"
        )
    }
  }

  private final class FixedNode(size: Int) extends Node {
    override def read(input: Input): AvroValue =
      AvroValue.Fixed(input.reader.readFixed(size))
  }

  private final class ArrayNode(items: Node, minItemSize: Long) extends Node {
    override def read(input: Input): AvroValue = {
      val values = Vector.newBuilder[AvroValue]
      var count = readBlockCount(input, minItemSize)
      while (count > 0) {
        var index = 0L
        while (index < count) {
          values += items.read(input)
          index += 1
        }
        count = readBlockCount(input, minItemSize)
      }
      AvroValue.Array(values.result())
    }
  }

  private final class MapNode(values: Node, minEntrySize: Long) extends Node {
    override def read(input: Input): AvroValue = {
      val entries = Vector.newBuilder[(String, AvroValue)]
      var count = readBlockCount(input, minEntrySize)
      while (count > 0) {
        var index = 0L
        while (index < count) {
          val key = input.reader.readString()
          entries += ((key, values.read(input)))
          index += 1
        }
        count = readBlockCount(input, minEntrySize)
      }
      AvroValue.Map(entries.result())
    }
  }

  private final class UnionNode(types: Array[Node]) extends Node {
    override def read(input: Input): AvroValue = {
      val begin = input.reader.position
      val index = input.reader.readInt()
      if (index >= 0 && index < types.length) AvroValue.Union(index, types(index).read(input))
      else
        throw new BinaryException(
          s"Invalid index $index at position $begin for union with ${types.length} types"
        )
    }
  }

  private def readBlockCount(input: Input, minItemSize: Long): Long = {
    val begin = input.reader.position
    val count = input.reader.readBlockCount()

    if (minItemSize > 0) {
      if (count > input.reader.remaining / minItemSize)
        throw new BinaryException(
          s"Invalid block count $count at position $begin, exceeding the remaining input"
        )
    } else {
      if (count > input.maxZeroByteItems - input.zeroByteItems)
        throw new BinaryException(
          s"Invalid block count $count at position $begin, exceeding the limit of ${input.maxZeroByteItems} zero byte items"
        )
      input.zeroByteItems += count
    }

    count
  }

  /** The named schemas defined in the schema, by full name, in the order they are defined. */
  private def namedSchemas(schema: Schema): collection.Map[String, Schema.Named] = {
    val definitions = mutable.LinkedHashMap.empty[String, Schema.Named]

    def collect(schema: Schema): Unit =
      schema match {
        case named: Schema.Named if !definitions.contains(named.fullName) =>
          definitions.update(named.fullName, named)
          named match {
            case record: Schema.Record => record.fields.foreach(field => collect(field.schema))
            case _                     => ()
          }

        case array: Schema.Array => collect(array.items)
        case map: Schema.Map     => collect(map.values)
        case union: Schema.Union => union.types.foreach(collect)
        case _                   => ()
      }

    collect(schema)
    definitions
  }

  /** The minimum size of records which always contain themselves. */
  private val Unbounded: Long =
    Long.MaxValue

  /** The minimum number of bytes needed to encode a value of the schema. */
  private def minSize(
    schema: Schema,
    definitions: collection.Map[String, Schema.Named],
    enclosing: Set[String]
  ): Long =
    schema match {
      case _: Schema.Null        => 0L
      case _: Schema.Float       => 4L
      case _: Schema.Double      => 8L
      case fixed: Schema.Fixed   => fixed.size.toLong
      case record: Schema.Record =>
        if (enclosing.contains(record.fullName)) Unbounded
        else {
          val fieldsEnclosing = enclosing + record.fullName
          record.fields.foldLeft(0L) { (size, field) =>
            add(size, minSize(field.schema, definitions, fieldsEnclosing))
          }
        }
      case ref: Schema.Ref =>
        minSize(definitions(ref.fullName), definitions, enclosing)
      // Other types are encoded using at least one byte, including union indices and block counts
      case _ =>
        1L
    }

  private def add(first: Long, second: Long): Long =
    if (first > Long.MaxValue - second) Long.MaxValue else first + second
}
