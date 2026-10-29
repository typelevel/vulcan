/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scodec.bits.ByteVector
import vulcan.AvroError
import vulcan.AvroValue
import vulcan.Schema

/** Decodes values in the Avro binary encoding of a writer schema, compiled once for the schema. */
private[vulcan] final class BinaryDecoder private (root: BinaryDecoder.Node) {

  /** Reads a value, throwing [[BinaryException]] for invalid input. */
  def read(reader: BinaryReader): AvroValue =
    root.read(new BinaryDecoder.Input(reader))

  /** Decodes a value from the start of the bytes, ignoring any remaining bytes. */
  def decode(bytes: ByteVector): Either[AvroError, AvroValue] =
    try Right(read(BinaryReader(bytes)))
    catch {
      case e: BinaryException => Left(AvroError(e.getMessage))
    }
}

private[vulcan] object BinaryDecoder {

  /** The limit for the total number of array items in a value encoded using zero bytes. */
  val MaxZeroByteItems: Long =
    1000000L

  /** Returns a decoder for the writer schema, if the schema is valid. */
  def apply(writerSchema: Schema): Either[AvroError, BinaryDecoder] =
    SchemaValidator.validate(writerSchema, validateDefaults = false).flatMap { _ =>
      val definitions = SchemaNames.definitions(writerSchema)

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
          Right(new BinaryDecoder(root))
      }
    }

  /** The state of reading a value, including the number of zero byte items read so far. */
  private final class Input(val reader: BinaryReader) {
    var zeroByteItems: Long = 0L
  }

  private sealed abstract class Node {
    def read(input: Input): AvroValue
  }

  private final class Compilation(definitions: collection.Map[String, Schema.Named]) {
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
      val values = new Array[AnyRef](fields.length)
      var index = 0
      while (index < fields.length) {
        values(index) = fields(index).read(input)
        index += 1
      }
      AvroValue.Record(Vector.from(arraySeq[AvroValue](values)))
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

  /** Arrays and maps, which are encoded in blocks, usually with all items in a single block. */
  private sealed abstract class BlocksNode[A <: AnyRef](minItemSize: Long) extends Node {
    protected def readItem(input: Input): A

    protected final def readItems(input: Input): Vector[A] = {
      var count = readBlockCount(input, minItemSize)
      if (count == 0) Vector.empty
      else {
        val first = new Array[AnyRef](count.toInt)
        var index = 0
        while (index < first.length) {
          first(index) = readItem(input)
          index += 1
        }

        count = readBlockCount(input, minItemSize)
        if (count == 0) Vector.from(arraySeq[A](first))
        else {
          val items = Vector.newBuilder[A] ++= arraySeq[A](first)
          while (count > 0) {
            var index = 0L
            while (index < count) {
              items += readItem(input)
              index += 1
            }
            count = readBlockCount(input, minItemSize)
          }
          items.result()
        }
      }
    }
  }

  private final class ArrayNode(items: Node, minItemSize: Long) extends BlocksNode[AvroValue](minItemSize) {
    override protected def readItem(input: Input): AvroValue =
      items.read(input)

    override def read(input: Input): AvroValue =
      AvroValue.Array(readItems(input))
  }

  private final class MapNode(values: Node, minEntrySize: Long)
    extends BlocksNode[(String, AvroValue)](minEntrySize) {
    override protected def readItem(input: Input): (String, AvroValue) = {
      val key = input.reader.readString()
      (key, values.read(input))
    }

    override def read(input: Input): AvroValue =
      AvroValue.Map(readItems(input))
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
      if (count > MaxZeroByteItems - input.zeroByteItems)
        throw new BinaryException(
          s"Invalid block count $count at position $begin, exceeding the limit of $MaxZeroByteItems zero byte items"
        )
      input.zeroByteItems += count
    }

    count
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
      case _: Schema.Null => 0L
      case _: Schema.Float => 4L
      case _: Schema.Double => 8L
      case fixed: Schema.Fixed => fixed.size.toLong
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

  private def arraySeq[A](values: Array[AnyRef]): ArraySeq[A] =
    ArraySeq.unsafeWrapArray(values).asInstanceOf[ArraySeq[A]]
}
