/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.MonadError
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scodec.bits.ByteVector
import vulcan.AvroException.InfiniteRecord
import vulcan.AvroException.InvalidBinary
import vulcan.internal.BinaryReader
import vulcan.internal.DecoderMonadError
import vulcan.internal.SchemaNames
import vulcan.internal.SchemaValidator

/**
  * Decodes Avro binary, written using a writer schema, to type `A`.
  *
  * Note decoders are compiled once for the writer schema using the
  * [[Codec#binaryDecoder]] function, and can be reused as many times
  * as required, while also supporting concurrent decodes.
  */
trait BinaryDecoder[A] {

  /** Attempts to decode a value from the start of the bytes, ignoring any remaining bytes. */
  def decode(bytes: ByteVector): Either[AvroException, A]
}

object BinaryDecoder {
  implicit final val binaryDecoderMonadError: MonadError[BinaryDecoder, AvroException] =
    new DecoderMonadError[BinaryDecoder, ByteVector] {
      override protected def decoder[A](f: ByteVector => Either[AvroException, A]): BinaryDecoder[A] =
        new BinaryDecoder[A] {
          override def decode(bytes: ByteVector): Either[AvroException, A] =
            f(bytes)
        }

      override protected def decode[A](fa: BinaryDecoder[A], input: ByteVector): Either[AvroException, A] =
        fa.decode(input)
    }

  /** The limit for the total number of array items in a value encoded using zero bytes. */
  private[vulcan] val MaxZeroByteItems: Long =
    1000000L

  /** The limit for the number of nested values of recursive schemas, to avoid stack overflows. */
  private[vulcan] val MaxDepth: Int =
    512

  /** Returns a decoder for values written using the writer schema, if the schema is valid. */
  private[vulcan] def apply(writerSchema: Schema): Either[AvroException, BinaryDecoder[AvroValue]] =
    reader(writerSchema).map { read =>
      new BinaryDecoder[AvroValue] {
        override def decode(bytes: ByteVector): Either[AvroException, AvroValue] =
          try Right(read(BinaryReader(bytes)))
          catch {
            case e: AvroException => Left(e)
          }
      }
    }

  /**
    * Returns a function reading values written using the writer schema, if the schema is valid.
    *
    * The function throws `AvroException` for invalid input.
    */
  private[vulcan] def reader(writerSchema: Schema): Either[AvroException, BinaryReader => AvroValue] =
    SchemaValidator.validate(writerSchema, validateDefaults = false).flatMap { _ =>
      val definitions = SchemaNames.definitions(writerSchema)

      definitions.values.collectFirst {
        case record: Schema.Record if minSize(record, definitions, Set.empty) == Unbounded =>
          record
      } match {
        case Some(record) =>
          Left(new InfiniteRecord(record))

        case None =>
          val root = new Compilation(definitions).compile(writerSchema)
          Right((reader: BinaryReader) => root.read(new Input(reader)))
      }
    }

  /**
    * The state of reading a value, including the number of zero byte items
    * read so far, and the number of nested values read for recursive schemas.
    */
  private final class Input(val reader: BinaryReader) {
    var zeroByteItems: Long = 0L

    var depth: Int = 0
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

  /**
    * A named schema, which is being compiled if references are recursive. Since references
    * are only used for recursive schemas, reading them limits the depth of nested values.
    */
  private final class Reference extends Node {
    var node: Node = null

    def resolved: Node =
      if (node != null) node else this

    override def read(input: Input): AvroValue = {
      if (input.depth == MaxDepth)
        throw new InvalidBinary(
          s"Invalid depth ${MaxDepth + 1} at position ${input.reader.position}, exceeding the limit of $MaxDepth nested values of recursive schemas"
        )

      input.depth += 1
      val value = node.read(input)
      input.depth -= 1
      value
    }
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
        throw new InvalidBinary(
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
        throw new InvalidBinary(
          s"Invalid index $index at position $begin for union with ${types.length} types"
        )
    }
  }

  private def readBlockCount(input: Input, minItemSize: Long): Long = {
    val begin = input.reader.position
    val count = input.reader.readBlockCount()

    if (minItemSize > 0) {
      if (count > input.reader.remaining / minItemSize)
        throw new InvalidBinary(
          s"Invalid block count $count at position $begin, exceeding the remaining input"
        )
    } else {
      if (count > MaxZeroByteItems - input.zeroByteItems)
        throw new InvalidBinary(
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
