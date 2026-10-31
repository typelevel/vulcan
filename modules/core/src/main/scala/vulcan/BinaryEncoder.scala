/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Contravariant
import scala.collection.mutable
import scodec.bits.ByteVector
import vulcan.AvroException.InvalidEnumValue
import vulcan.AvroException.InvalidFixedValue
import vulcan.AvroException.InvalidRecordValue
import vulcan.AvroException.InvalidUnionValue
import vulcan.AvroException.InvalidValue
import vulcan.internal.BinaryWriter
import vulcan.internal.SchemaValidator

/**
  * Encodes values of type `A` to Avro binary.
  *
  * Note encoders are compiled once for the schema using the
  * [[Codec#binaryEncoder]] function, and can be reused as
  * many times as needed, supporting concurrent encodes.
  */
trait BinaryEncoder[A] {

  /** Attempts to encode the specified value to Avro binary. */
  def encode(a: A): Either[AvroException, ByteVector]
}

object BinaryEncoder {
  implicit final val binaryEncoderContravariant: Contravariant[BinaryEncoder] =
    new Contravariant[BinaryEncoder] {
      override final def contramap[A, B](encoder: BinaryEncoder[A])(f: B => A): BinaryEncoder[B] =
        new BinaryEncoder[B] {
          override def encode(b: B): Either[AvroException, ByteVector] =
            encoder.encode(f(b))
        }
    }

  /** Returns an encoder for values of the schema, if the schema is valid. */
  private[vulcan] def apply(schema: Schema): Either[AvroException, BinaryEncoder[AvroValue]] =
    SchemaValidator.validate(schema, validateDefaults = false).map { _ =>
      val root = new Compilation().compile(schema)
      new BinaryEncoder[AvroValue] {

        /**
          * The size of the last value, since values usually have similar sizes. Concurrent
          * encodes might not see the latest size, which only affects the initial capacity.
          */
        private[this] var lastSize: Int = 64

        override def encode(value: AvroValue): Either[AvroException, ByteVector] = {
          val writer = new BinaryWriter(lastSize + lastSize / 8)
          try {
            root.write(value, writer)
            val bytes = writer.toByteVector
            lastSize = bytes.size.toInt
            Right(bytes)
          } catch {
            case e: AvroException => Left(e)
          }
        }
      }
    }

  private sealed abstract class Node {
    def write(value: AvroValue, writer: BinaryWriter): Unit
  }

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
        case array: Schema.Array =>
          new ArrayNode(compile(array.items))
        case map: Schema.Map =>
          new MapNode(compile(map.values))
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
            new RecordNode(
              record.fullName,
              record.fields.map(field => compile(field.schema)).toArray
            )
          case enumeration: Schema.Enum =>
            new EnumNode(enumeration.fullName, enumeration.symbols.zipWithIndex.toMap)
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

    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      node.write(value, writer)
  }

  private object NullNode extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Null => ()
        case _ => throw invalid(value, "null")
      }
  }

  private object BooleanNode extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Boolean(boolean) => writer.writeBoolean(boolean)
        case _ => throw invalid(value, "boolean")
      }
  }

  private object IntNode extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Int(int) => writer.writeInt(int)
        case _ => throw invalid(value, "int")
      }
  }

  private object LongNode extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Long(long) => writer.writeLong(long)
        case _ => throw invalid(value, "long")
      }
  }

  private object FloatNode extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Float(float) => writer.writeFloat(float)
        case _ => throw invalid(value, "float")
      }
  }

  private object DoubleNode extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Double(double) => writer.writeDouble(double)
        case _ => throw invalid(value, "double")
      }
  }

  private object BytesNode extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Bytes(bytes) => writer.writeBytes(bytes)
        case _ => throw invalid(value, "bytes")
      }
  }

  private object StringNode extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.String(string) => writer.writeString(string)
        case _ => throw invalid(value, "string")
      }
  }

  private final class RecordNode(fullName: String, fields: Array[Node]) extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Record(values) if values.size == fields.length =>
          var index = 0
          while (index < fields.length) {
            fields(index).write(values(index), writer)
            index += 1
          }

        case AvroValue.Record(values) =>
          throw new InvalidRecordValue(values.size, fullName, fields.length)

        case _ =>
          throw invalid(value, s"record $fullName")
      }
  }

  private final class EnumNode(fullName: String, indices: Map[String, Int]) extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Enum(symbol) =>
          indices.get(symbol) match {
            case Some(index) => writer.writeInt(index)
            case None => throw new InvalidEnumValue(symbol, fullName)
          }

        case _ =>
          throw invalid(value, s"enum $fullName")
      }
  }

  private final class FixedNode(fullName: String, size: Long) extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Fixed(bytes) if bytes.size == size =>
          writer.writeFixed(bytes)

        case AvroValue.Fixed(bytes) =>
          throw new InvalidFixedValue(bytes.size, fullName, size)

        case _ =>
          throw invalid(value, s"fixed $fullName")
      }
  }

  /** Writes the items in a single block, same as the Java Avro library. */
  private final class ArrayNode(items: Node) extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Array(values) =>
          if (values.nonEmpty) {
            writer.writeLong(values.size.toLong)
            val iterator = values.iterator
            while (iterator.hasNext) items.write(iterator.next(), writer)
          }
          writer.writeLong(0L)

        case _ =>
          throw invalid(value, "array")
      }
  }

  /** Writes the entries in a single block, same as the Java Avro library. */
  private final class MapNode(values: Node) extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Map(entries) =>
          if (entries.nonEmpty) {
            writer.writeLong(entries.size.toLong)
            val iterator = entries.iterator
            while (iterator.hasNext) {
              val entry = iterator.next()
              writer.writeString(entry._1)
              values.write(entry._2, writer)
            }
          }
          writer.writeLong(0L)

        case _ =>
          throw invalid(value, "map")
      }
  }

  private final class UnionNode(types: Array[Node]) extends Node {
    override def write(value: AvroValue, writer: BinaryWriter): Unit =
      value match {
        case AvroValue.Union(index, branchValue) if index >= 0 && index < types.length =>
          writer.writeInt(index)
          types(index).write(branchValue, writer)

        case AvroValue.Union(index, _) =>
          throw new InvalidUnionValue(index, types.length)

        case _ =>
          throw invalid(value, "union")
      }
  }

  private def invalid(value: AvroValue, schema: String): AvroException =
    new InvalidValue(value, schema)
}
