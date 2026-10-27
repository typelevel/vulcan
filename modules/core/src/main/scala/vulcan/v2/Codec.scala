/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import cats.{Invariant, Show}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import io.circe.Json
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReferenceArray
import java.util.Arrays
import scala.annotation.{implicitNotFound, tailrec}
import scala.collection.immutable.{ArraySeq, SortedMap, SortedSet}
import scala.collection.mutable
import scala.util.Try
import scodec.bits.ByteVector
import vulcan.AvroError
import vulcan.v2.internal._

/** Provides a schema, along with encoding and decoding functions for a given type. */
@implicitNotFound(
  "could not find implicit Codec[${A}]; ensure no imports are missing or manually define an instance"
)
sealed abstract class Codec[A] {

  /** The schema or an error if the schema could not be generated. */
  def schema: Either[AvroError, Schema]

  /** Attempts to encode the specified value. */
  def encode(a: A): Either[AvroError, AvroValue]

  /** Attempts to decode the specified value, written using the writer schema. */
  final def decode(value: AvroValue, writerSchema: Schema): Either[AvroError, A] =
    decoder(writerSchema, new Definitions(writerSchema))(value)

  /** Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    */
  final def imap[B](f: A => B)(g: B => A): Codec[B] =
    imapErrors(a => Right(f(a)))(b => Right(g(b)))

  /** Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    *
    * Similar to [[Codec#imap]], except the mapping from `A` to `B` might be unsuccessful.
    */
  final def imapError[B](f: A => Either[AvroError, B])(g: B => A): Codec[B] =
    imapErrors(f)(b => Right(g(b)))

  /** Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    *
    * Similar to [[Codec#imap]], except the mappings in both directions might be unsuccessful.
    */
  final def imapErrors[B](f: A => Either[AvroError, B])(g: B => Either[AvroError, A]): Codec[B] =
    Codec.ImapErrors(this, f, g)

  /** Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    *
    * Similar to [[Codec#imap]], except the mapping from `A` to `B` might be unsuccessful.
    */
  final def imapTry[B](f: A => Try[B])(g: B => A): Codec[B] =
    imapError(f(_).toEither.left.map(AvroError.fromThrowable))(g)

  /** Returns a function decoding values of the writer schema; neither a union nor a reference. */
  private[v2] def resolve(
    writerSchema: Schema,
    definitions: Definitions
  ): AvroValue => Either[AvroError, A]

  /** Returns a function decoding values of the writer schema, resolved once for the schema. */
  private[v2] final def decoder(
    writerSchema: Schema,
    definitions: Definitions
  ): AvroValue => Either[AvroError, A] =
    writerSchema match {
      case union: Schema.Union =>
        new Codec.UnionDecoder(this, union, definitions)

      case ref: Schema.Ref =>
        definitions.get(ref.fullName) match {
          case Some(named) => resolve(named, definitions)
          case None        => Codec.failing(AvroError(s"Undefined name ${ref.fullName}"))
        }

      case _ =>
        resolve(writerSchema, definitions)
    }

  private[vulcan] final def withTypeName(typeName: String): Codec[A] =
    Codec.WithTypeName(this, typeName)

  /** The binary encoder for the schema, compiled once for the codec. */
  private[v2] final lazy val binaryEncoder: Either[AvroError, BinaryEncoder] =
    schema.flatMap(BinaryEncoder(_))

  /** The JSON encoder for the schema, compiled once for the codec. */
  private[v2] final lazy val jsonEncoder: Either[AvroError, JsonEncoder] =
    schema.flatMap(JsonEncoder(_))

  override final def toString: String =
    schema match {
      case Right(schema) => s"Codec(${schema.toJson.spaces2})"
      case Left(error)   => error.toString
    }
}

/** @groupname General General Codecs
  * @groupprio General 0
  * @groupdesc General
  *   Default codecs for standard library types.
  *
  * @groupname Collection Collection Codecs
  * @groupprio Collection 1
  * @groupdesc Collection
  *   Default codecs for standard library collection types.
  *
  * @groupname Cats Cats Codecs
  * @groupprio Cats 2
  * @groupdesc Cats
  *   Default codecs for Cats data types and type class instances for [[Codec]].
  *
  * @groupname Create Create Codecs
  * @groupprio Create 5
  * @groupdesc Create
  *   Functions for creating new codecs.
  *
  * @groupname Utilities
  * @groupprio Utilities 7
  * @groupdesc Utilities
  *   Miscellaneous utility functions.
  */
object Codec {

  /** Returns the [[Codec]] for the specified type.
    *
    * @group Utilities
    */
  final def apply[A](implicit codec: Codec[A]): Codec[A] =
    codec

  /** @group General
    */
  implicit lazy val boolean: Codec[Boolean] =
    BooleanCodec

  /** @group General
    */
  implicit lazy val byte: Codec[Byte] =
    Codec.int
      .imapError { int =>
        if (int.isValidByte) Right(int.toByte)
        else Left(AvroError.unexpectedByte(int))
      }(_.toInt)
      .withTypeName("Byte")

  /** @group General
    */
  implicit lazy val bytes: Codec[ByteVector] =
    BytesCodec

  /** @group Cats
    */
  implicit final def chain[A](implicit codec: Codec[A]): Codec[Chain[A]] =
    ArrayCodec[A, Chain[A]](codec, _.length.toInt, _.iterator, chainBuilder)
      .withTypeName("Chain")

  /** @group General
    */
  implicit lazy val char: Codec[Char] =
    Codec.string
      .imapError { string =>
        if (string.length == 1) Right(string.charAt(0))
        else Left(AvroError.unexpectedChar(string.length))
      }(_.toString)
      .withTypeName("Char")

  /** Returns the result of decoding the specified value to the specified type, using the schema of
    * the codec as the writer schema.
    *
    * @group Utilities
    */
  final def decode[A](value: AvroValue)(implicit codec: Codec[A]): Either[AvroError, A] =
    codec.schema.flatMap(codec.decode(value, _))

  /** @group General
    */
  implicit lazy val double: Codec[Double] =
    DoubleCodec

  /** Returns the result of encoding the specified value.
    *
    * @group Utilities
    */
  final def encode[A](a: A)(implicit codec: Codec[A]): Either[AvroError, AvroValue] =
    codec.encode(a)

  /** @group General
    */
  implicit lazy val float: Codec[Float] =
    FloatCodec

  /** Returns the result of decoding the specified Avro binary, written using the writer schema, to
    * the specified type.
    *
    * The value is decoded from the start of the bytes, and any remaining bytes are ignored.
    *
    * @group Utilities
    */
  final def fromBinary[A: Codec](bytes: ByteVector, writerSchema: Schema): Either[AvroError, A] =
    BinaryDecoder(writerSchema)
      .flatMap(_.decode(bytes))
      .flatMap(Codec[A].decode(_, writerSchema))

  /** Returns the result of decoding the specified Avro JSON, written using the writer schema, to
    * the specified type.
    *
    * @group Utilities
    */
  final def fromJson[A: Codec](json: Json, writerSchema: Schema): Either[AvroError, A] =
    JsonDecoder(writerSchema)
      .flatMap(_.decode(json))
      .flatMap(Codec[A].decode(_, writerSchema))

  /** Returns a new [[Codec]] instance using the specified schema, and encode and decode functions.
    *
    * The decode function is called with values of the writer schema, which is neither a union nor a
    * reference. Values of unions are decoded using the schema of the written type.
    *
    * @group Create
    */
  final def instance[A](
    schema: Either[AvroError, Schema],
    encode: A => Either[AvroError, AvroValue],
    decode: (AvroValue, Schema) => Either[AvroError, A]
  ): Codec[A] =
    Instance(schema, encode, decode)

  /** @group General
    */
  implicit lazy val int: Codec[Int] =
    IntCodec

  /** @group Collection
    */
  implicit final def list[A](implicit codec: Codec[A]): Codec[List[A]] =
    ArrayCodec[A, List[A]](codec, _.length, _.iterator, _ => List.newBuilder)
      .withTypeName("List")

  /** @group General
    */
  implicit lazy val long: Codec[Long] =
    LongCodec

  /** @group Collection
    */
  implicit final def map[A](implicit codec: Codec[A]): Codec[Map[String, A]] =
    MapCodec[A, Map[String, A]](codec, _.size, _.iterator, _ => Map.newBuilder)
      .withTypeName("Map")

  /** @group Cats
    */
  implicit final def nonEmptyChain[A](implicit codec: Codec[A]): Codec[NonEmptyChain[A]] =
    ArrayCodec[A, Chain[A]](codec, _.length.toInt, _.iterator, chainBuilder)
      .imapError(NonEmptyChain.fromChain(_).toRight(AvroError.decodeEmptyCollection))(_.toChain)
      .withTypeName("NonEmptyChain")

  /** @group Cats
    */
  implicit final def nonEmptyList[A](implicit codec: Codec[A]): Codec[NonEmptyList[A]] =
    ArrayCodec[A, List[A]](codec, _.length, _.iterator, _ => List.newBuilder)
      .imapError(NonEmptyList.fromList(_).toRight(AvroError.decodeEmptyCollection))(_.toList)
      .withTypeName("NonEmptyList")

  /** @group Cats
    */
  implicit final def nonEmptyMap[A](implicit codec: Codec[A]): Codec[NonEmptyMap[String, A]] =
    MapCodec[A, SortedMap[String, A]](codec, _.size, _.iterator, _ => SortedMap.newBuilder)
      .imapError(NonEmptyMap.fromMap(_).toRight(AvroError.decodeEmptyCollection))(_.toSortedMap)
      .withTypeName("NonEmptyMap")

  /** @group Cats
    */
  implicit final def nonEmptySet[A](implicit
    codec: Codec[A],
    ordering: Ordering[A]
  ): Codec[NonEmptySet[A]] =
    ArrayCodec[A, SortedSet[A]](codec, _.size, _.iterator, _ => SortedSet.newBuilder)
      .imapError(NonEmptySet.fromSet(_).toRight(AvroError.decodeEmptyCollection))(_.toSortedSet)
      .withTypeName("NonEmptySet")

  /** @group Cats
    */
  implicit final def nonEmptyVector[A](implicit codec: Codec[A]): Codec[NonEmptyVector[A]] =
    ArrayCodec[A, Vector[A]](codec, _.length, _.iterator, vectorBuilder)
      .imapError(NonEmptyVector.fromVector(_).toRight(AvroError.decodeEmptyCollection))(_.toVector)
      .withTypeName("NonEmptyVector")

  /** @group Collection
    */
  implicit final def seq[A](implicit codec: Codec[A]): Codec[Seq[A]] =
    ArrayCodec[A, Seq[A]](codec, _.length, _.iterator, _ => Seq.newBuilder)
      .withTypeName("Seq")

  /** @group Collection
    */
  implicit final def set[A](implicit codec: Codec[A]): Codec[Set[A]] =
    ArrayCodec[A, Set[A]](codec, _.size, _.iterator, _ => Set.newBuilder)
      .withTypeName("Set")

  /** @group General
    */
  implicit lazy val short: Codec[Short] =
    Codec.int
      .imapError { int =>
        if (int.isValidShort) Right(int.toShort)
        else Left(AvroError.unexpectedShort(int))
      }(_.toInt)
      .withTypeName("Short")

  /** @group General
    */
  implicit lazy val string: Codec[String] =
    StringCodec

  /** Returns the result of encoding the specified value to Avro binary.
    *
    * @group Utilities
    */
  final def toBinary[A: Codec](a: A): Either[AvroError, ByteVector] = {
    val codec = Codec[A]
    codec.binaryEncoder.flatMap(encoder => codec.encode(a).flatMap(encoder.encode))
  }

  /** Returns the result of encoding the specified value to Avro JSON.
    *
    * @group Utilities
    */
  final def toJson[A: Codec](a: A): Either[AvroError, Json] = {
    val codec = Codec[A]
    codec.jsonEncoder.flatMap(encoder => codec.encode(a).flatMap(encoder.encode))
  }

  /** @group General
    */
  implicit lazy val unit: Codec[Unit] =
    NullCodec((), "Unit")

  /** @group Collection
    */
  implicit final def vector[A](implicit codec: Codec[A]): Codec[Vector[A]] =
    ArrayCodec[A, Vector[A]](codec, _.length, _.iterator, vectorBuilder)
      .withTypeName("Vector")

  /** @group Cats
    */
  implicit final val codecInvariant: Invariant[Codec] =
    new Invariant[Codec] {
      override final def imap[A, B](codec: Codec[A])(f: A => B)(g: B => A): Codec[B] =
        codec.imap(f)(g)
    }

  /** @group Cats
    */
  implicit final def codecShow[A]: Show[Codec[A]] =
    Show.fromToString

  private[v2] final case class ImapErrors[A, B](
    codec: Codec[A],
    f: A => Either[AvroError, B],
    g: B => Either[AvroError, A]
  ) extends Codec[B] {
    override def schema: Either[AvroError, Schema] =
      codec.schema

    override def encode(b: B): Either[AvroError, AvroValue] =
      g(b).flatMap(codec.encode)

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, B] = {
      val decode = codec.resolve(writerSchema, definitions)
      decode(_).flatMap(f)
    }
  }

  private[v2] final case class Instance[A](
    instanceSchema: Either[AvroError, Schema],
    instanceEncode: A => Either[AvroError, AvroValue],
    instanceDecode: (AvroValue, Schema) => Either[AvroError, A]
  ) extends Codec[A] {
    override lazy val schema: Either[AvroError, Schema] =
      instanceSchema.flatMap { schema =>
        SchemaValidator.validate(schema, validateDefaults = true).map(_ => schema)
      }

    override def encode(a: A): Either[AvroError, AvroValue] =
      instanceEncode(a)

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, A] =
      instanceDecode(_, writerSchema)
  }

  private[v2] final case class WithTypeName[A](
    codec: Codec[A],
    typeName: String
  ) extends Codec[A] {
    override def schema: Either[AvroError, Schema] =
      codec.schema

    override def encode(a: A): Either[AvroError, AvroValue] =
      codec.encode(a).left.map(AvroError.errorEncodingFrom(typeName, _))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, A] = {
      val decode = codec.resolve(writerSchema, definitions)
      decode(_).left.map(AvroError.errorDecodingTo(typeName, _))
    }
  }

  /** Codec for collections encoded as arrays, with a builder for the number of items. */
  private[v2] final case class ArrayCodec[A, C](
    items: Codec[A],
    size: C => Int,
    iterator: C => Iterator[A],
    newBuilder: Int => mutable.Builder[A, C]
  ) extends Codec[C] {
    override lazy val schema: Either[AvroError, Schema] =
      items.schema.map(Schema.Array(_))

    override def encode(collection: C): Either[AvroError, AvroValue] =
      build(iterator(collection), vectorBuilder[AvroValue](size(collection)))(items.encode)
        .map(AvroValue.Array(_))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, C] =
      writerSchema match {
        case array: Schema.Array =>
          val decode = items.decoder(array.items, definitions)
          value =>
            value match {
              case AvroValue.Array(values) =>
                build(values.iterator, newBuilder(values.size))(decode)

              case _ =>
                Left(invalidValue(value, writerSchema))
            }

        case _ =>
          failing(unexpectedSchemaType(writerSchema, "array"))
      }
  }

  /** Codec for maps with string keys, with a builder for the number of entries. */
  private[v2] final case class MapCodec[A, M](
    values: Codec[A],
    size: M => Int,
    iterator: M => Iterator[(String, A)],
    newBuilder: Int => mutable.Builder[(String, A), M]
  ) extends Codec[M] {
    override lazy val schema: Either[AvroError, Schema] =
      values.schema.map(Schema.Map(_))

    override def encode(map: M): Either[AvroError, AvroValue] =
      build(iterator(map), vectorBuilder[(String, AvroValue)](size(map))) { case (key, value) =>
        values.encode(value).map((key, _))
      }.map(AvroValue.Map(_))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, M] =
      writerSchema match {
        case map: Schema.Map =>
          val decode = values.decoder(map.values, definitions)
          value =>
            value match {
              case AvroValue.Map(entries) =>
                build(entries.iterator, newBuilder(entries.size)) { case (key, entryValue) =>
                  decode(entryValue).map((key, _))
                }

              case _ =>
                Left(invalidValue(value, writerSchema))
            }

        case _ =>
          failing(unexpectedSchemaType(writerSchema, "map"))
      }
  }

  /** Codec for a primitive type, which also decodes types which can be promoted to the type. */
  private[v2] sealed abstract class PrimitiveCodec[A](
    val typeName: String,
    primitiveSchema: Schema
  ) extends Codec[A] {
    override final val schema: Either[AvroError, Schema] =
      Right(primitiveSchema)

    protected final def invalid(value: AvroValue, writerSchema: Schema): Either[AvroError, A] =
      Left(AvroError.errorDecodingTo(typeName, invalidValue(value, writerSchema)))

    protected final def unexpected(writerSchema: Schema): AvroValue => Either[AvroError, A] =
      failing {
        AvroError.errorDecodingTo(
          typeName,
          unexpectedSchemaType(writerSchema, SchemaNames.typeName(primitiveSchema))
        )
      }
  }

  private[v2] final case class NullCodec[A](
    value: A,
    override val typeName: String
  ) extends PrimitiveCodec[A](typeName, Schema.Null) {
    private val encoded: Either[AvroError, AvroValue] =
      Right(AvroValue.Null)

    private val decoded: Either[AvroError, A] =
      Right(value)

    override def encode(a: A): Either[AvroError, AvroValue] =
      encoded

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, A] =
      writerSchema match {
        case _: Schema.Null => {
          case AvroValue.Null => decoded
          case value          => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[v2] case object BooleanCodec extends PrimitiveCodec[Boolean]("Boolean", Schema.Boolean) {
    override def encode(boolean: Boolean): Either[AvroError, AvroValue] =
      Right(AvroValue.Boolean(boolean))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, Boolean] =
      writerSchema match {
        case _: Schema.Boolean => {
          case AvroValue.Boolean(boolean) => Right(boolean)
          case value                      => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[v2] case object IntCodec extends PrimitiveCodec[Int]("Int", Schema.Int) {
    override def encode(int: Int): Either[AvroError, AvroValue] =
      Right(AvroValue.Int(int))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, Int] =
      writerSchema match {
        case _: Schema.Int => {
          case AvroValue.Int(int) => Right(int)
          case value              => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[v2] case object LongCodec extends PrimitiveCodec[Long]("Long", Schema.Long) {
    override def encode(long: Long): Either[AvroError, AvroValue] =
      Right(AvroValue.Long(long))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, Long] =
      writerSchema match {
        case _: Schema.Long => {
          case AvroValue.Long(long) => Right(long)
          case value                => invalid(value, writerSchema)
        }

        case _: Schema.Int => {
          case AvroValue.Int(int) => Right(int.toLong)
          case value              => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[v2] case object FloatCodec extends PrimitiveCodec[Float]("Float", Schema.Float) {
    override def encode(float: Float): Either[AvroError, AvroValue] =
      Right(AvroValue.Float(float))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, Float] =
      writerSchema match {
        case _: Schema.Float => {
          case AvroValue.Float(float) => Right(float)
          case value                  => invalid(value, writerSchema)
        }

        case _: Schema.Int => {
          case AvroValue.Int(int) => Right(int.toFloat)
          case value              => invalid(value, writerSchema)
        }

        case _: Schema.Long => {
          case AvroValue.Long(long) => Right(long.toFloat)
          case value                => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[v2] case object DoubleCodec extends PrimitiveCodec[Double]("Double", Schema.Double) {
    override def encode(double: Double): Either[AvroError, AvroValue] =
      Right(AvroValue.Double(double))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, Double] =
      writerSchema match {
        case _: Schema.Double => {
          case AvroValue.Double(double) => Right(double)
          case value                    => invalid(value, writerSchema)
        }

        case _: Schema.Float => {
          case AvroValue.Float(float) => Right(float.toDouble)
          case value                  => invalid(value, writerSchema)
        }

        case _: Schema.Int => {
          case AvroValue.Int(int) => Right(int.toDouble)
          case value              => invalid(value, writerSchema)
        }

        case _: Schema.Long => {
          case AvroValue.Long(long) => Right(long.toDouble)
          case value                => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[v2] case object BytesCodec
      extends PrimitiveCodec[ByteVector]("ByteVector", Schema.Bytes) {
    override def encode(bytes: ByteVector): Either[AvroError, AvroValue] =
      Right(AvroValue.Bytes(bytes))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, ByteVector] =
      writerSchema match {
        case _: Schema.Bytes => {
          case AvroValue.Bytes(bytes) => Right(bytes)
          case value                  => invalid(value, writerSchema)
        }

        case _: Schema.String => {
          case AvroValue.String(string) =>
            Right(ByteVector.view(string.getBytes(StandardCharsets.UTF_8)))
          case value =>
            invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[v2] case object StringCodec extends PrimitiveCodec[String]("String", Schema.String) {
    override def encode(string: String): Either[AvroError, AvroValue] =
      Right(AvroValue.String(string))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, String] =
      writerSchema match {
        case _: Schema.String => {
          case AvroValue.String(string) => Right(string)
          case value                    => invalid(value, writerSchema)
        }

        case _: Schema.Bytes => {
          case AvroValue.Bytes(bytes) => Right(new String(bytes.toArray, StandardCharsets.UTF_8))
          case value                  => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  /** Decodes values of a writer union using the written type, resolving types when first used. */
  private final class UnionDecoder[A](
    codec: Codec[A],
    union: Schema.Union,
    definitions: Definitions
  ) extends (AvroValue => Either[AvroError, A]) {
    private val types: Array[Schema] =
      union.types.toArray

    private val decoders =
      new AtomicReferenceArray[AvroValue => Either[AvroError, A]](types.length)

    override def apply(value: AvroValue): Either[AvroError, A] =
      value match {
        case AvroValue.Union(index, typeValue) if index >= 0 && index < types.length =>
          decoderOf(index)(typeValue)

        case AvroValue.Union(index, _) =>
          Left(AvroError(s"Invalid index $index for union with ${types.length} types"))

        case _ =>
          Left(invalidValue(value, union))
      }

    private def decoderOf(index: Int): AvroValue => Either[AvroError, A] = {
      val existing = decoders.get(index)
      if (existing != null) existing
      else {
        val decoder = codec.decoder(types(index), definitions)
        decoders.set(index, decoder)
        decoder
      }
    }
  }

  /** Builds vectors of the given number of items without copying them, unlike `VectorBuilder`. */
  private final class ExactVectorBuilder[A](size: Int) extends mutable.Builder[A, Vector[A]] {
    private[this] var items: Array[AnyRef] =
      new Array(size)

    private[this] var count: Int =
      0

    override def addOne(item: A): this.type = {
      items(count) = item.asInstanceOf[AnyRef]
      count += 1
      this
    }

    override def clear(): Unit = {
      items = new Array(size)
      count = 0
    }

    /** The items, which are not copied by `Vector.from` if there are at most 32. */
    override def result(): Vector[A] = {
      val exact = if (count == items.length) items else Arrays.copyOf(items, count)
      Vector.from(ArraySeq.unsafeWrapArray(exact).asInstanceOf[ArraySeq[A]])
    }
  }

  /** A builder for vectors of the given number of items, which only copies items if needed. */
  private def vectorBuilder[A](size: Int): mutable.Builder[A, Vector[A]] =
    if (size <= 32) new ExactVectorBuilder(size) else Vector.newBuilder

  private def chainBuilder[A](size: Int): mutable.Builder[A, Chain[A]] =
    vectorBuilder[A](size).mapResult(Chain.fromSeq)

  /** Builds the results of the function on the values, stopping at the first error. */
  private def build[A, B, C](
    values: Iterator[A],
    builder: mutable.Builder[B, C]
  )(f: A => Either[AvroError, B]): Either[AvroError, C] = {
    @tailrec def loop(): Either[AvroError, C] =
      if (values.hasNext)
        f(values.next()) match {
          case Right(value) =>
            builder += value
            loop()

          case Left(error) =>
            Left(error)
        }
      else Right(builder.result())

    loop()
  }

  /** A function decoding any value to the error. */
  private[v2] def failing[A](error: AvroError): AvroValue => Either[AvroError, A] = {
    val result = Left(error)
    _ => result
  }

  private[v2] def invalidValue(value: AvroValue, writerSchema: Schema): AvroError =
    AvroError(
      s"Invalid ${ValueNames.typeName(value)} value for ${SchemaNames.describe(writerSchema)}"
    )

  private[v2] def unexpectedSchemaType(writerSchema: Schema, expectedType: String): AvroError =
    AvroError(
      s"Got unexpected schema type ${SchemaNames.typeName(writerSchema)}, expected schema type $expectedType"
    )
}
