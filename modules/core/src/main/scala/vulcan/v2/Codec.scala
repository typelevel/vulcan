/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import cats.{~>, Invariant, Show}
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector}
import cats.free.FreeApplicative
import io.circe.{Json, JsonObject}
import java.nio.charset.StandardCharsets
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZoneOffset}
import java.util.concurrent.atomic.AtomicReferenceArray
import java.util.{Arrays, UUID}
import scala.annotation.{implicitNotFound, tailrec}
import scala.collection.immutable.{ArraySeq, SortedMap, SortedSet}
import scala.collection.mutable
import scala.util.Try
import scodec.bits.ByteVector
import vulcan.{AvroError, Prism}
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

  private[v2] final def withLogicalType(logicalType: LogicalType): Codec[A] =
    Codec.LogicalTypeCodec(this, logicalType)

  /** Encodes the value as JSON for defaults in schemas, where unions are without the type. */
  private[v2] final def encodeForSchema(a: A): Either[AvroError, Json] =
    for {
      validSchema <- schema
      encoder <- JsonEncoder.withoutUnionTypes(validSchema)
      value <- encode(a)
      json <- encoder.encode(value)
    } yield json

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
  * @groupname JavaTime Java Time Codecs
  * @groupprio JavaTime 3
  * @groupdesc JavaTime
  *   Default codecs for `java.time` types.
  *
  * @groupname JavaUtil Java Util Codecs
  * @groupprio JavaUtil 4
  * @groupdesc JavaUtil
  *   Default codecs for `java.util` types.
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

  /** Returns a new decimal [[Codec]] for `BigDecimal`.
    *
    * Values are decoded using the precision and scale of the writer schema.
    *
    * @group Create
    */
  final def decimal(precision: Int, scale: Int): Codec[BigDecimal] =
    DecimalCodec(precision, scale).withTypeName("BigDecimal")

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

  /** @group General
    */
  implicit final def either[A, B](implicit
    codecA: Codec[A],
    codecB: Codec[B]
  ): Codec[Either[A, B]] =
    Codec
      .union[Either[A, B]](alt => alt[Left[A, B]] ++ alt[Right[A, B]])
      .withTypeName("Either")

  /** Returns the result of encoding the specified value.
    *
    * @group Utilities
    */
  final def encode[A](a: A)(implicit codec: Codec[A]): Either[AvroError, AvroValue] =
    codec.encode(a)

  /** Returns a new enum [[Codec]] for type `A`.
    *
    * Symbols which are not part of the enum decode to the default, if there is one.
    *
    * @group Create
    */
  final def enumeration[A](
    name: String,
    namespace: String,
    symbols: Seq[String],
    encode: A => String,
    decode: String => Either[AvroError, A],
    default: Option[A] = None,
    doc: Option[String] = None,
    aliases: Seq[String] = Seq.empty,
    props: JsonObject = JsonObject.empty
  ): Codec[A] = {
    val enumName = SchemaNames.resolve(name, Some(namespace))
    EnumCodec(enumName, symbols.toVector, encode, decode, default, doc, aliases.toVector, props)
      .withTypeName(enumName.fullName)
  }

  /** Returns a new fixed [[Codec]] for type `A`.
    *
    * When encoding, bytes are zero-padded to the specified size. Zero-padding is applied at the
    * end, and will remain in the input to `decode`. Encoding checks to ensure the size is not
    * exceeded, while decoding ensures the exact size.
    *
    * @group Create
    */
  final def fixed[A](
    name: String,
    namespace: String,
    size: Int,
    encode: A => ByteVector,
    decode: ByteVector => Either[AvroError, A],
    doc: Option[String] = None,
    aliases: Seq[String] = Seq.empty,
    props: JsonObject = JsonObject.empty
  ): Codec[A] = {
    val fixedName = SchemaNames.resolve(name, Some(namespace))
    FixedCodec(fixedName, size, encode, decode, doc, aliases.toVector, props)
      .withTypeName(fixedName.fullName)
  }

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

  /** @group JavaTime
    */
  implicit lazy val instant: Codec[Instant] =
    Codec.long
      .imapErrors(millis => Right(Instant.ofEpochMilli(millis))) { instant =>
        epochTime(instant, "milliseconds")(instant.toEpochMilli)
      }
      .withLogicalType(LogicalType.timestampMillis)
      .withTypeName("Instant")

  /** `Instant` with microsecond precision, encoded with logical type `timestamp-micros`.
    *
    * @group JavaTime
    */
  final case class InstantMicros(value: Instant) {

    /** The microseconds since the epoch, throwing `ArithmeticException` if exceeding a long. */
    def toEpochMicro: Long = {
      val seconds = value.getEpochSecond
      val micros = value.getNano / 1000L

      // Avoids overflowing for the earliest instants, by not multiplying the floor of the seconds
      if (seconds < 0 && micros > 0)
        Math.addExact(Math.multiplyExact(seconds + 1, 1000000L), micros - 1000000L)
      else Math.addExact(Math.multiplyExact(seconds, 1000000L), micros)
    }
  }

  object InstantMicros {
    def fromEpochMicro(epochMicros: Long): InstantMicros =
      InstantMicros(
        Instant.ofEpochSecond(
          Math.floorDiv(epochMicros, 1000000L),
          Math.floorMod(epochMicros, 1000000L) * 1000L
        )
      )

    override def toString: String =
      "InstantMicros"
  }

  /** @group JavaTime
    */
  implicit lazy val instantMicros: Codec[InstantMicros] =
    Codec.long
      .imapErrors(micros => Right(InstantMicros.fromEpochMicro(micros))) { instantMicros =>
        epochTime(instantMicros.value, "microseconds")(instantMicros.toEpochMicro)
      }
      .withLogicalType(LogicalType.timestampMicros)
      .withTypeName("InstantMicros")

  /** @group General
    */
  implicit lazy val int: Codec[Int] =
    IntCodec

  /** @group General
    */
  implicit final def left[A, B](implicit codec: Codec[A]): Codec[Left[A, B]] =
    codec.imap(Left[A, B](_))(_.value)

  /** @group Collection
    */
  implicit final def list[A](implicit codec: Codec[A]): Codec[List[A]] =
    ArrayCodec[A, List[A]](codec, _.length, _.iterator, _ => List.newBuilder)
      .withTypeName("List")

  /** @group JavaTime
    */
  implicit lazy val localDate: Codec[LocalDate] =
    Codec.int
      .imapErrors(days => Right(LocalDate.ofEpochDay(days.toLong))) { date =>
        val days = date.toEpochDay
        if (days.isValidInt) Right(days.toInt)
        else Left(AvroError.encodeDateSizeExceeded(date))
      }
      .withLogicalType(LogicalType.date)
      .withTypeName("LocalDate")

  /** @group JavaTime
    */
  implicit lazy val localDateTime: Codec[LocalDateTime] =
    Codec.long
      .imapErrors { millis =>
        Right(LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC))
      } { dateTime =>
        epochTime(dateTime, "milliseconds")(dateTime.toInstant(ZoneOffset.UTC).toEpochMilli)
      }
      .withLogicalType(LogicalType.localTimestampMillis)
      .withTypeName("LocalDateTime")

  /** @group JavaTime
    */
  final lazy val localTimeMicros: Codec[LocalTime] =
    Codec.long
      .imapError { micros =>
        if (micros >= 0 && micros < MicrosPerDay) Right(LocalTime.ofNanoOfDay(micros * 1000L))
        else Left(unexpectedTime("Long", micros, MicrosPerDay))
      }(_.toNanoOfDay / 1000L)
      .withLogicalType(LogicalType.timeMicros)
      .withTypeName("LocalTime")

  /** @group JavaTime
    */
  final lazy val localTimeMillis: Codec[LocalTime] =
    Codec.int
      .imapError { millis =>
        if (millis >= 0 && millis < MillisPerDay) Right(LocalTime.ofNanoOfDay(millis * 1000000L))
        else Left(unexpectedTime("Int", millis.toLong, MillisPerDay.toLong))
      }(time => (time.toNanoOfDay / 1000000L).toInt)
      .withLogicalType(LogicalType.timeMillis)
      .withTypeName("LocalTime")

  /** @group General
    */
  implicit lazy val long: Codec[Long] =
    LongCodec

  /** @group Collection
    */
  implicit final def map[A](implicit codec: Codec[A]): Codec[Map[String, A]] =
    MapCodec[A, Map[String, A]](codec, _.size, _.iterator, _ => Map.newBuilder)
      .withTypeName("Map")

  /** @group General
    */
  implicit lazy val none: Codec[None.type] =
    NullCodec(None, "None")

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

  /** Returns a [[Codec]] for options, encoded as a union with `null` first.
    *
    * If the underlying codec is for a union, the types of that union are included in the union.
    *
    * @group General
    */
  implicit final def option[A](implicit codec: Codec[A]): Codec[Option[A]] =
    OptionCodec(codec).withTypeName("Option")

  /** Returns a new record [[Codec]] for type `A`.
    *
    * Fields are decoded from the writer field with the same name or the name of an alias of the
    * field, and otherwise using the default of the field. Other writer fields are ignored.
    *
    * @group Create
    */
  final def record[A](
    name: String,
    namespace: String,
    doc: Option[String] = None,
    aliases: Seq[String] = Seq.empty,
    props: JsonObject = JsonObject.empty
  )(f: FieldBuilder[A] => FreeApplicative[Field[A, *], A]): Codec[A] = {
    val recordName = SchemaNames.resolve(name, Some(namespace))
    RecordCodec(recordName, doc, aliases.toVector, props, f(FieldBuilder.instance))
      .withTypeName(recordName.fullName)
  }

  /** @group General
    */
  implicit final def right[A, B](implicit codec: Codec[B]): Codec[Right[A, B]] =
    codec.imap(Right[A, B](_))(_.value)

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
  implicit final def some[A](implicit codec: Codec[A]): Codec[Some[A]] =
    codec.imap(Some(_))(_.value)

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

  /** Returns a new union [[Codec]] for type `A`.
    *
    * Values are encoded with the first alternative which includes the value. Values are decoded
    * with the alternative for the written type: the same type, or a named type with the same full
    * name or alias, then the same name, and otherwise the first type the written type can be
    * promoted to.
    *
    * @group Create
    */
  final def union[A](f: AltBuilder[A] => Chain[Alt[A]]): Codec[A] =
    UnionCodec(f(AltBuilder.instance))

  /** @group General
    */
  implicit lazy val unit: Codec[Unit] =
    NullCodec((), "Unit")

  /** @group JavaUtil
    */
  implicit lazy val uuid: Codec[UUID] =
    Codec.string
      .imapError { string =>
        try Right(UUID.fromString(string))
        catch {
          case _: IllegalArgumentException => Left(AvroError(s"Invalid UUID string: $string"))
        }
      }(_.toString)
      .withLogicalType(LogicalType.uuid)
      .withTypeName("UUID")

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

  /** @group Create
    */
  sealed abstract class Alt[A] {
    type B

    def codec: Codec[B]

    def prism: Prism[A, B]

    def imap[A0](f: A0 => Option[A], g: A => A0): Alt[A0] =
      Alt(codec, prism.imap(f, g))
  }

  private[v2] object Alt {
    final def apply[A, B0](
      _codec: Codec[B0],
      _prism: Prism[A, B0]
    ): Alt[A] =
      new Alt[A] {
        override final type B = B0
        override final val codec: Codec[B] = _codec
        override final val prism: Prism[A, B] = _prism
      }
  }

  /** @group Create
    */
  sealed abstract class AltBuilder[A] {
    def apply[B](implicit codec: Codec[B], prism: Prism[A, B]): Chain[Alt[A]]
  }

  private[v2] object AltBuilder {
    private[this] final val Instance: AltBuilder[Any] =
      new AltBuilder[Any] {
        override final def apply[B](implicit
          codec: Codec[B],
          prism: Prism[Any, B]
        ): Chain[Alt[Any]] =
          Chain.one(Alt(codec, prism))

        override final def toString: String =
          "AltBuilder"
      }

    final def instance[A]: AltBuilder[A] =
      Instance.asInstanceOf[AltBuilder[A]]
  }

  /** @group Create
    */
  sealed abstract class Field[A, B] {
    def name: String

    def access: A => B

    def codec: Codec[B]

    def doc: Option[String]

    def default: Option[B]

    def order: Option[Schema.Field.Order]

    def aliases: Seq[String]

    def props: JsonObject
  }

  /** @group Create
    */
  sealed abstract class FieldBuilder[A] {
    def apply[B](
      name: String,
      access: A => B,
      doc: Option[String] = None,
      default: Option[B] = None,
      order: Option[Schema.Field.Order] = None,
      aliases: Seq[String] = Seq.empty,
      props: JsonObject = JsonObject.empty
    )(implicit codec: Codec[B]): FreeApplicative[Field[A, *], B]
  }

  private[v2] object FieldBuilder {
    private[this] final val Instance: FieldBuilder[Any] =
      new FieldBuilder[Any] {
        override final def apply[B](
          name: String,
          access: Any => B,
          doc: Option[String],
          default: Option[B],
          order: Option[Schema.Field.Order],
          aliases: Seq[String],
          props: JsonObject
        )(implicit codec: Codec[B]): FreeApplicative[Field[Any, *], B] = {
          val _name = name
          val _access = access
          val _codec = codec
          val _doc = doc
          val _default = default
          val _order = order
          val _aliases = aliases
          val _props = props

          FreeApplicative.lift {
            new Field[Any, B] {
              override val name: String = _name
              override val access: Any => B = _access
              override val codec: Codec[B] = _codec
              override val doc: Option[String] = _doc
              override val default: Option[B] = _default
              override val order: Option[Schema.Field.Order] = _order
              override val aliases: Seq[String] = _aliases
              override val props: JsonObject = _props
            }
          }
        }

        override final def toString: String =
          "FieldBuilder"
      }

    final def instance[A]: FieldBuilder[A] =
      Instance.asInstanceOf[FieldBuilder[A]]
  }

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

  /** Codec for options, as unions with `null` first, including the types of unions. */
  private[v2] final case class OptionCodec[A](codec: Codec[A]) extends Codec[Option[A]] {
    override lazy val schema: Either[AvroError, Schema] =
      codec.schema.flatMap { schema =>
        val types =
          schema match {
            case union: Schema.Union => union.types
            case _                   => Vector(schema)
          }

        validated(Schema.Union(Schema.Null +: types))
      }

    /** Whether the types of the union of the codec are included in the union of the option. */
    private lazy val includesUnion: Boolean =
      codec.schema.exists(_.isInstanceOf[Schema.Union])

    override def encode(option: Option[A]): Either[AvroError, AvroValue] =
      (schema, option) match {
        case (Right(_), Some(a)) =>
          codec.encode(a).flatMap {
            case AvroValue.Union(index, value) if includesUnion =>
              Right(AvroValue.Union(index + 1, value))

            case value if !includesUnion =>
              Right(AvroValue.Union(1, value))

            case value =>
              Left(AvroError(s"Invalid ${ValueNames.typeName(value)} value for union"))
          }

        case (Right(_), None) =>
          EncodedNone

        case (Left(error), _) =>
          Left(error)
      }

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, Option[A]] =
      (schema, writerSchema) match {
        case (Left(error), _) =>
          failing(error)

        case (Right(_), _: Schema.Null) => {
          case AvroValue.Null => DecodedNone
          case value          => Left(invalidValue(value, writerSchema))
        }

        case (Right(_), _) =>
          val decode = codec.resolve(writerSchema, definitions)
          decode(_).map(Some(_))
      }
  }

  /** Codec for unions, which decodes using the alternative for the written type. */
  private[v2] final case class UnionCodec[A](alternatives: Chain[Alt[A]]) extends Codec[A] {
    private val alts: Vector[Alt[A]] =
      alternatives.toVector

    private lazy val types: Either[AvroError, Vector[Schema]] =
      build(alts.iterator, vectorBuilder[Schema](alts.size))(_.codec.schema)

    override lazy val schema: Either[AvroError, Schema] =
      types.flatMap(types => validated(Schema.Union(types)))

    override def encode(a: A): Either[AvroError, AvroValue] =
      schema match {
        case Right(_)    => encodeWith(a, 0)
        case Left(error) => Left(error)
      }

    @tailrec private def encodeWith(a: A, index: Int): Either[AvroError, AvroValue] =
      if (index < alts.length) {
        val alt = alts(index)
        alt.prism.getOption(a) match {
          case Some(value) => alt.codec.encode(value).map(AvroValue.Union(index, _))
          case None        => encodeWith(a, index + 1)
        }
      } else Left(AvroError("Exhausted alternatives for union"))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, A] =
      schema.flatMap(_ => types) match {
        case Right(types) =>
          unionType(writerSchema, types) match {
            case Some(index) =>
              val alt = alts(index)
              val decode = alt.codec.resolve(writerSchema, definitions)
              decode(_).map(alt.prism.reverseGet)

            case None =>
              failing {
                AvroError(s"Missing alternative for ${SchemaNames.describe(writerSchema)} in union")
              }
          }

        case Left(error) =>
          failing(error)
      }
  }

  /** Codec for records, which decodes fields from the writer field with the same name or an alias
    * of the field, and otherwise uses the default of the field.
    */
  private[v2] final case class RecordCodec[A](
    name: Schema.Name,
    doc: Option[String],
    aliases: Vector[String],
    props: JsonObject,
    fields: FreeApplicative[Field[A, *], A]
  ) extends Codec[A] {

    /** The fields, in the order of the record. */
    private lazy val fieldVector: Vector[Field[A, _]] =
      fields.analyze(new (Field[A, *] ~> λ[x => Vector[Field[A, _]]]) {
        override def apply[B](field: Field[A, B]): Vector[Field[A, _]] = Vector(field)
      })

    /** Returns a value from the values of the fields, in the order of the record. */
    private lazy val construct: Array[Any] => A =
      fields.foldMap(new (Field[A, *] ~> λ[x => Array[Any] => x]) {
        override def apply[B](field: Field[A, B]): Array[Any] => B = {
          val index = fieldVector.indexWhere(_ eq field)
          _(index).asInstanceOf[B]
        }
      })

    private lazy val encoders: Vector[A => Either[AvroError, AvroValue]] =
      fieldVector.map(fieldEncoder(_))

    override lazy val schema: Either[AvroError, Schema] =
      build(fieldVector.iterator, vectorBuilder[Schema.Field](fieldVector.size))(fieldSchema(_))
        .flatMap { fields =>
          validated {
            Schema.Record(name, fields).withDoc(doc).withAliases(aliases).withProps(props)
          }
        }

    override def encode(a: A): Either[AvroError, AvroValue] =
      schema match {
        case Right(_) =>
          build(encoders.iterator, vectorBuilder[AvroValue](encoders.size))(_(a))
            .map(AvroValue.Record(_))

        case Left(error) =>
          Left(error)
      }

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, A] =
      (schema, writerSchema) match {
        case (Left(error), _) =>
          failing(error)

        case (Right(_), record: Schema.Record) =>
          val decoders = fieldVector.map(fieldDecoder(_, record, definitions)).toArray
          value =>
            value match {
              case AvroValue.Record(values) if values.size == record.fields.size =>
                decodeFields(values, decoders)

              case AvroValue.Record(values) =>
                Left {
                  AvroError(
                    s"Invalid record with ${values.size} values for record ${record.fullName} with ${record.fields.size} fields"
                  )
                }

              case _ =>
                Left(invalidValue(value, writerSchema))
            }

        case (Right(_), _) =>
          failing(unexpectedSchemaType(writerSchema, "record"))
      }

    private def decodeFields(
      values: Vector[AvroValue],
      decoders: Array[Vector[AvroValue] => Either[AvroError, Any]]
    ): Either[AvroError, A] = {
      val decoded = new Array[Any](decoders.length)

      @tailrec def loop(index: Int): Either[AvroError, A] =
        if (index < decoders.length)
          decoders(index)(values) match {
            case Right(value) =>
              decoded(index) = value
              loop(index + 1)

            case Left(error) =>
              Left(error)
          }
        else Right(construct(decoded))

      loop(0)
    }
  }

  /** Codec for enums, where unknown symbols decode to the default, if there is one. */
  private[v2] final case class EnumCodec[A](
    name: Schema.Name,
    symbols: Vector[String],
    encodeSymbol: A => String,
    decodeSymbol: String => Either[AvroError, A],
    default: Option[A],
    doc: Option[String],
    aliases: Vector[String],
    props: JsonObject
  ) extends Codec[A] {
    private val symbolSet: Set[String] =
      symbols.toSet

    override lazy val schema: Either[AvroError, Schema] =
      validated {
        Schema
          .Enum(name, symbols)
          .withDefault(default.map(encodeSymbol))
          .withDoc(doc)
          .withAliases(aliases)
          .withProps(props)
      }

    override def encode(a: A): Either[AvroError, AvroValue] =
      schema match {
        case Right(_) =>
          val symbol = encodeSymbol(a)
          if (symbolSet.contains(symbol)) Right(AvroValue.Enum(symbol))
          else Left(AvroError.encodeSymbolNotInSchema(symbol, symbols))

        case Left(error) =>
          Left(error)
      }

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, A] =
      (schema, writerSchema) match {
        case (Left(error), _) =>
          failing(error)

        case (Right(_), _: Schema.Enum) => {
          case AvroValue.Enum(symbol) =>
            if (symbolSet.contains(symbol)) decodeSymbol(symbol)
            else default.toRight(AvroError.decodeSymbolNotInSchema(symbol, symbols))

          case value =>
            Left(invalidValue(value, writerSchema))
        }

        case (Right(_), _) =>
          failing(unexpectedSchemaType(writerSchema, "enum"))
      }
  }

  /** Codec for fixed, which pads bytes when encoding, and requires the same size when decoding. */
  private[v2] final case class FixedCodec[A](
    name: Schema.Name,
    size: Int,
    encodeBytes: A => ByteVector,
    decodeBytes: ByteVector => Either[AvroError, A],
    doc: Option[String],
    aliases: Vector[String],
    props: JsonObject
  ) extends Codec[A] {
    override lazy val schema: Either[AvroError, Schema] =
      validated(Schema.Fixed(name, size).withDoc(doc).withAliases(aliases).withProps(props))

    override def encode(a: A): Either[AvroError, AvroValue] =
      schema match {
        case Right(_) =>
          val bytes = encodeBytes(a)
          if (bytes.size == size) Right(AvroValue.Fixed(bytes))
          else if (bytes.size < size) Right(AvroValue.Fixed(bytes.padRight(size.toLong)))
          else Left(AvroError.encodeExceedsFixedSize(bytes.size.toInt, size))

        case Left(error) =>
          Left(error)
      }

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, A] =
      (schema, writerSchema) match {
        case (Left(error), _) =>
          failing(error)

        case (Right(_), fixed: Schema.Fixed) if fixed.size == size => {
          case AvroValue.Fixed(bytes) =>
            if (bytes.size == size) decodeBytes(bytes)
            else Left(AvroError.decodeNotEqualFixedSize(bytes.size.toInt, size))

          case value =>
            Left(invalidValue(value, writerSchema))
        }

        case (Right(_), fixed: Schema.Fixed) =>
          failing(AvroError(s"Got unexpected fixed size ${fixed.size}, expected fixed size $size"))

        case (Right(_), _) =>
          failing(unexpectedSchemaType(writerSchema, "fixed"))
      }
  }

  /** Codec for decimals, which decodes using the precision and scale of the writer schema. */
  private[v2] final case class DecimalCodec(precision: Int, scale: Int) extends Codec[BigDecimal] {
    override val schema: Either[AvroError, Schema] =
      if (precision <= 0)
        Left(AvroError(s"Invalid decimal precision $precision: must be positive"))
      else if (scale < 0)
        Left(AvroError(s"Invalid decimal scale $scale: cannot be negative"))
      else if (scale > precision)
        Left(AvroError(s"Invalid decimal scale $scale: cannot exceed precision $precision"))
      else
        Right(Schema.Bytes.withLogicalType(LogicalType.decimal(precision, scale)))

    override def encode(decimal: BigDecimal): Either[AvroError, AvroValue] =
      schema match {
        case Right(_) if decimal.scale != scale =>
          Left(AvroError.encodeDecimalScalesMismatch(decimal.scale, scale))

        case Right(_) if decimal.precision > precision =>
          Left(AvroError.encodeDecimalPrecisionExceeded(decimal.precision, precision))

        case Right(_) =>
          Right(AvroValue.Bytes(ByteVector.view(decimal.underlying.unscaledValue.toByteArray)))

        case Left(error) =>
          Left(error)
      }

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, BigDecimal] =
      (schema, writerSchema) match {
        case (Left(error), _) =>
          failing(error)

        case (Right(_), _: Schema.Bytes) =>
          writerSchema.logicalType match {
            case Some(LogicalType.Decimal(writerPrecision, writerScale)) => {
              case AvroValue.Bytes(bytes) => decodeDecimal(bytes, writerPrecision, writerScale)
              case value                  => Left(invalidValue(value, writerSchema))
            }

            case writerLogicalType =>
              failing(unexpectedLogicalType(writerLogicalType))
          }

        case (Right(_), _) =>
          failing(unexpectedSchemaType(writerSchema, "bytes"))
      }
  }

  /** Codec for a logical type, which must be the logical type of writer schemas. */
  private[v2] final case class LogicalTypeCodec[A](
    codec: Codec[A],
    logicalType: LogicalType
  ) extends Codec[A] {
    override lazy val schema: Either[AvroError, Schema] =
      codec.schema.flatMap(addLogicalType(_, logicalType))

    override def encode(a: A): Either[AvroError, AvroValue] =
      codec.encode(a)

    /** Checks the logical type after decoding, so errors for the writer schema type come first. */
    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroError, A] = {
      val decode = codec.resolve(writerSchema, definitions)
      writerSchema.logicalType match {
        case Some(writerLogicalType) if writerLogicalType == logicalType =>
          decode

        case writerLogicalType =>
          val error = Left(unexpectedLogicalType(writerLogicalType))
          decode(_).flatMap(_ => error)
      }
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
          case AvroValue.Bytes(bytes) =>
            Right(ByteVectors.decodeString(bytes, StandardCharsets.UTF_8))
          case value => invalid(value, writerSchema)
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

  private def addLogicalType(schema: Schema, logicalType: LogicalType): Either[AvroError, Schema] =
    schema match {
      case int: Schema.Int       => Right(int.withLogicalType(logicalType))
      case long: Schema.Long     => Right(long.withLogicalType(logicalType))
      case bytes: Schema.Bytes   => Right(bytes.withLogicalType(logicalType))
      case string: Schema.String => Right(string.withLogicalType(logicalType))
      case fixed: Schema.Fixed   => Right(fixed.withLogicalType(logicalType))
      case _                     =>
        Left {
          AvroError(
            s"Unable to add logical type ${logicalType.name} to ${SchemaNames.describe(schema)}"
          )
        }
    }

  /** Decodes the decimal using the scale, and checks that it does not exceed the precision. */
  private def decodeDecimal(
    bytes: ByteVector,
    precision: Int,
    scale: Int
  ): Either[AvroError, BigDecimal] =
    if (bytes.isEmpty) Left(AvroError("Unable to decode decimal from empty bytes"))
    else {
      val unscaled = new java.math.BigInteger(bytes.toArray)
      val decimal = BigDecimal(new java.math.BigDecimal(unscaled, scale))
      if (decimal.precision <= precision) Right(decimal)
      else Left(AvroError.decodeDecimalPrecisionExceeded(decimal.precision, precision))
    }

  /** The time since the epoch in the unit, or an error if it exceeds a long. */
  private def epochTime(value: Any, unit: String)(time: => Long): Either[AvroError, Long] =
    try Right(time)
    catch {
      case _: ArithmeticException =>
        Left(AvroError(s"Unable to encode $value as epoch $unit exceeding the maximum long size"))
    }

  private final val MillisPerDay: Int = 86400000

  private final val MicrosPerDay: Long = 86400000000L

  private def unexpectedTime(typeName: String, value: Long, perDay: Long): AvroError =
    AvroError(s"Got unexpected $typeName value $value, expected value in range 0 to ${perDay - 1}")

  private[v2] def unexpectedLogicalType(logicalType: Option[LogicalType]): AvroError =
    logicalType match {
      case Some(logicalType) => AvroError(s"Got unexpected logical type ${logicalType.name}")
      case None              => AvroError("Got unexpected missing logical type")
    }

  /** The schema of the field, including the default encoded as JSON. */
  private def fieldSchema[A, B](field: Field[A, B]): Either[AvroError, Schema.Field] =
    for {
      schema <- field.codec.schema
      default <- field.default match {
        case Some(default) =>
          field.codec
            .encodeForSchema(default)
            .map(Some(_))
            .left
            .map(AvroError.errorEncodingFrom(s"default for field ${field.name}", _))

        case None =>
          Right(None)
      }
    } yield Schema
      .Field(field.name, schema)
      .withDoc(field.doc)
      .withDefault(default)
      .withOrder(field.order.getOrElse(Schema.Field.Order.Ascending))
      .withAliases(field.aliases.toVector)
      .withProps(field.props)

  private def fieldEncoder[A, B](field: Field[A, B]): A => Either[AvroError, AvroValue] =
    a =>
      field.codec
        .encode(field.access(a))
        .left
        .map(AvroError.errorEncodingFrom(s"field ${field.name}", _))

  /** Decodes the value of the writer field with the same name or an alias of the field, or uses the
    * default of the field if there is no such writer field.
    */
  private def fieldDecoder[A, B](
    field: Field[A, B],
    record: Schema.Record,
    definitions: Definitions
  ): Vector[AvroValue] => Either[AvroError, Any] = {
    val writerIndex =
      (field.name +: field.aliases).iterator
        .map(name => record.fields.indexWhere(_.name == name))
        .find(_ >= 0)

    writerIndex match {
      case Some(index) =>
        val decode = field.codec.decoder(record.fields(index).schema, definitions)
        values =>
          decode(values(index)).left.map(AvroError.errorDecodingTo(s"field ${field.name}", _))

      case None =>
        val result = field.default.toRight(AvroError.decodeMissingRecordField(field.name))
        _ => result
    }
  }

  private val EncodedNone: Either[AvroError, AvroValue] =
    Right(AvroValue.Union(0, AvroValue.Null))

  private val DecodedNone: Either[AvroError, Option[Nothing]] =
    Right(None)

  /** The index of the union type to decode values of the writer schema with: the same type, or a
    * named type with the same full name or alias, then the same name, then a promoted type.
    */
  private def unionType(writerSchema: Schema, types: Vector[Schema]): Option[Int] = {
    def find(matches: Schema => Boolean): Option[Int] =
      Some(types.indexWhere(matches)).filter(_ >= 0)

    writerSchema match {
      case named: Schema.Named =>
        find(sameNamedType(named, _)(_.fullName))
          .orElse(find(sameNamedType(named, _)(_.name)))

      case _ =>
        find(SchemaNames.typeName(_) == SchemaNames.typeName(writerSchema))
          .orElse(find(isPromotion(writerSchema, _)))
    }
  }

  /** Returns `true` if the schema is the same kind of named schema, with the name of the writer
    * schema as its name or as an alias, comparing names using the function.
    */
  private def sameNamedType(writerSchema: Schema.Named, schema: Schema)(
    name: Schema.Name => String
  ): Boolean =
    schema match {
      case named: Schema.Named
          if SchemaNames.typeName(named) == SchemaNames.typeName(writerSchema) =>
        val aliases = named.aliases.map(SchemaNames.resolve(_, named.name.namespace))
        (named.name +: aliases).exists(name(_) == name(writerSchema.name))

      case _ =>
        false
    }

  /** Returns `true` if values of the writer schema can be promoted to the schema. */
  private def isPromotion(writerSchema: Schema, schema: Schema): Boolean =
    (writerSchema, schema) match {
      case (_: Schema.Int, _: Schema.Long | _: Schema.Float | _: Schema.Double) => true
      case (_: Schema.Long, _: Schema.Float | _: Schema.Double)                 => true
      case (_: Schema.Float, _: Schema.Double)                                  => true
      case (_: Schema.String, _: Schema.Bytes)                                  => true
      case (_: Schema.Bytes, _: Schema.String)                                  => true
      case _                                                                    => false
    }

  /** The schema, if it is valid, including defaults. */
  private def validated(schema: Schema): Either[AvroError, Schema] =
    SchemaValidator.validate(schema, validateDefaults = true).map(_ => schema)

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
