/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Invariant
import cats.Show
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.free.FreeApplicative
import cats.syntax.all.*
import cats.~>
import io.circe.Json
import io.circe.JsonObject
import java.nio.charset.StandardCharsets
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
import java.util.Arrays
import java.util.UUID
import java.util.concurrent.atomic.AtomicReferenceArray
import scala.annotation.implicitNotFound
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.util.Try
import scodec.bits.ByteVector
import vulcan.AvroException.*
import vulcan.internal.*

/**
  * Provides a schema, along with encoders and decoders for a given type.
  *
  * Note encoders and decoders are compiled when requested, so make sure
  * to request them once and reuse them as many times as required.
  */
@implicitNotFound(
  "could not find implicit Codec[${A}]; ensure no imports are missing or manually define an instance"
)
sealed abstract class Codec[A] {

  /** The schema or an error if the schema could not be generated. */
  def schema: Either[AvroException, Schema]

  /** Returns an `Encoder` for values, or an error if the schema is invalid. */
  final def encoder: Either[AvroException, Encoder[A]] =
    schema.flatMap(SchemaValidator.validate(_, validateDefaults = false)).map { _ =>
      new Encoder[A] {
        override def encode(a: A): Either[AvroException, AvroValue] =
          Codec.this.encode(a)
      }
    }

  /**
    * Returns a `Decoder` for values written using the provided writer
    * schema, or an error if the schema or writer schema is invalid.
    */
  final def decoder(writerSchema: Schema): Either[AvroException, Decoder[A]] =
    (schema >> SchemaValidator.validate(writerSchema, validateDefaults = false)).map { _ =>
      val resolved = resolveDecoder(writerSchema, new Definitions(writerSchema))
      new Decoder[A] {
        override def decode(value: AvroValue): Either[AvroException, A] =
          resolved(value)
      }
    }

  /** Returns a `BinaryEncoder` for Avro binary, or an error if the schema is invalid. */
  final def binaryEncoder: Either[AvroException, BinaryEncoder[A]] =
    schema.flatMap(BinaryEncoder(_)).map { compiled =>
      new BinaryEncoder[A] {
        override def encode(a: A): Either[AvroException, ByteVector] =
          Codec.this.encode(a).flatMap(compiled.encode)
      }
    }

  /**
    * Returns a `BinaryDecoder` for Avro binary written using the provided
    * writer schema, or an error if the schema or writer schema is invalid.
    *
    * To protect against invalid input, decoding fails for values nested
    * more than 512 levels deep in recursive schemas, and for values with
    * more than 1,000,000 array or map items which are encoded using zero
    * bytes, like nulls and empty records.
    */
  final def binaryDecoder(writerSchema: Schema): Either[AvroException, BinaryDecoder[A]] =
    (schema >> BinaryDecoder(writerSchema)).map { compiled =>
      val resolved = resolveDecoder(writerSchema, new Definitions(writerSchema))
      new BinaryDecoder[A] {
        override def decode(bytes: ByteVector): Either[AvroException, A] =
          compiled.decode(bytes).flatMap(resolved)
      }
    }

  /** Returns a `JsonEncoder` for Avro `Json`, or an error if the schema is invalid. */
  final def jsonEncoder: Either[AvroException, JsonEncoder[A]] =
    schema.flatMap(JsonEncoder(_)).map { compiled =>
      new JsonEncoder[A] {
        override def encode(a: A): Either[AvroException, Json] =
          Codec.this.encode(a).flatMap(compiled.encode)
      }
    }

  /**
    * Returns a `JsonDecoder` for Avro `Json` written using the provided
    * writer schema, or an error if the schema or writer schema is invalid.
    *
    * To protect against invalid input, decoding fails for values nested
    * more than 512 levels deep in recursive schemas.
    */
  final def jsonDecoder(writerSchema: Schema): Either[AvroException, JsonDecoder[A]] =
    (schema >> JsonDecoder(writerSchema)).map { compiled =>
      val resolved = resolveDecoder(writerSchema, new Definitions(writerSchema))
      new JsonDecoder[A] {
        override def decode(json: Json): Either[AvroException, A] =
          compiled.decode(json).flatMap(resolved)
      }
    }

  /**
    * Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    */
  final def imap[B](f: A => B)(g: B => A): Codec[B] =
    imapErrors(a => Right(f(a)))(b => Right(g(b)))

  /**
    * Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    *
    * Similar to [[Codec#imap]], except the mapping from `A` to `B` might be unsuccessful.
    */
  final def imapError[B](f: A => Either[AvroException, B])(g: B => A): Codec[B] =
    imapErrors(f)(b => Right(g(b)))

  /**
    * Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    *
    * Similar to [[Codec#imap]], except the mappings in both directions might be unsuccessful.
    */
  final def imapErrors[B](f: A => Either[AvroException, B])(g: B => Either[AvroException, A]): Codec[B] =
    new Codec.ImapErrors(this, f, g)

  /**
    * Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    *
    * Similar to [[Codec#imap]], except the mapping from `A` to `B` might be unsuccessful.
    */
  final def imapTry[B](f: A => Try[B])(g: B => A): Codec[B] =
    imapError(f(_).toEither.left.map(e => AvroException(e.toString, e)))(g)

  /** Attempts to encode the specified value. */
  private[vulcan] def encode(a: A): Either[AvroException, AvroValue]

  /** Returns a function decoding values of the writer schema; neither a union nor a reference. */
  private[vulcan] def resolve(
    writerSchema: Schema,
    definitions: Definitions
  ): AvroValue => Either[AvroException, A]

  /** The value encoded as `null`, if there is one, used as the default of nullable fields. */
  private[vulcan] def nullValue: Option[A] =
    None

  /** Returns a function decoding values of the writer schema, resolved once for the schema. */
  private[vulcan] final def resolveDecoder(
    writerSchema: Schema,
    definitions: Definitions
  ): AvroValue => Either[AvroException, A] =
    writerSchema match {
      case union: Schema.Union =>
        new Codec.UnionDecoder(this, union, definitions)

      case ref: Schema.Ref =>
        definitions.get(ref.fullName) match {
          case Some(named) => resolve(named, definitions)
          case None => Codec.failing(new UndefinedName(ref.fullName))
        }

      case _ =>
        resolve(writerSchema, definitions)
    }

  private[vulcan] final def withTypeName(typeName: String): Codec[A] =
    new Codec.WithTypeName(this, typeName)

  private[vulcan] final def withLogicalType(logicalType: LogicalType): Codec[A] =
    new Codec.LogicalTypeCodec(this, logicalType)

  /** Encodes the value as JSON for defaults in schemas, where unions are without the type. */
  private[vulcan] final def encodeForSchema(a: A): Either[AvroException, Json] =
    for {
      validSchema <- schema
      encoder <- JsonEncoder.withoutUnionTypes(validSchema)
      value <- encode(a)
      json <- encoder.encode(value)
    } yield json

  override final def toString: String =
    schema match {
      case Right(schema) => s"Codec(${schema.toJson.spaces2})"
      case Left(error) => error.toString
    }
}

/**
  * @groupname General General Codecs
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

  /**
    * Returns the [[Codec]] for the specified type.
    *
    * @group Utilities
    */
  final def apply[A](implicit codec: Codec[A]): Codec[A] =
    codec

  /**
    * @group General
    */
  implicit lazy val boolean: Codec[Boolean] =
    BooleanCodec

  /**
    * @group General
    */
  implicit lazy val byte: Codec[Byte] =
    Codec.int
      .imapError { int =>
        if (int.isValidByte) Right(int.toByte)
        else Left(new ValueOutOfRange("Int", int.toLong, Byte.MinValue.toLong, Byte.MaxValue.toLong))
      }(_.toInt)
      .withTypeName("Byte")

  /**
    * @group General
    */
  implicit lazy val bytes: Codec[ByteVector] =
    BytesCodec

  /**
    * @group Cats
    */
  implicit final def chain[A](implicit codec: Codec[A]): Codec[Chain[A]] =
    new ArrayCodec[A, Chain[A]](codec, _.length.toInt, _.iterator, chainBuilder)
      .withTypeName("Chain")

  /**
    * @group General
    */
  implicit lazy val char: Codec[Char] =
    Codec.string
      .imapError { string =>
        if (string.length == 1) Right(string.charAt(0))
        else Left(new InvalidChar(string.length))
      }(_.toString)
      .withTypeName("Char")

  /**
    * Returns a new decimal [[Codec]] for `BigDecimal`.
    *
    * Values are decoded using the precision and scale of the writer schema.
    *
    * @group Create
    */
  final def decimal(precision: Int, scale: Int): Codec[BigDecimal] =
    new DecimalCodec(precision, scale).withTypeName("BigDecimal")

  /**
    * @group General
    */
  implicit lazy val double: Codec[Double] =
    DoubleCodec

  /**
    * @group General
    */
  implicit final def either[A, B](implicit
    codecA: Codec[A],
    codecB: Codec[B]
  ): Codec[Either[A, B]] =
    Codec
      .union[Either[A, B]](alt => alt[Left[A, B]] ++ alt[Right[A, B]])
      .withTypeName("Either")

  /**
    * Returns a new enum [[Codec]] for type `A`.
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
    decode: String => Either[AvroException, A],
    default: Option[A] = None,
    doc: Option[String] = None,
    aliases: Seq[String] = Seq.empty,
    props: JsonObject = JsonObject.empty
  ): Codec[A] = {
    val enumName = SchemaNames.resolve(name, Some(namespace))
    new EnumCodec(enumName, symbols.toVector, encode, decode, default, doc, aliases.toVector, props)
      .withTypeName(enumName.fullName)
  }

  /**
    * Returns a new fixed [[Codec]] for type `A`.
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
    decode: ByteVector => Either[AvroException, A],
    doc: Option[String] = None,
    aliases: Seq[String] = Seq.empty,
    props: JsonObject = JsonObject.empty
  ): Codec[A] = {
    val fixedName = SchemaNames.resolve(name, Some(namespace))
    new FixedCodec(fixedName, size, encode, decode, doc, aliases.toVector, props)
      .withTypeName(fixedName.fullName)
  }

  /**
    * @group General
    */
  implicit lazy val float: Codec[Float] =
    FloatCodec

  /**
    * Returns a new [[Codec]] using the specified schema, encoder, and decoder function.
    *
    * The decoder function returns a [[Decoder]] for values of the writer schema, and is
    * called once for each writer schema. Values of unions are decoded using the schema
    * of the written type, so the writer schema is neither a union nor a reference. It
    * includes the definitions of the named schemas it references, so it can be passed
    * to [[Codec#decoder]] of other codecs. If the decoder function returns an error,
    * decoding values fails with the error.
    *
    * @group Create
    */
  final def instance[A](
    schema: Either[AvroException, Schema],
    encoder: Encoder[A],
    decoder: Schema => Either[AvroException, Decoder[A]]
  ): Codec[A] =
    new Instance(schema, encoder, decoder)

  /**
    * @group JavaTime
    */
  implicit lazy val instant: Codec[Instant] =
    Codec.long
      .imapErrors(millis => Right(Instant.ofEpochMilli(millis))) { instant =>
        epochTime(instant, "milliseconds")(instant.toEpochMilli)
      }
      .withLogicalType(LogicalType.timestampMillis)
      .withTypeName("Instant")

  /**
    * `Instant` with microsecond precision, encoded with logical type `timestamp-micros`.
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

  /**
    * @group JavaTime
    */
  implicit lazy val instantMicros: Codec[InstantMicros] =
    Codec.long
      .imapErrors(micros => Right(InstantMicros.fromEpochMicro(micros))) { instantMicros =>
        epochTime(instantMicros.value, "microseconds")(instantMicros.toEpochMicro)
      }
      .withLogicalType(LogicalType.timestampMicros)
      .withTypeName("InstantMicros")

  /**
    * @group General
    */
  implicit lazy val int: Codec[Int] =
    IntCodec

  /**
    * @group General
    */
  implicit final def left[A, B](implicit codec: Codec[A]): Codec[Left[A, B]] =
    codec.imap(Left[A, B](_))(_.value)

  /**
    * @group Collection
    */
  implicit final def list[A](implicit codec: Codec[A]): Codec[List[A]] =
    new ArrayCodec[A, List[A]](codec, _.length, _.iterator, _ => List.newBuilder)
      .withTypeName("List")

  /**
    * @group JavaTime
    */
  implicit lazy val localDate: Codec[LocalDate] =
    Codec.int
      .imapErrors(days => Right(LocalDate.ofEpochDay(days.toLong))) { date =>
        val days = date.toEpochDay
        if (days.isValidInt) Right(days.toInt)
        else Left(new DateSizeExceeded(days))
      }
      .withLogicalType(LogicalType.date)
      .withTypeName("LocalDate")

  /**
    * @group JavaTime
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

  /**
    * @group JavaTime
    */
  final lazy val localTimeMicros: Codec[LocalTime] =
    Codec.long
      .imapError { micros =>
        if (micros >= 0 && micros < MicrosPerDay) Right(LocalTime.ofNanoOfDay(micros * 1000L))
        else Left(new ValueOutOfRange("Long", micros, 0L, MicrosPerDay - 1L))
      }(_.toNanoOfDay / 1000L)
      .withLogicalType(LogicalType.timeMicros)
      .withTypeName("LocalTime")

  /**
    * @group JavaTime
    */
  final lazy val localTimeMillis: Codec[LocalTime] =
    Codec.int
      .imapError { millis =>
        if (millis >= 0 && millis < MillisPerDay) Right(LocalTime.ofNanoOfDay(millis * 1000000L))
        else Left(new ValueOutOfRange("Int", millis.toLong, 0L, MillisPerDay - 1L))
      }(time => (time.toNanoOfDay / 1000000L).toInt)
      .withLogicalType(LogicalType.timeMillis)
      .withTypeName("LocalTime")

  /**
    * @group General
    */
  implicit lazy val long: Codec[Long] =
    LongCodec

  /**
    * @group Collection
    */
  implicit final def map[A](implicit codec: Codec[A]): Codec[Map[String, A]] =
    new MapCodec[A, Map[String, A]](codec, _.size, _.iterator, _ => Map.newBuilder)
      .withTypeName("Map")

  /**
    * @group General
    */
  implicit lazy val none: Codec[None.type] =
    new NullCodec(None, "None")

  /**
    * @group Cats
    */
  implicit final def nonEmptyChain[A](implicit codec: Codec[A]): Codec[NonEmptyChain[A]] =
    new ArrayCodec[A, Chain[A]](codec, _.length.toInt, _.iterator, chainBuilder)
      .imapError(NonEmptyChain.fromChain(_).toRight(new EmptyCollection()))(_.toChain)
      .withTypeName("NonEmptyChain")

  /**
    * @group Cats
    */
  implicit final def nonEmptyList[A](implicit codec: Codec[A]): Codec[NonEmptyList[A]] =
    new ArrayCodec[A, List[A]](codec, _.length, _.iterator, _ => List.newBuilder)
      .imapError(NonEmptyList.fromList(_).toRight(new EmptyCollection()))(_.toList)
      .withTypeName("NonEmptyList")

  /**
    * @group Cats
    */
  implicit final def nonEmptyMap[A](implicit codec: Codec[A]): Codec[NonEmptyMap[String, A]] =
    new MapCodec[A, SortedMap[String, A]](codec, _.size, _.iterator, _ => SortedMap.newBuilder)
      .imapError(NonEmptyMap.fromMap(_).toRight(new EmptyCollection()))(_.toSortedMap)
      .withTypeName("NonEmptyMap")

  /**
    * @group Cats
    */
  implicit final def nonEmptySet[A](implicit
    codec: Codec[A],
    ordering: Ordering[A]
  ): Codec[NonEmptySet[A]] =
    new ArrayCodec[A, SortedSet[A]](codec, _.size, _.iterator, _ => SortedSet.newBuilder)
      .imapError(NonEmptySet.fromSet(_).toRight(new EmptyCollection()))(_.toSortedSet)
      .withTypeName("NonEmptySet")

  /**
    * @group Cats
    */
  implicit final def nonEmptyVector[A](implicit codec: Codec[A]): Codec[NonEmptyVector[A]] =
    new ArrayCodec[A, Vector[A]](codec, _.length, _.iterator, vectorBuilder)
      .imapError(NonEmptyVector.fromVector(_).toRight(new EmptyCollection()))(_.toVector)
      .withTypeName("NonEmptyVector")

  /**
    * Returns a [[Codec]] for options, encoded as a union with `null` first.
    *
    * If the underlying codec is for a union, the types of that union are included in the union.
    *
    * @group General
    */
  implicit final def option[A](implicit codec: Codec[A]): Codec[Option[A]] =
    new OptionCodec(codec).withTypeName("Option")

  /**
    * Returns a new record [[Codec]] for type `A`.
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
    new RecordCodec(recordName, doc, aliases.toVector, props, f(FieldBuilder.instance))
      .withTypeName(recordName.fullName)
  }

  /**
    * @group General
    */
  implicit final def right[A, B](implicit codec: Codec[B]): Codec[Right[A, B]] =
    codec.imap(Right[A, B](_))(_.value)

  /**
    * @group Collection
    */
  implicit final def seq[A](implicit codec: Codec[A]): Codec[Seq[A]] =
    new ArrayCodec[A, Seq[A]](codec, _.length, _.iterator, _ => Seq.newBuilder)
      .withTypeName("Seq")

  /**
    * @group Collection
    */
  implicit final def set[A](implicit codec: Codec[A]): Codec[Set[A]] =
    new ArrayCodec[A, Set[A]](codec, _.size, _.iterator, _ => Set.newBuilder)
      .withTypeName("Set")

  /**
    * @group General
    */
  implicit lazy val short: Codec[Short] =
    Codec.int
      .imapError { int =>
        if (int.isValidShort) Right(int.toShort)
        else Left(new ValueOutOfRange("Int", int.toLong, Short.MinValue.toLong, Short.MaxValue.toLong))
      }(_.toInt)
      .withTypeName("Short")

  /**
    * @group General
    */
  implicit final def some[A](implicit codec: Codec[A]): Codec[Some[A]] =
    codec.imap(Some(_))(_.value)

  /**
    * @group General
    */
  implicit lazy val string: Codec[String] =
    StringCodec

  /**
    * Returns a new union [[Codec]] for type `A`.
    *
    * Values are encoded with the first alternative which includes the value. Values are decoded
    * with the alternative for the written type: the same type, or a named type with the same full
    * name or alias, then the same name, and otherwise the first type the written type can be
    * promoted to.
    *
    * @group Create
    */
  final def union[A](f: AltBuilder[A] => Chain[Alt[A]]): Codec[A] =
    new UnionCodec(f(AltBuilder.instance))

  /**
    * @group General
    */
  implicit lazy val unit: Codec[Unit] =
    new NullCodec((), "Unit")

  /**
    * @group JavaUtil
    */
  implicit lazy val uuid: Codec[UUID] =
    Codec.string
      .imapError { string =>
        try Right(UUID.fromString(string))
        catch {
          case _: IllegalArgumentException => Left(new InvalidUuid(string))
        }
      }(_.toString)
      .withLogicalType(LogicalType.uuid)
      .withTypeName("UUID")

  /**
    * @group Collection
    */
  implicit final def vector[A](implicit codec: Codec[A]): Codec[Vector[A]] =
    new ArrayCodec[A, Vector[A]](codec, _.length, _.iterator, vectorBuilder)
      .withTypeName("Vector")

  /**
    * @group Cats
    */
  implicit final val codecInvariant: Invariant[Codec] =
    new Invariant[Codec] {
      override final def imap[A, B](codec: Codec[A])(f: A => B)(g: B => A): Codec[B] =
        codec.imap(f)(g)
    }

  /**
    * @group Cats
    */
  implicit final def codecShow[A]: Show[Codec[A]] =
    Show.fromToString

  /**
    * @group Create
    */
  sealed abstract class Alt[A] {
    type B

    def codec: Codec[B]

    def prism: Prism[A, B]

    def imap[A0](f: A0 => Option[A], g: A => A0): Alt[A0] =
      Alt(codec, prism.imap(f, g))
  }

  private[vulcan] object Alt {
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

  /**
    * @group Create
    */
  sealed abstract class AltBuilder[A] {
    def apply[B](implicit codec: Codec[B], prism: Prism[A, B]): Chain[Alt[A]]
  }

  private[vulcan] object AltBuilder {
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

  /**
    * @group Create
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

  /**
    * @group Create
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

  private[vulcan] object FieldBuilder {
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

  private[vulcan] final class ImapErrors[A, B](
    val codec: Codec[A],
    val f: A => Either[AvroException, B],
    val g: B => Either[AvroException, A]
  ) extends Codec[B] {
    override def schema: Either[AvroException, Schema] =
      codec.schema

    override def encode(b: B): Either[AvroException, AvroValue] =
      g(b).flatMap(codec.encode)

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, B] = {
      val decode = codec.resolve(writerSchema, definitions)
      decode(_).flatMap(f)
    }
  }

  private[vulcan] final class Instance[A](
    val instanceSchema: Either[AvroException, Schema],
    val instanceEncoder: Encoder[A],
    val instanceDecoder: Schema => Either[AvroException, Decoder[A]]
  ) extends Codec[A] {
    override lazy val schema: Either[AvroException, Schema] =
      instanceSchema.flatMap { schema =>
        SchemaValidator.validate(schema, validateDefaults = true).map(_ => schema)
      }

    override def encode(a: A): Either[AvroException, AvroValue] =
      instanceEncoder.encode(a)

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, A] =
      instanceDecoder(definitions.expanded(writerSchema)) match {
        case Right(decoder) => decoder.decode(_)
        case Left(error) => failing(error)
      }
  }

  private[vulcan] final class WithTypeName[A](
    val codec: Codec[A],
    val typeName: String
  ) extends Codec[A] {
    override def schema: Either[AvroException, Schema] =
      codec.schema

    override def encode(a: A): Either[AvroException, AvroValue] =
      codec.encode(a).left.map(new ErrorEncodingType(typeName, _))

    override def nullValue: Option[A] =
      codec.nullValue

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, A] = {
      val decode = codec.resolve(writerSchema, definitions)
      decode(_).left.map(new ErrorDecodingType(typeName, _))
    }
  }

  /** Codec for collections encoded as arrays, with a builder for the number of items. */
  private[vulcan] final class ArrayCodec[A, C](
    val items: Codec[A],
    val size: C => Int,
    val iterator: C => Iterator[A],
    val newBuilder: Int => mutable.Builder[A, C]
  ) extends Codec[C] {
    override lazy val schema: Either[AvroException, Schema] =
      items.schema.map(Schema.Array(_))

    override def encode(collection: C): Either[AvroException, AvroValue] =
      build(iterator(collection), vectorBuilder[AvroValue](size(collection)))(items.encode)
        .map(AvroValue.Array(_))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, C] =
      writerSchema match {
        case array: Schema.Array =>
          val decode = items.resolveDecoder(array.items, definitions)
          value =>
            value match {
              case AvroValue.Array(values) =>
                build(values.iterator, newBuilder(values.size))(decode)

              case _ =>
                Left(invalidValue(value, writerSchema))
            }

        case _ =>
          failing(new UnexpectedSchemaType(writerSchema, "array"))
      }
  }

  /** Codec for maps with string keys, with a builder for the number of entries. */
  private[vulcan] final class MapCodec[A, M](
    val values: Codec[A],
    val size: M => Int,
    val iterator: M => Iterator[(String, A)],
    val newBuilder: Int => mutable.Builder[(String, A), M]
  ) extends Codec[M] {
    override lazy val schema: Either[AvroException, Schema] =
      values.schema.map(Schema.Map(_))

    override def encode(map: M): Either[AvroException, AvroValue] =
      build(iterator(map), vectorBuilder[(String, AvroValue)](size(map))) { case (key, value) =>
        values.encode(value).map((key, _))
      }.map(AvroValue.Map(_))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, M] =
      writerSchema match {
        case map: Schema.Map =>
          val decode = values.resolveDecoder(map.values, definitions)
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
          failing(new UnexpectedSchemaType(writerSchema, "map"))
      }
  }

  /** Codec for options, as unions with `null` first, including the types of unions. */
  private[vulcan] final class OptionCodec[A](val codec: Codec[A]) extends Codec[Option[A]] {
    override lazy val schema: Either[AvroException, Schema] =
      codec.schema.flatMap { schema =>
        val types =
          schema match {
            case union: Schema.Union => union.types
            case _ => Vector(schema)
          }

        validated(Schema.Union(Schema.Null +: types))
      }

    /** Whether the types of the union of the codec are included in the union of the option. */
    private lazy val includesUnion: Boolean =
      codec.schema.exists(_.isInstanceOf[Schema.Union])

    override def encode(option: Option[A]): Either[AvroException, AvroValue] =
      (schema, option) match {
        case (Right(_), Some(a)) =>
          codec.encode(a).flatMap {
            case AvroValue.Union(index, value) if includesUnion =>
              Right(AvroValue.Union(index + 1, value))

            case value if !includesUnion =>
              Right(AvroValue.Union(1, value))

            case value =>
              Left(new InvalidValue(value, "union"))
          }

        case (Right(_), None) =>
          EncodedNone

        case (Left(error), _) =>
          Left(error)
      }

    override val nullValue: Option[Option[A]] =
      Some(None)

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, Option[A]] =
      (schema, writerSchema) match {
        case (Left(error), _) =>
          failing(error)

        case (Right(_), _: Schema.Null) => {
          case AvroValue.Null => DecodedNone
          case value => Left(invalidValue(value, writerSchema))
        }

        case (Right(_), _) =>
          val decode = codec.resolve(writerSchema, definitions)
          decode(_).map(Some(_))
      }
  }

  /** Codec for unions, which decodes using the alternative for the written type. */
  private[vulcan] final class UnionCodec[A](val alternatives: Chain[Alt[A]]) extends Codec[A] {
    private val alts: Vector[Alt[A]] =
      alternatives.toVector

    private lazy val types: Either[AvroException, Vector[Schema]] =
      build(alts.iterator, vectorBuilder[Schema](alts.size))(_.codec.schema)

    override lazy val schema: Either[AvroException, Schema] =
      types.flatMap(types => validated(Schema.Union(types)))

    override def encode(a: A): Either[AvroException, AvroValue] =
      schema match {
        case Right(_) => encodeWith(a, 0)
        case Left(error) => Left(error)
      }

    @tailrec private def encodeWith(a: A, index: Int): Either[AvroException, AvroValue] =
      if (index < alts.length) {
        val alt = alts(index)
        alt.prism.getOption(a) match {
          case Some(value) => alt.codec.encode(value).map(AvroValue.Union(index, _))
          case None => encodeWith(a, index + 1)
        }
      } else Left(new ExhaustedAlternatives())

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, A] =
      (schema >> types) match {
        case Right(types) =>
          unionType(writerSchema, types) match {
            case Some(index) =>
              val alt = alts(index)
              val decode = alt.codec.resolve(writerSchema, definitions)
              decode(_).map(alt.prism.reverseGet)

            case None =>
              failing(new MissingAlternative(writerSchema))
          }

        case Left(error) =>
          failing(error)
      }
  }

  /**
    * Codec for records, which decodes fields from the writer field with the same name or an alias
    * of the field, and otherwise uses the default of the field.
    */
  private[vulcan] final class RecordCodec[A](
    val name: Schema.Name,
    val doc: Option[String],
    val aliases: Vector[String],
    val props: JsonObject,
    val fields: FreeApplicative[Field[A, *], A]
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

    private lazy val encoders: Vector[A => Either[AvroException, AvroValue]] =
      fieldVector.map(fieldEncoder(_))

    override lazy val schema: Either[AvroException, Schema] =
      build(fieldVector.iterator, vectorBuilder[Schema.Field](fieldVector.size))(fieldSchema(_))
        .flatMap { fields =>
          validated {
            Schema.Record(name, fields).withDoc(doc).withAliases(aliases).withProps(props)
          }
        }

    override def encode(a: A): Either[AvroException, AvroValue] =
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
    ): AvroValue => Either[AvroException, A] =
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
                Left(new InvalidRecordValue(values.size, record.fullName, record.fields.size))

              case _ =>
                Left(invalidValue(value, writerSchema))
            }

        case (Right(_), _) =>
          failing(new UnexpectedSchemaType(writerSchema, "record"))
      }

    private def decodeFields(
      values: Vector[AvroValue],
      decoders: Array[Vector[AvroValue] => Either[AvroException, Any]]
    ): Either[AvroException, A] = {
      val decoded = new Array[Any](decoders.length)

      @tailrec def loop(index: Int): Either[AvroException, A] =
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
  private[vulcan] final class EnumCodec[A](
    val name: Schema.Name,
    val symbols: Vector[String],
    val encodeSymbol: A => String,
    val decodeSymbol: String => Either[AvroException, A],
    val default: Option[A],
    val doc: Option[String],
    val aliases: Vector[String],
    val props: JsonObject
  ) extends Codec[A] {
    private val symbolSet: Set[String] =
      symbols.toSet

    override lazy val schema: Either[AvroException, Schema] =
      validated {
        Schema
          .Enum(name, symbols)
          .withDefault(default.map(encodeSymbol))
          .withDoc(doc)
          .withAliases(aliases)
          .withProps(props)
      }

    override def encode(a: A): Either[AvroException, AvroValue] =
      schema match {
        case Right(_) =>
          val symbol = encodeSymbol(a)
          if (symbolSet.contains(symbol)) Right(AvroValue.Enum(symbol))
          else Left(new EncodeSymbolNotInSchema(symbol, symbols))

        case Left(error) =>
          Left(error)
      }

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, A] =
      (schema, writerSchema) match {
        case (Left(error), _) =>
          failing(error)

        case (Right(_), _: Schema.Enum) => {
          case AvroValue.Enum(symbol) =>
            if (symbolSet.contains(symbol)) decodeSymbol(symbol)
            else default.toRight(new DecodeSymbolNotInSchema(symbol, symbols))

          case value =>
            Left(invalidValue(value, writerSchema))
        }

        case (Right(_), _) =>
          failing(new UnexpectedSchemaType(writerSchema, "enum"))
      }
  }

  /** Codec for fixed, which pads bytes when encoding, and requires the same size when decoding. */
  private[vulcan] final class FixedCodec[A](
    val name: Schema.Name,
    val size: Int,
    val encodeBytes: A => ByteVector,
    val decodeBytes: ByteVector => Either[AvroException, A],
    val doc: Option[String],
    val aliases: Vector[String],
    val props: JsonObject
  ) extends Codec[A] {
    override lazy val schema: Either[AvroException, Schema] =
      validated(Schema.Fixed(name, size).withDoc(doc).withAliases(aliases).withProps(props))

    override def encode(a: A): Either[AvroException, AvroValue] =
      schema match {
        case Right(_) =>
          val bytes = encodeBytes(a)
          if (bytes.size == size) Right(AvroValue.Fixed(bytes))
          else if (bytes.size < size) Right(AvroValue.Fixed(bytes.padRight(size.toLong)))
          else Left(new EncodeFixedSizeExceeded(bytes.size.toInt, size))

        case Left(error) =>
          Left(error)
      }

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, A] =
      (schema, writerSchema) match {
        case (Left(error), _) =>
          failing(error)

        case (Right(_), fixed: Schema.Fixed) if fixed.size == size => {
          case AvroValue.Fixed(bytes) =>
            if (bytes.size == size) decodeBytes(bytes)
            else Left(new DecodeFixedSizeMismatch(bytes.size.toInt, size))

          case value =>
            Left(invalidValue(value, writerSchema))
        }

        case (Right(_), fixed: Schema.Fixed) =>
          failing(new UnexpectedFixedSize(fixed.size, size))

        case (Right(_), _) =>
          failing(new UnexpectedSchemaType(writerSchema, "fixed"))
      }
  }

  /** Codec for decimals, which decodes using the precision and scale of the writer schema. */
  private[vulcan] final class DecimalCodec(val precision: Int, val scale: Int) extends Codec[BigDecimal] {
    override val schema: Either[AvroException, Schema] =
      if (precision <= 0)
        Left(new InvalidDecimalPrecision(precision))
      else if (scale < 0 || scale > precision)
        Left(new InvalidDecimalScale(scale, precision))
      else
        Right(Schema.Bytes.withLogicalType(LogicalType.decimal(precision, scale)))

    override def encode(decimal: BigDecimal): Either[AvroException, AvroValue] =
      schema match {
        case Right(_) if decimal.scale != scale =>
          Left(new DecimalScaleMismatch(decimal.scale, scale))

        case Right(_) if decimal.precision > precision =>
          Left(new EncodeDecimalPrecisionExceeded(decimal.precision, precision))

        case Right(_) =>
          Right(AvroValue.Bytes(ByteVector.view(decimal.underlying.unscaledValue.toByteArray)))

        case Left(error) =>
          Left(error)
      }

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, BigDecimal] =
      (schema, writerSchema) match {
        case (Left(error), _) =>
          failing(error)

        case (Right(_), _: Schema.Bytes) =>
          writerSchema.logicalType match {
            case Some(LogicalType.Decimal(writerPrecision, writerScale)) => {
              case AvroValue.Bytes(bytes) => decodeDecimal(bytes, writerPrecision, writerScale)
              case value => Left(invalidValue(value, writerSchema))
            }

            case _ =>
              failing(new UnexpectedLogicalType(writerSchema))
          }

        case (Right(_), _) =>
          failing(new UnexpectedSchemaType(writerSchema, "bytes"))
      }
  }

  /** Codec for a logical type, which must be the logical type of writer schemas. */
  private[vulcan] final class LogicalTypeCodec[A](
    val codec: Codec[A],
    val logicalType: LogicalType
  ) extends Codec[A] {
    override lazy val schema: Either[AvroException, Schema] =
      codec.schema.flatMap(addLogicalType(_, logicalType))

    override def encode(a: A): Either[AvroException, AvroValue] =
      codec.encode(a)

    /** Checks the logical type after decoding, so errors for the writer schema type come first. */
    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, A] = {
      val decode = codec.resolve(writerSchema, definitions)
      writerSchema.logicalType match {
        case Some(writerLogicalType) if writerLogicalType == logicalType =>
          decode
        case _ =>
          decode(_) >> Left(new UnexpectedLogicalType(writerSchema))
      }
    }
  }

  /** Codec for a primitive type, which also decodes types which can be promoted to the type. */
  private[vulcan] sealed abstract class PrimitiveCodec[A](
    val typeName: String,
    primitiveSchema: Schema
  ) extends Codec[A] {
    override final val schema: Either[AvroException, Schema] =
      Right(primitiveSchema)

    protected final def invalid(value: AvroValue, writerSchema: Schema): Either[AvroException, A] =
      Left(new ErrorDecodingType(typeName, invalidValue(value, writerSchema)))

    protected final def unexpected(writerSchema: Schema): AvroValue => Either[AvroException, A] =
      failing {
        new ErrorDecodingType(
          typeName,
          new UnexpectedSchemaType(writerSchema, SchemaNames.typeName(primitiveSchema))
        )
      }
  }

  private[vulcan] final class NullCodec[A](
    val value: A,
    override val typeName: String
  ) extends PrimitiveCodec[A](typeName, Schema.Null) {
    private val encoded: Either[AvroException, AvroValue] =
      Right(AvroValue.Null)

    private val decoded: Either[AvroException, A] =
      Right(value)

    override def encode(a: A): Either[AvroException, AvroValue] =
      encoded

    override val nullValue: Option[A] =
      Some(value)

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, A] =
      writerSchema match {
        case _: Schema.Null => {
          case AvroValue.Null => decoded
          case value => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[vulcan] object BooleanCodec extends PrimitiveCodec[Boolean]("Boolean", Schema.Boolean) {
    override def encode(boolean: Boolean): Either[AvroException, AvroValue] =
      Right(AvroValue.Boolean(boolean))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, Boolean] =
      writerSchema match {
        case _: Schema.Boolean => {
          case AvroValue.Boolean(boolean) => Right(boolean)
          case value => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[vulcan] object IntCodec extends PrimitiveCodec[Int]("Int", Schema.Int) {
    override def encode(int: Int): Either[AvroException, AvroValue] =
      Right(AvroValue.Int(int))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, Int] =
      writerSchema match {
        case _: Schema.Int => {
          case AvroValue.Int(int) => Right(int)
          case value => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[vulcan] object LongCodec extends PrimitiveCodec[Long]("Long", Schema.Long) {
    override def encode(long: Long): Either[AvroException, AvroValue] =
      Right(AvroValue.Long(long))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, Long] =
      writerSchema match {
        case _: Schema.Long => {
          case AvroValue.Long(long) => Right(long)
          case value => invalid(value, writerSchema)
        }

        case _: Schema.Int => {
          case AvroValue.Int(int) => Right(int.toLong)
          case value => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[vulcan] object FloatCodec extends PrimitiveCodec[Float]("Float", Schema.Float) {
    override def encode(float: Float): Either[AvroException, AvroValue] =
      Right(AvroValue.Float(float))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, Float] =
      writerSchema match {
        case _: Schema.Float => {
          case AvroValue.Float(float) => Right(float)
          case value => invalid(value, writerSchema)
        }

        case _: Schema.Int => {
          case AvroValue.Int(int) => Right(int.toFloat)
          case value => invalid(value, writerSchema)
        }

        case _: Schema.Long => {
          case AvroValue.Long(long) => Right(long.toFloat)
          case value => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[vulcan] object DoubleCodec extends PrimitiveCodec[Double]("Double", Schema.Double) {
    override def encode(double: Double): Either[AvroException, AvroValue] =
      Right(AvroValue.Double(double))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, Double] =
      writerSchema match {
        case _: Schema.Double => {
          case AvroValue.Double(double) => Right(double)
          case value => invalid(value, writerSchema)
        }

        case _: Schema.Float => {
          case AvroValue.Float(float) => Right(float.toDouble)
          case value => invalid(value, writerSchema)
        }

        case _: Schema.Int => {
          case AvroValue.Int(int) => Right(int.toDouble)
          case value => invalid(value, writerSchema)
        }

        case _: Schema.Long => {
          case AvroValue.Long(long) => Right(long.toDouble)
          case value => invalid(value, writerSchema)
        }

        case _ =>
          unexpected(writerSchema)
      }
  }

  private[vulcan] object BytesCodec extends PrimitiveCodec[ByteVector]("ByteVector", Schema.Bytes) {
    override def encode(bytes: ByteVector): Either[AvroException, AvroValue] =
      Right(AvroValue.Bytes(bytes))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, ByteVector] =
      writerSchema match {
        case _: Schema.Bytes => {
          case AvroValue.Bytes(bytes) => Right(bytes)
          case value => invalid(value, writerSchema)
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

  private[vulcan] object StringCodec extends PrimitiveCodec[String]("String", Schema.String) {
    override def encode(string: String): Either[AvroException, AvroValue] =
      Right(AvroValue.String(string))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): AvroValue => Either[AvroException, String] =
      writerSchema match {
        case _: Schema.String => {
          case AvroValue.String(string) => Right(string)
          case value => invalid(value, writerSchema)
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
  ) extends (AvroValue => Either[AvroException, A]) {
    private val types: Array[Schema] =
      union.types.toArray

    private val decoders =
      new AtomicReferenceArray[AvroValue => Either[AvroException, A]](types.length)

    override def apply(value: AvroValue): Either[AvroException, A] =
      value match {
        case AvroValue.Union(index, typeValue) if index >= 0 && index < types.length =>
          decoderOf(index)(typeValue)

        case AvroValue.Union(index, _) =>
          Left(new InvalidUnionValue(index, types.length))

        case _ =>
          Left(invalidValue(value, union))
      }

    private def decoderOf(index: Int): AvroValue => Either[AvroException, A] = {
      val existing = decoders.get(index)
      if (existing != null) existing
      else {
        val decoder = codec.resolveDecoder(types(index), definitions)
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
  private[vulcan] def vectorBuilder[A](size: Int): mutable.Builder[A, Vector[A]] =
    if (size <= 32) new ExactVectorBuilder(size) else Vector.newBuilder

  private def chainBuilder[A](size: Int): mutable.Builder[A, Chain[A]] =
    vectorBuilder[A](size).mapResult(Chain.fromSeq)

  /** Builds the results of the function on the values, stopping at the first error. */
  private def build[A, B, C](
    values: Iterator[A],
    builder: mutable.Builder[B, C]
  )(f: A => Either[AvroException, B]): Either[AvroException, C] = {
    @tailrec def loop(): Either[AvroException, C] =
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

  private def addLogicalType(schema: Schema, logicalType: LogicalType): Either[AvroException, Schema] =
    schema match {
      case int: Schema.Int => Right(int.withLogicalType(logicalType))
      case long: Schema.Long => Right(long.withLogicalType(logicalType))
      case bytes: Schema.Bytes => Right(bytes.withLogicalType(logicalType))
      case string: Schema.String => Right(string.withLogicalType(logicalType))
      case fixed: Schema.Fixed => Right(fixed.withLogicalType(logicalType))
      case _ =>
        Left(new UnsupportedLogicalType(logicalType, schema))
    }

  /** Decodes the decimal using the scale, and checks that it does not exceed the precision. */
  private def decodeDecimal(
    bytes: ByteVector,
    precision: Int,
    scale: Int
  ): Either[AvroException, BigDecimal] =
    if (bytes.isEmpty) Left(new EmptyDecimal())
    else {
      val unscaled = new java.math.BigInteger(bytes.toArray)
      val decimal = BigDecimal(new java.math.BigDecimal(unscaled, scale))
      if (decimal.precision <= precision) Right(decimal)
      else Left(new DecodeDecimalPrecisionExceeded(decimal.precision, precision))
    }

  /** The time since the epoch in the unit, or an error if it exceeds a long. */
  private def epochTime(value: Any, unit: String)(time: => Long): Either[AvroException, Long] =
    try Right(time)
    catch {
      case _: ArithmeticException =>
        Left(new EpochSizeExceeded(value, unit))
    }

  private final val MillisPerDay: Int = 86400000

  private final val MicrosPerDay: Long = 86400000000L

  /** The schema of the field, including the default encoded as JSON. */
  private def fieldSchema[A, B](field: Field[A, B]): Either[AvroException, Schema.Field] =
    for {
      schema <- field.codec.schema
      default <- field.default match {
        case Some(default) =>
          field.codec
            .encodeForSchema(default)
            .map(Some(_))
            .left
            .map(new ErrorEncodingType(s"default for field ${field.name}", _))

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

  private def fieldEncoder[A, B](field: Field[A, B]): A => Either[AvroException, AvroValue] =
    a =>
      field.codec
        .encode(field.access(a))
        .left
        .map(new ErrorEncodingType(s"field ${field.name}", _))

  /**
    * Decodes the value of the writer field with the same name or an alias of the field, or uses the
    * default of the field if there is no such writer field.
    */
  private def fieldDecoder[A, B](
    field: Field[A, B],
    record: Schema.Record,
    definitions: Definitions
  ): Vector[AvroValue] => Either[AvroException, Any] = {
    val writerIndex =
      (field.name +: field.aliases).iterator
        .map(name => record.fields.indexWhere(_.name == name))
        .find(_ >= 0)

    writerIndex match {
      case Some(index) =>
        val decode = field.codec.resolveDecoder(record.fields(index).schema, definitions)
        values => decode(values(index)).left.map(new ErrorDecodingType(s"field ${field.name}", _))

      case None =>
        val result = field.default.toRight(new MissingField(field.name))
        _ => result
    }
  }

  private val EncodedNone: Either[AvroException, AvroValue] =
    Right(AvroValue.Union(0, AvroValue.Null))

  private val DecodedNone: Either[AvroException, Option[Nothing]] =
    Right(None)

  /**
    * The index of the union type to decode values of the writer schema with: the same type, or a
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

  /**
    * Returns `true` if the schema is the same kind of named schema, with the name of the writer
    * schema as its name or as an alias, comparing names using the function.
    */
  private def sameNamedType(writerSchema: Schema.Named, schema: Schema)(
    name: Schema.Name => String
  ): Boolean =
    schema match {
      case named: Schema.Named if SchemaNames.typeName(named) == SchemaNames.typeName(writerSchema) =>
        val aliases = named.aliases.map(SchemaNames.resolve(_, named.name.namespace))
        (named.name +: aliases).exists(name(_) == name(writerSchema.name))

      case _ =>
        false
    }

  /** Returns `true` if values of the writer schema can be promoted to the schema. */
  private def isPromotion(writerSchema: Schema, schema: Schema): Boolean =
    (writerSchema, schema) match {
      case (_: Schema.Int, _: Schema.Long | _: Schema.Float | _: Schema.Double) => true
      case (_: Schema.Long, _: Schema.Float | _: Schema.Double) => true
      case (_: Schema.Float, _: Schema.Double) => true
      case (_: Schema.String, _: Schema.Bytes) => true
      case (_: Schema.Bytes, _: Schema.String) => true
      case _ => false
    }

  /** The schema, if it is valid, including defaults. */
  private def validated(schema: Schema): Either[AvroException, Schema] =
    SchemaValidator.validate(schema, validateDefaults = true).map(_ => schema)

  /** A function decoding any value to the error. */
  private[vulcan] def failing[A](error: AvroException): AvroValue => Either[AvroException, A] = {
    val result = Left(error)
    _ => result
  }

  private[vulcan] def invalidValue(value: AvroValue, writerSchema: Schema): AvroException =
    new InvalidValue(value, SchemaNames.describe(writerSchema))
}
