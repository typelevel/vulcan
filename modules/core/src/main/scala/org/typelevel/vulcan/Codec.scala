/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan

import cats.Defer
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
import org.typelevel.vulcan.AvroException.*
import org.typelevel.vulcan.internal.*
import scala.annotation.implicitNotFound
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scodec.bits.ByteVector

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
  final lazy val schema: Either[AvroException, Schema] =
    schemaIn(new Codec.SchemaContext).flatMap(Codec.validated)

  /** The value encoded as `null`, if there is one, used as the default of nullable fields. */
  def nullValue: Option[A] =
    None

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
      resolveDecoder(writerSchema, new Definitions(writerSchema))
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
    * Returns a `BinaryEncoder` for Avro binary using the provided writer schema,
    * or an error if the schema or writer schema is invalid, or if values cannot
    * be written using the writer schema.
    *
    * When encoding, values are resolved into the writer schema following Avro's
    * schema resolution, as if schema values were read using the writer schema.
    * Fields missing in the schema use the defaults of the writer schema, and
    * fields missing in the writer schema are left out. Values cannot be written
    * if reading them using the writer schema could fail, for example if a field
    * of the writer schema is missing in the schema and has no default, or if the
    * schemas have different logical types.
    */
  final def binaryEncoder(writerSchema: Schema): Either[AvroException, BinaryEncoder[A]] =
    schema.flatMap { schema =>
      if (schema == writerSchema) binaryEncoder
      else
        (ValueResolver(schema, writerSchema), BinaryEncoder(writerSchema)).mapN { (resolve, compiled) =>
          new BinaryEncoder[A] {
            override def encode(a: A): Either[AvroException, ByteVector] =
              Codec.this.encode(a).flatMap(resolve).flatMap(compiled.encode)
          }
        }
    }

  /**
    * Returns a `BinaryDecoder` for Avro binary written using the provided
    * writer schema, or an error if the schema or writer schema is invalid.
    *
    * To protect against invalid input, decoding fails for values with
    * records, arrays and maps nested more than 512 levels deep, and for
    * values with more than 1,000,000 array or map items which are encoded
    * using zero bytes, like nulls and empty records.
    */
  final def binaryDecoder(writerSchema: Schema): Either[AvroException, BinaryDecoder[A]] =
    (schema >> BinaryDecoder(writerSchema)).map { compiled =>
      val decoder = resolveDecoder(writerSchema, new Definitions(writerSchema))
      new BinaryDecoder[A] {
        override def decode(bytes: ByteVector): Either[AvroException, A] =
          compiled.decode(bytes) match {
            case Right(value) => decoder.decode(value)
            case left @ Left(_) => left.rightCast
          }
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
    * To protect against invalid input, decoding fails for values with
    * records, arrays and maps nested more than 512 levels deep.
    */
  final def jsonDecoder(writerSchema: Schema): Either[AvroException, JsonDecoder[A]] =
    (schema >> JsonDecoder(writerSchema)).map { compiled =>
      val decoder = resolveDecoder(writerSchema, new Definitions(writerSchema))
      new JsonDecoder[A] {
        override def decode(json: Json): Either[AvroException, A] =
          compiled.decode(json) match {
            case Right(value) => decoder.decode(value)
            case left @ Left(_) => left.rightCast
          }
      }
    }

  /**
    * Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    */
  final def imap[B](f: A => B)(g: B => A): Codec[B] =
    exmap(a => Right(f(a)))(b => Right(g(b)))

  /**
    * Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    *
    * Similar to [[Codec#imap]], except the mapping from `A` to `B` might be unsuccessful,
    * since `B` is narrower than `A`.
    */
  final def narrow[B](f: A => Either[AvroException, B])(g: B => A): Codec[B] =
    exmap(f)(b => Right(g(b)))

  /**
    * Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    *
    * Similar to [[Codec#imap]], except the mapping from `B` to `A` might be unsuccessful,
    * since `B` is wider than `A`.
    */
  final def widen[B](f: A => B)(g: B => Either[AvroException, A]): Codec[B] =
    exmap(a => Right(f(a)))(g)

  /**
    * Returns a new [[Codec]] which uses this [[Codec]] for encoding and decoding, mapping
    * back-and-forth between types `A` and `B`.
    *
    * Similar to [[Codec#imap]], except the mappings in both directions might be unsuccessful.
    */
  final def exmap[B](f: A => Either[AvroException, B])(g: B => Either[AvroException, A]): Codec[B] =
    new Codec.Exmap(this, f, g)

  /**
    * Returns a new [[Codec]] with the logical type added to its schema.
    *
    * The schema is an error if the logical type is invalid for the type
    * of the schema, or if the schema already has a logical type.
    *
    * Decoding requires writer schemas to have the same logical type.
    */
  final def withLogicalType(logicalType: LogicalType): Codec[A] =
    new Codec.LogicalTypeCodec(this, logicalType)

  /** Returns a new [[Codec]] that includes the type name in errors when encoding and decoding. */
  final def withTypeName(typeName: String): Codec[A] =
    new Codec.WithTypeName(this, typeName)

  /** Returns the schema without validating it, where enclosing records are references. */
  private[vulcan] def schemaIn(context: Codec.SchemaContext): Either[AvroException, Schema]

  /** Attempts to encode the specified value. */
  private[vulcan] def encode(a: A): Either[AvroException, AvroValue]

  /** Returns a decoder for values of the writer schema; neither a union nor a reference. */
  private[vulcan] def resolve(
    writerSchema: Schema,
    definitions: Definitions
  ): Decoder[A]

  private[vulcan] def resolveWithTypeName(
    writerSchema: Schema,
    definitions: Definitions,
    typeName: String
  ): Decoder[A] =
    new Codec.TypeNameDecoder(resolve(writerSchema, definitions), typeName)

  /** Returns a decoder for values of the writer schema, resolved once for named schemas. */
  private[vulcan] final def resolveDecoder(
    writerSchema: Schema,
    definitions: Definitions
  ): Decoder[A] =
    writerSchema match {
      case union: Schema.Union =>
        new Codec.UnionDecoder(this, union, definitions)

      case ref: Schema.Ref =>
        definitions.get(ref.fullName) match {
          case Some(named) => definitions.decoder(this, named)(resolve(named, definitions))
          case None => Codec.failing(new UndefinedName(ref.fullName))
        }

      case named: Schema.Named =>
        definitions.decoder(this, named)(resolve(named, definitions))

      case _ =>
        resolve(writerSchema, definitions)
    }

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
      .narrow { int =>
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
    new ArrayCodec[A, Chain[A]](initialized(codec, "Chain items"), _.length.toInt, _.iterator, chainBuilder)
      .withTypeName("Chain")

  /**
    * @group General
    */
  implicit lazy val char: Codec[Char] =
    Codec.string
      .narrow { string =>
        if (string.length == 1) Right(string.charAt(0))
        else Left(new InvalidChar(string.length))
      }(_.toString)
      .withTypeName("Char")

  /**
    * Returns a new [[Codec]] using the specified schema, encoder, and decoder function.
    *
    * The decoder function returns a [[Decoder]] for values of the writer schema, and is
    * called once for each writer schema. Values of unions are decoded using the schema
    * of the written type, so the writer schema is neither a union nor a reference. It
    * includes the definitions of the named schemas it references, so it can be passed
    * to [[Codec#decoder]] of other codecs.
    *
    * @group Create
    */
  final def custom[A](
    schema: Either[AvroException, Schema],
    encoder: Encoder[A],
    decoder: Schema => Either[AvroException, Decoder[A]]
  ): Codec[A] =
    new CustomCodec(schema, encoder, decoder)

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
    val enumName = Schema.Name(name, Some(namespace))
    new EnumCodec(enumName, symbols.toVector, encode, decode, default, doc, aliases.toVector, props)
      .withTypeName(enumName.fullName)
  }

  /**
    * Returns a new [[Codec]] whose schema, encoding, and decoding fail with the specified error.
    *
    * @group Create
    */
  final def fail[A](error: AvroException): Codec[A] = {
    val result = Left(error)
    new CustomCodec[A](result, _ => result, _ => result)
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
    val fixedName = Schema.Name(name, Some(namespace))
    new FixedCodec(fixedName, size, encode, decode, doc, aliases.toVector, props)
      .withTypeName(fixedName.fullName)
  }

  /**
    * @group General
    */
  implicit lazy val float: Codec[Float] =
    FloatCodec

  /**
    * @group JavaTime
    */
  implicit lazy val instant: Codec[Instant] =
    Codec.long
      .widen(Instant.ofEpochMilli(_)) { instant =>
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
      .widen(InstantMicros.fromEpochMicro(_)) { instantMicros =>
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
    initialized(codec, "Left value").imap(Left[A, B](_))(_.value)

  /**
    * @group Collection
    */
  implicit final def list[A](implicit codec: Codec[A]): Codec[List[A]] =
    new ArrayCodec[A, List[A]](initialized(codec, "List items"), _.length, _.iterator, _ => List.newBuilder)
      .withTypeName("List")

  /**
    * @group JavaTime
    */
  implicit lazy val localDate: Codec[LocalDate] =
    Codec.int
      .widen(days => LocalDate.ofEpochDay(days.toLong)) { date =>
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
      .widen { millis =>
        LocalDateTime.ofInstant(Instant.ofEpochMilli(millis), ZoneOffset.UTC)
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
      .narrow { micros =>
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
      .narrow { millis =>
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
  implicit final def map[A](implicit codec: Codec[A]): Codec[Map[String, A]] = {
    val values = initialized(codec, "Map values")
    new MapCodec[A, Map[String, A]](values, _.size, _.iterator, _ => Map.newBuilder, hashed = true)
      .withTypeName("Map")
  }

  /**
    * @group General
    */
  implicit lazy val none: Codec[None.type] =
    new NullCodec(None, "None")

  /**
    * @group Cats
    */
  implicit final def nonEmptyChain[A](implicit codec: Codec[A]): Codec[NonEmptyChain[A]] = {
    val items = initialized(codec, "NonEmptyChain items")
    new ArrayCodec[A, Chain[A]](items, _.length.toInt, _.iterator, chainBuilder)
      .narrow(NonEmptyChain.fromChain(_).toRight(new EmptyCollection()))(_.toChain)
      .withTypeName("NonEmptyChain")
  }

  /**
    * @group Cats
    */
  implicit final def nonEmptyList[A](implicit codec: Codec[A]): Codec[NonEmptyList[A]] = {
    val items = initialized(codec, "NonEmptyList items")
    new ArrayCodec[A, List[A]](items, _.length, _.iterator, _ => List.newBuilder)
      .narrow(NonEmptyList.fromList(_).toRight(new EmptyCollection()))(_.toList)
      .withTypeName("NonEmptyList")
  }

  /**
    * @group Cats
    */
  implicit final def nonEmptyMap[A](implicit codec: Codec[A]): Codec[NonEmptyMap[String, A]] = {
    val values = initialized(codec, "NonEmptyMap values")
    new MapCodec[A, SortedMap[String, A]](values, _.size, _.iterator, _ => SortedMap.newBuilder)
      .narrow(NonEmptyMap.fromMap(_).toRight(new EmptyCollection()))(_.toSortedMap)
      .withTypeName("NonEmptyMap")
  }

  /**
    * @group Cats
    */
  implicit final def nonEmptySet[A](implicit
    codec: Codec[A],
    ordering: Ordering[A]
  ): Codec[NonEmptySet[A]] = {
    val items = initialized(codec, "NonEmptySet items")
    new ArrayCodec[A, SortedSet[A]](items, _.size, _.iterator, _ => SortedSet.newBuilder)
      .narrow(NonEmptySet.fromSet(_).toRight(new EmptyCollection()))(_.toSortedSet)
      .withTypeName("NonEmptySet")
  }

  /**
    * @group Cats
    */
  implicit final def nonEmptyVector[A](implicit codec: Codec[A]): Codec[NonEmptyVector[A]] = {
    val items = initialized(codec, "NonEmptyVector items")
    new ArrayCodec[A, Vector[A]](items, _.length, _.iterator, vectorBuilder)
      .narrow(NonEmptyVector.fromVector(_).toRight(new EmptyCollection()))(_.toVector)
      .withTypeName("NonEmptyVector")
  }

  /**
    * Returns a [[Codec]] for options, encoded as a union with `null` first.
    *
    * If the underlying codec is for a union, the types of that union are included in the union.
    *
    * @group General
    */
  implicit final def option[A](implicit codec: Codec[A]): Codec[Option[A]] =
    new OptionCodec(initialized(codec, "Option value")).withTypeName("Option")

  /**
    * Returns a new record [[Codec]] for type `A`.
    *
    * Fields are decoded from the writer field with the same name or the name of an alias of the
    * field, and otherwise using the default of the field. Other writer fields are ignored.
    *
    * The fields are built when the codec is first used, so fields can use codecs defined later,
    * including the codec itself for recursive types.
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
    val recordName = Schema.Name(name, Some(namespace))
    new RecordCodec(recordName, doc, aliases.toVector, props, f)
      .withTypeName(recordName.fullName)
  }

  /**
    * Returns a [[Codec]] for a recursive type, providing the
    * returned codec for recursive references within the type.
    *
    * The schema generation will fail if the codec refers to
    * itself other than within a record, since Avro schemas
    * refer to records by name.
    *
    * This function is useful for codecs that are `def`s, like
    * codecs for generic types, which would otherwise create a
    * new codec for every nested record.
    *
    * @group Create
    */
  final def recursive[A](f: Codec[A] => Codec[A]): Codec[A] = {
    lazy val codec: Codec[A] = f(new Deferred(() => codec))
    codec
  }

  /**
    * @group General
    */
  implicit final def right[A, B](implicit codec: Codec[B]): Codec[Right[A, B]] =
    initialized(codec, "Right value").imap(Right[A, B](_))(_.value)

  /**
    * @group Collection
    */
  implicit final def seq[A](implicit codec: Codec[A]): Codec[Seq[A]] =
    new ArrayCodec[A, Seq[A]](initialized(codec, "Seq items"), _.length, _.iterator, _ => Seq.newBuilder)
      .withTypeName("Seq")

  /**
    * @group Collection
    */
  implicit final def set[A](implicit codec: Codec[A]): Codec[Set[A]] = {
    val items = initialized(codec, "Set items")
    new ArrayCodec[A, Set[A]](items, _.size, _.iterator, _ => Set.newBuilder, hashed = true)
      .withTypeName("Set")
  }

  /**
    * @group General
    */
  implicit lazy val short: Codec[Short] =
    Codec.int
      .narrow { int =>
        if (int.isValidShort) Right(int.toShort)
        else Left(new ValueOutOfRange("Int", int.toLong, Short.MinValue.toLong, Short.MaxValue.toLong))
      }(_.toInt)
      .withTypeName("Short")

  /**
    * @group General
    */
  implicit final def some[A](implicit codec: Codec[A]): Codec[Some[A]] =
    initialized(codec, "Some value").imap(Some(_))(_.value)

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
      .narrow { string =>
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
    new ArrayCodec[A, Vector[A]](initialized(codec, "Vector items"), _.length, _.iterator, vectorBuilder)
      .withTypeName("Vector")

  /**
    * @group Cats
    */
  implicit final val codecDefer: Defer[Codec] =
    new Defer[Codec] {
      override final def defer[A](codec: => Codec[A]): Codec[A] =
        new Deferred(() => codec)
    }

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
          Chain.one(Alt(initialized(codec, "union alternative"), prism))

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

    def order: Schema.Field.Order

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
      order: Schema.Field.Order = Schema.Field.Order.Ascending,
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
          order: Schema.Field.Order,
          aliases: Seq[String],
          props: JsonObject
        )(implicit codec: Codec[B]): FreeApplicative[Field[Any, *], B] = {
          val _name = name
          val _access = access
          val _codec = initialized(codec, s"field $name")
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
              override val order: Schema.Field.Order = _order
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

  private[vulcan] final class Exmap[A, B](
    val codec: Codec[A],
    val f: A => Either[AvroException, B],
    val g: B => Either[AvroException, A]
  ) extends Codec[B] {
    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      codec.schemaIn(context)

    override def encode(b: B): Either[AvroException, AvroValue] =
      g(b).flatMap(codec.encode)

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): Decoder[B] =
      new ExmapDecoder(codec.resolve(writerSchema, definitions), f, null)

    override def resolveWithTypeName(
      writerSchema: Schema,
      definitions: Definitions,
      typeName: String
    ): Decoder[B] =
      new ExmapDecoder(codec.resolve(writerSchema, definitions), f, typeName)
  }

  private[vulcan] final class CustomCodec[A](
    val customSchema: Either[AvroException, Schema],
    val customEncoder: Encoder[A],
    val customDecoder: Schema => Either[AvroException, Decoder[A]]
  ) extends Codec[A] {
    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      customSchema

    override def encode(a: A): Either[AvroException, AvroValue] =
      customEncoder.encode(a)

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): Decoder[A] =
      customDecoder(definitions.expanded(writerSchema)) match {
        case Right(decoder) => decoder
        case Left(error) => failing(error)
      }
  }

  private[vulcan] final class WithTypeName[A](
    val codec: Codec[A],
    val typeName: String
  ) extends Codec[A] {
    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      codec.schemaIn(context)

    override def encode(a: A): Either[AvroException, AvroValue] =
      codec.encode(a).left.map(new ErrorEncodingType(typeName, _))

    override def nullValue: Option[A] =
      codec.nullValue

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): Decoder[A] =
      codec.resolveWithTypeName(writerSchema, definitions, typeName)
  }

  /** Codec for collections encoded as arrays, with a builder for the number of items. */
  private[vulcan] final class ArrayCodec[A, C](
    val items: Codec[A],
    val size: C => Int,
    val iterator: C => Iterator[A],
    val newBuilder: Int => mutable.Builder[A, C],
    val hashed: Boolean = false
  ) extends Codec[C] {
    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      items.schemaIn(context).map(Schema.Array(_))

    override def encode(collection: C): Either[AvroException, AvroValue] =
      build(iterator(collection), vectorBuilder[AvroValue](size(collection)))(items.encode)
        .map(AvroValue.Array(_))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): Decoder[C] =
      resolveWithTypeName(writerSchema, definitions, null)

    override def resolveWithTypeName(
      writerSchema: Schema,
      definitions: Definitions,
      typeName: String
    ): Decoder[C] =
      writerSchema match {
        case array: Schema.Array =>
          new ArrayDecoder(this, array, items.resolveDecoder(array.items, definitions), typeName)

        case _ =>
          failing(includeTypeName(typeName, new UnexpectedSchemaType(writerSchema, "array")))
      }
  }

  /**
    * Codec for maps with string keys, with a builder for the number of entries. Decoding hashed
    * maps fails for keys with the same hash code beyond the limit.
    */
  private[vulcan] final class MapCodec[A, M](
    val values: Codec[A],
    val size: M => Int,
    val iterator: M => Iterator[(String, A)],
    val newBuilder: Int => mutable.Builder[(String, A), M],
    val hashed: Boolean = false
  ) extends Codec[M] {
    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      values.schemaIn(context).map(Schema.Map(_))

    override def encode(map: M): Either[AvroException, AvroValue] =
      build(iterator(map), vectorBuilder[(String, AvroValue)](size(map))) { case (key, value) =>
        values.encode(value).map((key, _))
      }.map(AvroValue.Map(_))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): Decoder[M] =
      resolveWithTypeName(writerSchema, definitions, null)

    override def resolveWithTypeName(
      writerSchema: Schema,
      definitions: Definitions,
      typeName: String
    ): Decoder[M] =
      writerSchema match {
        case map: Schema.Map =>
          new MapDecoder(this, map, values.resolveDecoder(map.values, definitions), typeName)

        case _ =>
          failing(includeTypeName(typeName, new UnexpectedSchemaType(writerSchema, "map")))
      }
  }

  /** Codec for options, as unions with `null` first, including the types of unions. */
  private[vulcan] final class OptionCodec[A](val codec: Codec[A]) extends Codec[Option[A]] {
    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      codec.schemaIn(context).map { schema =>
        val types =
          schema match {
            case union: Schema.Union => union.types
            case _ => Vector(schema)
          }

        Schema.Union(Schema.Null +: types)
      }

    /** Whether the types of the union of the codec are included in the union of the option. */
    lazy val includesUnion: Boolean =
      isUnion(codec)

    override def encode(option: Option[A]): Either[AvroException, AvroValue] =
      option match {
        case Some(a) =>
          codec.encode(a).flatMap {
            case AvroValue.Union(index, value) if includesUnion =>
              Right(AvroValue.Union(index + 1, value))

            case value if !includesUnion =>
              Right(AvroValue.Union(1, value))

            case value =>
              Left(new InvalidValue(value, "union"))
          }

        case None =>
          EncodedNone
      }

    override val nullValue: Option[Option[A]] =
      Some(None)

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): Decoder[Option[A]] =
      resolveWithTypeName(writerSchema, definitions, null)

    override def resolveWithTypeName(
      writerSchema: Schema,
      definitions: Definitions,
      typeName: String
    ): Decoder[Option[A]] =
      writerSchema match {
        case _: Schema.Null => {
          case AvroValue.Null => DecodedNone
          case value => Left(includeTypeName(typeName, invalidValue(value, writerSchema)))
        }

        case _ =>
          new SomeDecoder(codec.resolve(writerSchema, definitions), typeName)
      }
  }

  /** Codec for unions, which decodes using the alternative for the written type. */
  private[vulcan] final class UnionCodec[A](val alternatives: Chain[Alt[A]]) extends Codec[A] {
    val alts: Vector[Alt[A]] =
      alternatives.toVector

    private lazy val types: Either[AvroException, Vector[Schema]] =
      schema.map {
        case union: Schema.Union => union.types
        case schema => Vector(schema)
      }

    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      build(alts.iterator, vectorBuilder[Schema](alts.size))(_.codec.schemaIn(context))
        .map(Schema.Union(_))

    override def encode(a: A): Either[AvroException, AvroValue] =
      encodeWith(a, 0)

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
    ): Decoder[A] =
      resolveWithTypeName(writerSchema, definitions, null)

    override def resolveWithTypeName(
      writerSchema: Schema,
      definitions: Definitions,
      typeName: String
    ): Decoder[A] =
      types match {
        case Right(types) =>
          unionType(writerSchema, types) match {
            case Some(index) =>
              val alt = alts(index)
              new AltDecoder(alt.codec.resolve(writerSchema, definitions), alt.prism, typeName)

            case None =>
              failing(includeTypeName(typeName, new MissingAlternative(writerSchema)))
          }

        case Left(error) =>
          failing(includeTypeName(typeName, error))
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
    val f: FieldBuilder[A] => FreeApplicative[Field[A, *], A]
  ) extends Codec[A] {

    /** The fields, built on first use, so the fields can refer to the codec itself. */
    lazy val fields: FreeApplicative[Field[A, *], A] =
      f(FieldBuilder.instance)

    /** The fields, in the order of the record. */
    lazy val fieldVector: Vector[Field[A, _]] =
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

    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      (context.enclosing.get(name.fullName), context.records.get(this)) match {
        case (Some(enclosing), _) if enclosing eq this =>
          Right(Schema.Ref(name))

        case (Some(_), _) =>
          Left(new RecursiveCodecMismatch(name.fullName))

        case (None, Some(schema)) =>
          Right(schema)

        case (None, None) =>
          val deferred = context.deferred
          context.enclosing.update(name.fullName, this)
          context.deferred = Set.empty
          val schema = recordSchema(context)
          context.enclosing.remove(name.fullName)
          context.deferred = deferred
          schema.foreach(context.records.update(this, _))
          schema
      }

    private def recordSchema(context: SchemaContext): Either[AvroException, Schema.Record] =
      build(fieldVector.iterator, vectorBuilder[Schema.Field](fieldVector.size))(fieldSchema(_, context))
        .map(Schema.Record(name, _).withDoc(doc).withAliases(aliases).withProps(props))

    override def encode(a: A): Either[AvroException, AvroValue] =
      build(encoders.iterator, vectorBuilder[AvroValue](encoders.size))(_(a))
        .map(AvroValue.Record(_))

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): Decoder[A] =
      resolveWithTypeName(writerSchema, definitions, null)

    override def resolveWithTypeName(
      writerSchema: Schema,
      definitions: Definitions,
      typeName: String
    ): Decoder[A] =
      writerSchema match {
        case record: Schema.Record =>
          val decoders = fieldVector.map(fieldDecoder(_, record, definitions)).toArray
          new RecordDecoder(record, decoders, construct, typeName)

        case _ =>
          failing(includeTypeName(typeName, new UnexpectedSchemaType(writerSchema, "record")))
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

    private lazy val enumSchema: Either[AvroException, Schema] =
      Right {
        Schema
          .Enum(name, symbols)
          .withDefault(default.map(encodeSymbol))
          .withDoc(doc)
          .withAliases(aliases)
          .withProps(props)
      }

    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      enumSchema

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
    ): Decoder[A] =
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
    private val fixedSchema: Either[AvroException, Schema] =
      Right(Schema.Fixed(name, size).withDoc(doc).withAliases(aliases).withProps(props))

    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      fixedSchema

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
    ): Decoder[A] =
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
    private val decimalSchema: Either[AvroException, Schema] =
      if (precision <= 0)
        Left(new InvalidDecimalPrecision(precision))
      else if (scale < 0 || scale > precision)
        Left(new InvalidDecimalScale(scale, precision))
      else
        Right(Schema.Bytes.withLogicalType(LogicalType.decimal(precision, scale)))

    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      decimalSchema

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
    ): Decoder[BigDecimal] =
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
    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      codec.schemaIn(context).flatMap(addLogicalType(_, logicalType))

    override def encode(a: A): Either[AvroException, AvroValue] =
      codec.encode(a)

    /** Checks the logical type after decoding, so errors for the writer schema type come first. */
    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): Decoder[A] = {
      val decoder = codec.resolve(writerSchema, definitions)
      writerSchema.logicalType match {
        case Some(writerLogicalType) if writerLogicalType == logicalType =>
          decoder
        case _ =>
          value => decoder.decode(value) >> Left(new UnexpectedLogicalType(writerSchema))
      }
    }
  }

  /** Codec for a primitive type, which also decodes types which can be promoted to the type. */
  private[vulcan] sealed abstract class PrimitiveCodec[A](
    val typeName: String,
    primitiveSchema: Schema
  ) extends Codec[A] {
    private[this] val result: Either[AvroException, Schema] =
      Right(primitiveSchema)

    override final def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      result

    protected final def invalid(value: AvroValue, writerSchema: Schema): Either[AvroException, A] =
      Left(new ErrorDecodingType(typeName, invalidValue(value, writerSchema)))

    protected final def unexpected(writerSchema: Schema): Decoder[A] =
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
    ): Decoder[A] =
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
    ): Decoder[Boolean] =
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
    ): Decoder[Int] =
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
    ): Decoder[Long] =
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
    ): Decoder[Float] =
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
    ): Decoder[Double] =
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
    ): Decoder[ByteVector] =
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
    ): Decoder[String] =
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

  /** Codec which defers to the codec of a function, called once, for recursive codecs. */
  private[vulcan] final class Deferred[A](thunk: () => Codec[A]) extends Codec[A] {
    private lazy val next: Codec[A] =
      thunk()

    lazy val codec: Codec[A] = {
      val visited = mutable.Set.empty[Deferred[A]]

      @tailrec def loop(deferred: Deferred[A]): Codec[A] =
        if (!visited.add(deferred)) fail(new RecursiveCodecWithoutRecord())
        else
          deferred.next match {
            case next: Deferred[A] => loop(next)
            case next => next
          }

      loop(this)
    }

    override def schemaIn(context: SchemaContext): Either[AvroException, Schema] =
      if (context.deferred.contains(this))
        Left(new RecursiveCodecWithoutRecord())
      else {
        val deferred = context.deferred
        context.deferred = deferred + this
        val schema = codec.schemaIn(context)
        context.deferred = deferred
        schema
      }

    override def encode(a: A): Either[AvroException, AvroValue] =
      codec.encode(a)

    override def nullValue: Option[A] =
      codec.nullValue

    override def resolve(
      writerSchema: Schema,
      definitions: Definitions
    ): Decoder[A] =
      codec.resolve(writerSchema, definitions)

    override def resolveWithTypeName(
      writerSchema: Schema,
      definitions: Definitions,
      typeName: String
    ): Decoder[A] =
      codec.resolveWithTypeName(writerSchema, definitions, typeName)
  }

  private[vulcan] final class SchemaContext {
    val enclosing: mutable.Map[String, RecordCodec[_]] =
      mutable.Map.empty

    val records: mutable.Map[RecordCodec[_], Schema.Record] =
      mutable.Map.empty

    var deferred: Set[Deferred[_]] =
      Set.empty
  }

  /** Decodes values of a writer union using the decoder for the written type. */
  private final class UnionDecoder[A](
    codec: Codec[A],
    union: Schema.Union,
    definitions: Definitions
  ) extends Decoder[A] {
    private val decoders: Array[Decoder[A]] =
      union.types.map(codec.resolveDecoder(_, definitions)).toArray

    override def decode(value: AvroValue): Either[AvroException, A] =
      value match {
        case AvroValue.Union(index, typeValue) if index >= 0 && index < decoders.length =>
          decoders(index).decode(typeValue)

        case AvroValue.Union(index, _) =>
          Left(new InvalidUnionValue(index, decoders.length))

        case _ =>
          Left(invalidValue(value, union))
      }
  }

  private final class RecordDecoder[A](
    writerSchema: Schema.Record,
    fields: Array[FieldDecoder],
    construct: Array[Any] => A,
    typeName: String
  ) extends Decoder[A] {
    override def decode(value: AvroValue): Either[AvroException, A] =
      value match {
        case AvroValue.Record(values) if values.size == writerSchema.fields.size =>
          val decoded = new Array[Any](fields.length)
          var error: AvroException = null
          var index = 0
          while ((error eq null) && index < fields.length) {
            val field = fields(index)
            val result: Either[AvroException, Any] =
              if (field.writerIndex >= 0) field.decoder.decode(values(field.writerIndex))
              else field.default

            result match {
              case Right(fieldValue) =>
                decoded(index) = fieldValue
                index += 1

              case Left(fieldError) =>
                error =
                  if (field.writerIndex >= 0) new ErrorDecodingType(s"field ${field.name}", fieldError)
                  else fieldError
            }
          }

          if (error eq null) Right(construct(decoded))
          else Left(includeTypeName(typeName, error))

        case AvroValue.Record(values) =>
          val error = new InvalidRecordValue(values.size, writerSchema.fullName, writerSchema.fields.size)
          Left(includeTypeName(typeName, error))

        case _ =>
          Left(includeTypeName(typeName, invalidValue(value, writerSchema)))
      }
  }

  /**
    * Decodes a field from the writer field at the index, or uses the default
    * if the index is negative, since the writer schema is without the field.
    */
  private final class FieldDecoder(
    val name: String,
    val writerIndex: Int,
    val decoder: Decoder[_],
    val default: Either[AvroException, Any]
  )

  private final class ArrayDecoder[A, C](
    codec: ArrayCodec[A, C],
    writerSchema: Schema.Array,
    items: Decoder[A],
    typeName: String
  ) extends Decoder[C] {
    override def decode(value: AvroValue): Either[AvroException, C] =
      value match {
        case AvroValue.Array(values) =>
          val hashed = codec.hashed && values.size > MaxEqualHashCodes
          val builder: mutable.Builder[A, Any] =
            if (hashed) vectorBuilder[A](values.size) else codec.newBuilder(values.size)

          val iterator = values.iterator
          var error: AvroException = null
          while ((error eq null) && iterator.hasNext)
            items.decode(iterator.next()) match {
              case Right(item) => builder += item
              case Left(itemError) => error = itemError
            }

          if (error ne null) Left(includeTypeName(typeName, error))
          else if (!hashed) Right(builder.result().asInstanceOf[C])
          else {
            val decoded = builder.result().asInstanceOf[Vector[A]]
            if (hashCollisions(decoded.size)(decoded(_)))
              Left(includeTypeName(typeName, new HashCollisionsExceeded("items", MaxEqualHashCodes)))
            else Right((codec.newBuilder(decoded.size) ++= decoded).result())
          }

        case _ =>
          Left(includeTypeName(typeName, invalidValue(value, writerSchema)))
      }
  }

  private final class MapDecoder[A, M](
    codec: MapCodec[A, M],
    writerSchema: Schema.Map,
    values: Decoder[A],
    typeName: String
  ) extends Decoder[M] {
    override def decode(value: AvroValue): Either[AvroException, M] =
      value match {
        case AvroValue.Map(entries) if codec.hashed && hashCollisions(entries.size)(entries(_)._1) =>
          Left(includeTypeName(typeName, new HashCollisionsExceeded("keys", MaxEqualHashCodes)))

        case AvroValue.Map(entries) =>
          val builder = codec.newBuilder(entries.size)
          val iterator = entries.iterator
          var error: AvroException = null
          while ((error eq null) && iterator.hasNext) {
            val entry = iterator.next()
            values.decode(entry._2) match {
              case Right(entryValue) => builder += ((entry._1, entryValue))
              case Left(valueError) => error = valueError
            }
          }

          if (error eq null) Right(builder.result())
          else Left(includeTypeName(typeName, error))

        case _ =>
          Left(includeTypeName(typeName, invalidValue(value, writerSchema)))
      }
  }

  /** Decodes values of options which are not `null`. */
  private final class SomeDecoder[A](decoder: Decoder[A], typeName: String) extends Decoder[Option[A]] {
    override def decode(value: AvroValue): Either[AvroException, Option[A]] =
      decoder.decode(value) match {
        case Right(a) => Right(Some(a))
        case Left(error) => Left(includeTypeName(typeName, error))
      }
  }

  /** Decodes values of the written type using the alternative of a union. */
  private final class AltDecoder[A, B](
    decoder: Decoder[B],
    prism: Prism[A, B],
    typeName: String
  ) extends Decoder[A] {
    override def decode(value: AvroValue): Either[AvroException, A] =
      decoder.decode(value) match {
        case Right(b) => Right(prism.reverseGet(b))
        case Left(error) => Left(includeTypeName(typeName, error))
      }
  }

  private final class ExmapDecoder[A, B](
    decoder: Decoder[A],
    f: A => Either[AvroException, B],
    typeName: String
  ) extends Decoder[B] {
    override def decode(value: AvroValue): Either[AvroException, B] =
      decoder.decode(value) match {
        case Right(a) =>
          f(a) match {
            case Left(error) if typeName ne null => Left(new ErrorDecodingType(typeName, error))
            case result => result
          }

        case Left(error) =>
          Left(includeTypeName(typeName, error))
      }
  }

  /** Decodes values using the decoder, including the type name in errors. */
  private final class TypeNameDecoder[A](decoder: Decoder[A], typeName: String) extends Decoder[A] {
    override def decode(value: AvroValue): Either[AvroException, A] =
      decoder.decode(value) match {
        case Left(error) => Left(new ErrorDecodingType(typeName, error))
        case result => result
      }
  }

  /** The error, including the type name unless it is `null`. */
  private def includeTypeName(typeName: String, error: AvroException): AvroException =
    if (typeName eq null) error else new ErrorDecodingType(typeName, error)

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

  /** The limit for distinct map keys and set items with the same hash code, added in quadratic time. */
  private[vulcan] final val MaxEqualHashCodes: Int = 64

  /**
    * Returns `true` if more than [[MaxEqualHashCodes]] distinct keys have the same hash code. Sorts
    * the hash codes, since counting them in a hash table could be slowed down by crafted keys too.
    */
  private def hashCollisions(size: Int)(key: Int => Any): Boolean =
    size > MaxEqualHashCodes && {
      // Hash codes in the high bits and indices in the low bits, so equal hash codes become adjacent
      val sorted = Array.tabulate(size)(index => (key(index).##.toLong << 32) | index.toLong)
      Arrays.sort(sorted)

      def distinctKeys(start: Int, end: Int): Int = {
        val keys = mutable.ArrayBuffer.empty[Any]
        var index = start
        while (index < end && keys.size <= MaxEqualHashCodes) {
          val next = key(sorted(index).toInt)
          if (!keys.contains(next)) keys += next
          index += 1
        }
        keys.size
      }

      @tailrec def loop(start: Int): Boolean =
        if (start == size) false
        else {
          var end = start + 1
          while (end < size && (sorted(end) >> 32) == (sorted(start) >> 32)) end += 1
          if (end - start > MaxEqualHashCodes && distinctKeys(start, end) > MaxEqualHashCodes) true
          else loop(end)
        }

      loop(0)
    }

  /** The schema with the logical type, unless it has a logical type or the logical type is invalid. */
  private def addLogicalType(schema: Schema, logicalType: LogicalType): Either[AvroException, Schema] = {
    val added: Option[Schema] =
      schema match {
        case _ if schema.props.contains("logicalType") => None
        case int: Schema.Int => Some(int.withLogicalType(logicalType))
        case long: Schema.Long => Some(long.withLogicalType(logicalType))
        case bytes: Schema.Bytes => Some(bytes.withLogicalType(logicalType))
        case string: Schema.String => Some(string.withLogicalType(logicalType))
        case fixed: Schema.Fixed => Some(fixed.withLogicalType(logicalType))
        case _ => None
      }

    added
      .filter(_.logicalType.contains(logicalType))
      .toRight(new UnsupportedLogicalType(logicalType, schema))
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

      // The magnitude is at least 2^(bitLength - 1), so with 0.30102 below log10(2), it has at least
      // this many digits. Checked first, since computing the precision of large numbers is slow
      val minDigits = (unscaled.bitLength - 1L) * 30102L / 100000L + 1L

      if (minDigits > precision) Left(new DecodeDecimalPrecisionExceeded(precision))
      else {
        val decimal = BigDecimal(new java.math.BigDecimal(unscaled, scale))
        if (decimal.precision <= precision) Right(decimal)
        else Left(new DecodeDecimalPrecisionExceeded(precision))
      }
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
  private def fieldSchema[A, B](
    field: Field[A, B],
    context: SchemaContext
  ): Either[AvroException, Schema.Field] =
    for {
      schema <- field.codec.schemaIn(context)
      default <- field.default match {
        case Some(default) =>
          field.codec
            .encode(default)
            .flatMap(defaultJson(field.codec, _))
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
      .withOrder(field.order)
      .withAliases(field.aliases.toVector)
      .withProps(field.props)

  /** The value encoded by the codec as a default value in schemas, without building the codec schema. */
  private def defaultJson(codec: Codec[_], value: AvroValue): Either[AvroException, Json] =
    (codec, value) match {
      case (codec: WithTypeName[_], _) => defaultJson(codec.codec, value)
      case (codec: Exmap[_, _], _) => defaultJson(codec.codec, value)
      case (codec: LogicalTypeCodec[_], _) => defaultJson(codec.codec, value)
      case (codec: Deferred[_], _) => defaultJson(codec.codec, value)
      case (codec: CustomCodec[_], _) =>
        codec.schema.flatMap(JsonEncoder.withoutUnionTypes).flatMap(_.encode(value))

      case (codec: RecordCodec[_], AvroValue.Record(values)) if values.size == codec.fieldVector.size =>
        val fields = codec.fieldVector.iterator.zip(values.iterator)
        build(fields, vectorBuilder[(String, Json)](values.size)) { case (field, fieldValue) =>
          defaultJson(field.codec, fieldValue).map((field.name, _))
        }.map(Json.fromFields)

      case (codec: UnionCodec[_], AvroValue.Union(index, typeValue)) if codec.alts.indices.contains(index) =>
        defaultJson(codec.alts(index).codec, typeValue)

      case (_: OptionCodec[_], AvroValue.Union(0, _)) =>
        Right(Json.Null)

      case (codec: OptionCodec[_], AvroValue.Union(index, typeValue)) =>
        if (codec.includesUnion) defaultJson(codec.codec, AvroValue.Union(index - 1, typeValue))
        else defaultJson(codec.codec, typeValue)

      case (codec: ArrayCodec[_, _], AvroValue.Array(items)) =>
        build(items.iterator, vectorBuilder[Json](items.size))(defaultJson(codec.items, _))
          .map(Json.fromValues)

      case (codec: MapCodec[_, _], AvroValue.Map(entries)) =>
        build(entries.iterator, vectorBuilder[(String, Json)](entries.size)) { case (key, entryValue) =>
          defaultJson(codec.values, entryValue).map((key, _))
        }.map(Json.fromFields)

      case (_, AvroValue.Null) => Right(Json.Null)
      case (_, AvroValue.Boolean(boolean)) => Right(Json.fromBoolean(boolean))
      case (_, AvroValue.Int(int)) => Right(Json.fromInt(int))
      case (_, AvroValue.Long(long)) => Right(Json.fromLong(long))
      case (_, AvroValue.Float(float)) => Right(JsonNumbers.float(float))
      case (_, AvroValue.Double(double)) => Right(JsonNumbers.double(double))
      case (_, AvroValue.Bytes(bytes)) => Right(JsonEncoder.bytesJson(bytes))
      case (_, AvroValue.String(string)) => Right(Json.fromString(string))
      case (_, AvroValue.Enum(symbol)) => Right(Json.fromString(symbol))
      case (_, AvroValue.Fixed(bytes)) => Right(JsonEncoder.bytesJson(bytes))
      case (_, _) => Left(new InvalidValue(value, "codec"))
    }

  /** Returns `true` if the codec is for a union, without building its schema. */
  @tailrec private def isUnion(codec: Codec[_]): Boolean =
    codec match {
      case _: UnionCodec[_] | _: OptionCodec[_] => true
      case codec: WithTypeName[_] => isUnion(codec.codec)
      case codec: Exmap[_, _] => isUnion(codec.codec)
      case codec: LogicalTypeCodec[_] => isUnion(codec.codec)
      case codec: Deferred[_] => isUnion(codec.codec)
      case codec: CustomCodec[_] => codec.customSchema.exists(_.isInstanceOf[Schema.Union])
      case _ => false
    }

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
  ): FieldDecoder = {
    val writerIndex =
      (field.name +: field.aliases).iterator
        .map(name => record.fields.indexWhere(_.name == name))
        .find(_ >= 0)

    writerIndex match {
      case Some(index) =>
        val decoder = field.codec.resolveDecoder(record.fields(index).schema, definitions)
        new FieldDecoder(field.name, index, decoder, null)

      case None =>
        new FieldDecoder(field.name, -1, null, field.default.toRight(new MissingField(field.name)))
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
        val aliases = named.aliases.map(Schema.Name(_, named.name.namespace))
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

  /** A decoder decoding any value to the error. */
  private[vulcan] def failing[A](error: AvroException): Decoder[A] = {
    val result = Left(error)
    _ => result
  }

  /** The codec, or a codec failing with an error if the codec is `null`. */
  private def initialized[A](codec: Codec[A], description: => String): Codec[A] =
    if (codec ne null) codec else fail(new UninitializedCodec(description))

  private[vulcan] def invalidValue(value: AvroValue, writerSchema: Schema): AvroException =
    new InvalidValue(value, SchemaNames.describe(writerSchema))
}
