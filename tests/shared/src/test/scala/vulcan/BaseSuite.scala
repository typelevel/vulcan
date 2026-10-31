/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Eq
import cats.Show
import cats.effect.IO
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import scodec.bits.ByteVector
import weaver.Expectations
import weaver.SimpleIOSuite
import weaver.SourceLocation
import weaver.scalacheck.Checkers

abstract class BaseSuite extends SimpleIOSuite with Checkers {
  implicit val byteVectorShow: Show[ByteVector] =
    Show.fromToString

  /** Expects the codec schema to be the given JSON. */
  def expectSchema[A](json: String)(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.same(Right(json), codec.schema.map(_.toString))

  /** Expects the codec schema to fail with the given error message. */
  def expectSchemaError[A](message: String)(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.eql(Left(message), codec.schema.left.map(_.message))

  /** Expects the codec to encode `a` as `value`. */
  def expectEncode[A](a: A, value: AvroValue)(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.same(Right(value), codec.encode(a))

  /** Expects encoding `a` to fail with the given error message. */
  def expectEncodeError[A](
    a: A,
    message: String
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.eql(Left(message), codec.encode(a).left.map(_.message))

  /** Expects the codec to decode `value`, written with the codec schema, as `a`. */
  def expectDecode[A](
    value: Either[AvroException, AvroValue],
    a: A
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expectDecode(value, codec.schema, a)

  /** Expects the codec to decode `value`, written with `schema`, as `a`. */
  def expectDecode[A](
    value: Either[AvroException, AvroValue],
    schema: Either[AvroException, Schema],
    a: A
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    whenSuccess((value, schema).tupled) { case (v, s) =>
      expect.same(Right(a), codec.decoder(s).flatMap(_.decode(v)))
    }

  /** Expects decoding `value`, written with the codec schema, to fail with the given error message. */
  def expectDecodeError[A](
    value: Either[AvroException, AvroValue],
    message: String
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expectDecodeError(value, codec.schema, message)

  /** Expects decoding `value`, written with `schema`, to fail with the given error message. */
  def expectDecodeError[A](
    value: Either[AvroException, AvroValue],
    schema: Either[AvroException, Schema],
    message: String
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    whenSuccess((value, schema).tupled) { case (v, s) =>
      expect.same(Left(message), codec.decoder(s).flatMap(_.decode(v)).left.map(_.message))
    }

  /** Expects `a` to roundtrip as an Avro value, as Avro binary, and as Avro JSON. */
  def expectRoundtrip[A](a: A)(implicit
    codec: Codec[A],
    eq: Eq[A] = Eq.fromUniversalEquals[A],
    loc: SourceLocation
  ): Expectations =
    whenSuccess(codec.schema) { schema =>
      val decoded = List(
        (codec.encoder, codec.decoder(schema)).flatMapN((e, d) => e.encode(a).flatMap(d.decode)),
        (codec.binaryEncoder, codec.binaryDecoder(schema)).flatMapN((e, d) => e.encode(a).flatMap(d.decode)),
        (codec.jsonEncoder, codec.jsonDecoder(schema)).flatMapN((e, d) => e.encode(a).flatMap(d.decode))
      )

      forEach(decoded)(d => expect.eql(Right(a), d.left.map(_.message)))
    }

  /** Expects generated values to roundtrip like [[expectRoundtrip]]. */
  def forallRoundtrip[A](implicit
    codec: Codec[A],
    arbitrary: Arbitrary[A],
    eq: Eq[A] = Eq.fromUniversalEquals[A],
    loc: SourceLocation
  ): IO[Expectations] = {
    implicit val show: Show[A] = Show.fromToString
    forall((a: A) => expectRoundtrip(a))
  }
}
