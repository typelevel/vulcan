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
  implicit val byteVectorShow: Show[ByteVector] = Show.fromToString

  def expectSchemaIs[A](
    expectedSchema: String
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.same(Right(expectedSchema), codec.schema.map(_.toString))

  def expectEncodeIs[A](
    a: A,
    encoded: Either[AvroException, AvroValue]
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.same(encoded, codec.encode(a))

  def expectDecodeIs[A](
    value: Either[AvroException, AvroValue],
    decoded: Either[AvroException, A]
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expectDecodeIs(value, decoded, codec.schema)

  def expectDecodeIs[A](
    value: Either[AvroException, AvroValue],
    decoded: Either[AvroException, A],
    schema: Either[AvroException, Schema]
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    whenSuccess((value, schema).tupled) { case (v, s) =>
      expect.same(decoded, codec.decoder(s).flatMap(_.decode(v)))
    }

  def expectSchemaError[A](
    expectedErrorMessage: String
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.same(Left(expectedErrorMessage), codec.schema.left.map(_.message))

  def expectDecodeError[A](
    value: Either[AvroException, AvroValue],
    schema: Either[AvroException, Schema],
    expectedErrorMessage: String
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    whenSuccess((value, schema).tupled) { case (v, s) =>
      expect.same(Left(expectedErrorMessage), codec.decoder(s).flatMap(_.decode(v)).left.map(_.message))
    }

  def expectEncodeError[A](
    a: A,
    expectedErrorMessage: String
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.same(Left(expectedErrorMessage), codec.encode(a).left.map(_.message))

  /** Checks generated values roundtrip, showing failing values using toString. */
  def roundtrip[A](implicit
    codec: Codec[A],
    arbitrary: Arbitrary[A],
    eq: Eq[A],
    loc: SourceLocation
  ): IO[Expectations] = {
    implicit val show: Show[A] = Show.fromToString

    forall { (a: A) =>
      roundtrip(a)
    }
  }

  /** Checks the value roundtrips as an Avro value, as Avro binary, and as Avro JSON. */
  def roundtrip[A](a: A)(implicit
    codec: Codec[A],
    eq: Eq[A],
    loc: SourceLocation
  ): Expectations =
    valueRoundtrip(a) && binaryRoundtrip(a) && jsonRoundtrip(a)

  private def valueRoundtrip[A](a: A)(implicit
    codec: Codec[A],
    eq: Eq[A],
    loc: SourceLocation
  ): Expectations = {
    val decoded =
      for {
        schema <- codec.schema
        encoded <- codec.encoder.flatMap(_.encode(a))
        decoded <- codec.decoder(schema).flatMap(_.decode(encoded))
      } yield decoded

    expect.eql(Right(a), decoded.left.map(_.message))
  }

  private def binaryRoundtrip[A](a: A)(implicit
    codec: Codec[A],
    eq: Eq[A],
    loc: SourceLocation
  ): Expectations = {
    val decoded =
      for {
        binary <- codec.binaryEncoder.flatMap(_.encode(a))
        decoded <- codec.schema.flatMap(codec.binaryDecoder).flatMap(_.decode(binary))
      } yield decoded

    expect.eql(Right(a), decoded.left.map(_.message))
  }

  private def jsonRoundtrip[A](a: A)(implicit
    codec: Codec[A],
    eq: Eq[A],
    loc: SourceLocation
  ): Expectations = {
    val decoded =
      for {
        json <- codec.jsonEncoder.flatMap(_.encode(a))
        decoded <- codec.schema.flatMap(codec.jsonDecoder).flatMap(_.decode(json))
      } yield decoded

    expect.eql(Right(a), decoded.left.map(_.message))
  }
}
