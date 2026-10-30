/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Eq
import cats.Show
import cats.effect.IO
import org.scalacheck.Arbitrary
import weaver.Expectations
import weaver.SourceLocation

trait RoundtripHelpers {
  self: BaseSuite =>

  /** Checks generated values roundtrip, showing failing values using toString. */
  def roundtrip[A](implicit
    codec: Codec[A],
    arbitrary: Arbitrary[A],
    eq: Eq[A],
    loc: SourceLocation
  ): IO[Expectations] = {
    implicit val show: Show[A] = Show.fromToString

    forall { (a: A) =>
      roundtrip(a) && binaryRoundtrip(a) && jsonRoundtrip(a)
    }
  }

  def roundtrip[A](a: A)(implicit
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

  def binaryRoundtrip[A](a: A)(implicit
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

  def jsonRoundtrip[A](a: A)(implicit
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
