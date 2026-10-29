/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.examples

import cats.Eq
import io.circe.Json
import io.circe.JsonObject
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import scodec.bits.*
import vulcan.AvroException
import vulcan.Codec

sealed trait FixedBoolean extends Product with Serializable

object FixedBoolean {
  implicit val codec: Codec[FixedBoolean] =
    Codec
      .fixed(
        name = "FixedBoolean",
        namespace = "vulcan.examples",
        size = 1,
        encode = {
          case FalseFixedBoolean => hex"00"
          case TrueFixedBoolean => hex"01"
        },
        decode = bytes => {
          val byte = bytes.head
          if (byte == 0.toByte) Right(FalseFixedBoolean)
          else if (byte == 1.toByte) Right(TrueFixedBoolean)
          else Left(AvroException(s"unknown byte $byte"))
        },
        doc = Some("A boolean represented as a byte"),
        aliases = List("SomeOtherBoolean"),
        props = JsonObject("custom" -> Json.fromString("value"))
      )

  implicit val fixedBooleanEq: Eq[FixedBoolean] =
    Eq.fromUniversalEquals

  implicit val fixedBooleanArbitrary: Arbitrary[FixedBoolean] =
    Arbitrary(Gen.oneOf(FalseFixedBoolean, TrueFixedBoolean))
}

case object TrueFixedBoolean extends FixedBoolean

case object FalseFixedBoolean extends FixedBoolean
