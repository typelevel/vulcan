/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.examples

import io.circe.Json
import io.circe.JsonObject
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import vulcan.AvroException
import vulcan.Codec

sealed trait SealedTraitEnumNoDefault

object SealedTraitEnumNoDefault {
  implicit final val arbitrary: Arbitrary[SealedTraitEnumNoDefault] =
    Arbitrary(Gen.oneOf(FirstInSealedTraitEnumNoDefault, SecondInSealedTraitEnumNoDefault))

  implicit final val codec: Codec[SealedTraitEnumNoDefault] =
    Codec.enumeration(
      name = "SealedTraitEnumNoDefault",
      namespace = "vulcan.examples",
      symbols = List("first", "second"),
      aliases = List("first", "second"),
      doc = Some("documentation"),
      default = None,
      encode = {
        case FirstInSealedTraitEnumNoDefault => "first"
        case SecondInSealedTraitEnumNoDefault => "second"
      },
      decode = {
        case "first" => Right(FirstInSealedTraitEnumNoDefault)
        case "second" => Right(SecondInSealedTraitEnumNoDefault)
        case other => Left(AvroException(other))
      },
      props = JsonObject("custom1" -> Json.fromInt(10), "custom2" -> Json.fromString("value2"))
    )
}

case object FirstInSealedTraitEnumNoDefault extends SealedTraitEnumNoDefault

case object SecondInSealedTraitEnumNoDefault extends SealedTraitEnumNoDefault
