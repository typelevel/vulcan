/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package enumeratum

import enumeratum.EnumEntry.Lowercase
import enumeratum.VulcanEnumSpec.Suit
import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import vulcan.AvroException
import vulcan.Codec
import vulcan.generic.AvroDoc
import vulcan.generic.AvroNamespace

final class VulcanEnumSpec extends AnyFunSpec with ScalaCheckPropertyChecks with EitherValues {

  /** Encodes the value, and decodes it using the schema of the codec as the writer schema. */
  private def encodeAndDecode[A](a: A)(implicit codec: Codec[A]): Either[AvroException, A] =
    for {
      encoder <- codec.encoder
      value <- encoder.encode(a)
      schema <- codec.schema
      decoder <- codec.decoder(schema)
      decoded <- decoder.decode(value)
    } yield decoded

  describe("VulcanEnum") {
    describe("schema") {
      it("should use an enum schema") {
        assert {
          Codec[Suit].schema.value.toString ===
            """{"type":"enum","name":"Suit","namespace":"com.example","doc":"The different card suits","symbols":["clubs","diamonds","hearts","spades"]}"""
        }
      }
    }

    it("should provide the codec as vulcanCodec") {
      assert(Suit.vulcanCodec.schema === Codec[Suit].schema)
    }

    it("should roundtrip enum values") {
      val gen = Gen.oneOf[Suit](Suit.Diamonds, Suit.Hearts, Suit.Spades)
      forAll(gen) { suit =>
        val roundtrip = encodeAndDecode(suit)
        assert(roundtrip.value === suit)
      }
    }

    it("should error if withNameOption does not handle schema symbol") {
      val roundtrip = encodeAndDecode[Suit](Suit.Clubs)
      assert {
        roundtrip.swap.value.message ===
          "Error decoding com.example.Suit: clubs is not a member of Suit (clubs, diamonds, hearts, spades)"
      }
    }
  }
}

object VulcanEnumSpec {

  @AvroNamespace("com.example")
  @AvroDoc("The different card suits")
  sealed trait Suit extends EnumEntry with Lowercase

  object Suit extends Enum[Suit] with VulcanEnum[Suit] {
    case object Clubs extends Suit
    case object Diamonds extends Suit
    case object Hearts extends Suit
    case object Spades extends Suit

    val values = findValues

    override def withNameOption(name: String): Option[Suit] =
      if (name == "clubs") None
      else super.withNameOption(name)
  }
}
