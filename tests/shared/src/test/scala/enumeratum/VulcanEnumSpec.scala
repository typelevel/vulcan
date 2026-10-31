/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package enumeratum

import enumeratum.EnumEntry.Lowercase
import vulcan.AvroException
import vulcan.BaseSuite
import vulcan.Codec
import vulcan.generic.AvroDoc
import vulcan.generic.AvroNamespace

object VulcanEnumSpec extends BaseSuite {

  /** Encodes the value, and decodes it using the schema of the codec as the writer schema. */
  private def encodeAndDecode[A](a: A)(implicit codec: Codec[A]): Either[AvroException, A] =
    for {
      encoder <- codec.encoder
      value <- encoder.encode(a)
      schema <- codec.schema
      decoder <- codec.decoder(schema)
      decoded <- decoder.decode(value)
    } yield decoded

  pureTest("VulcanEnum schema should use an enum schema") {
    expect.same(
      Right(
        """{"type":"enum","name":"Suit","namespace":"com.example","doc":"The different card suits","symbols":["clubs","diamonds","hearts","spades"]}"""
      ),
      Codec[Suit].schema.map(_.toString)
    )
  }

  pureTest("VulcanEnum should provide the codec as vulcanCodec") {
    expect.same(Codec[Suit].schema, Suit.vulcanCodec.schema)
  }

  pureTest("VulcanEnum should roundtrip enum values") {
    forEach(List[Suit](Suit.Diamonds, Suit.Hearts, Suit.Spades)) { suit =>
      expect.same(Right(suit), encodeAndDecode(suit))
    }
  }

  pureTest("VulcanEnum should error if withNameOption does not handle schema symbol") {
    expect.same(
      Left(
        "Error decoding com.example.Suit: clubs is not a member of Suit (clubs, diamonds, hearts, spades)"
      ),
      encodeAndDecode[Suit](Suit.Clubs).left.map(_.message)
    )
  }

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
