/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package enumeratum

import enumeratum.EnumEntry.Lowercase
import vulcan.BaseSuite
import vulcan.Codec
import vulcan.generic.AvroDoc
import vulcan.generic.AvroNamespace

object VulcanEnumSpec extends BaseSuite {
  pureTest("VulcanEnum schema should use an enum schema") {
    expectSchema[Suit] {
      """{"type":"enum","name":"Suit","namespace":"com.example","doc":"The different card suits","symbols":["clubs","diamonds","hearts","spades"]}"""
    }
  }

  pureTest("VulcanEnum should provide the codec as vulcanCodec") {
    expect.same(Codec[Suit].schema, Suit.vulcanCodec.schema)
  }

  pureTest("VulcanEnum should roundtrip enum values") {
    forEach(List[Suit](Suit.Diamonds, Suit.Hearts, Suit.Spades))(expectRoundtrip(_))
  }

  pureTest("VulcanEnum should error if withNameOption does not handle schema symbol") {
    expectDecodeError[Suit](
      Codec[Suit].encoder.flatMap(_.encode(Suit.Clubs)),
      "Error decoding com.example.Suit: clubs is not a member of Suit (clubs, diamonds, hearts, spades)"
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
