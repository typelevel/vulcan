/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package enumeratum.values

import enumeratum.EitherValues
import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import vulcan.AvroException
import vulcan.Codec

final class VulcanValueEnumSpec extends AnyFunSpec with ScalaCheckPropertyChecks with EitherValues {

  /** Encodes the value, and decodes it using the schema of the codec as the writer schema. */
  private def encodeAndDecode[A](a: A)(implicit codec: Codec[A]): Either[AvroException, A] =
    for {
      encoder <- codec.encoder
      value <- encoder.encode(a)
      schema <- codec.schema
      decoder <- codec.decoder(schema)
      decoded <- decoder.decode(value)
    } yield decoded

  describe("ByteVulcanEnum") {

    it("schema should be same as for underlying type") {
      assert {
        Codec[CustomByteEnum].schema.value.toString ===
          Codec[Byte].schema.value.toString
      }
    }

    it("should roundtrip enumeration values") {
      val gen = Gen.oneOf[CustomByteEnum](CustomByteEnum.First, CustomByteEnum.Second)
      forAll(gen) { customEnum =>
        val roundtrip = encodeAndDecode(customEnum)
        assert(roundtrip.value === customEnum)
      }
    }

    it("should error if withValueOpt does not handle schema value") {
      val roundtrip = encodeAndDecode[CustomByteEnum](CustomByteEnum.Third)
      assert {
        roundtrip.swap.value.message ===
          """3 is not a member of CustomByteEnum (1, 2, 3)"""
      }
    }
  }

  describe("CharVulcanEnum") {

    it("schema should be same as for underlying type") {
      assert {
        Codec[CustomCharEnum].schema.value.toString ===
          Codec[Char].schema.value.toString
      }
    }

    it("should roundtrip enumeration values") {
      val gen = Gen.oneOf[CustomCharEnum](CustomCharEnum.First, CustomCharEnum.Second)
      forAll(gen) { customEnum =>
        val roundtrip = encodeAndDecode(customEnum)
        assert(roundtrip.value === customEnum)
      }
    }

    it("should error if withValueOpt does not handle schema value") {
      val roundtrip = encodeAndDecode[CustomCharEnum](CustomCharEnum.Third)
      assert {
        roundtrip.swap.value.message ===
          """3 is not a member of CustomCharEnum (1, 2, 3)"""
      }
    }
  }

  describe("IntVulcanEnum") {

    it("schema should be same as for underlying type") {
      assert {
        Codec[CustomIntEnum].schema.value.toString ===
          Codec[Int].schema.value.toString
      }
    }

    it("should roundtrip enumeration values") {
      val gen = Gen.oneOf[CustomIntEnum](CustomIntEnum.First, CustomIntEnum.Second)
      forAll(gen) { customEnum =>
        val roundtrip = encodeAndDecode(customEnum)
        assert(roundtrip.value === customEnum)
      }
    }

    it("should error if withValueOpt does not handle schema value") {
      val roundtrip = encodeAndDecode[CustomIntEnum](CustomIntEnum.Third)
      assert {
        roundtrip.swap.value.message ===
          """3 is not a member of CustomIntEnum (1, 2, 3)"""
      }
    }
  }

  describe("LongVulcanEnum") {

    it("schema should be same as for underlying type") {
      assert {
        Codec[CustomLongEnum].schema.value.toString ===
          Codec[Long].schema.value.toString
      }
    }

    it("should roundtrip enumeration values") {
      val gen = Gen.oneOf[CustomLongEnum](CustomLongEnum.First, CustomLongEnum.Second)
      forAll(gen) { customEnum =>
        val roundtrip = encodeAndDecode(customEnum)
        assert(roundtrip.value === customEnum)
      }
    }

    it("should error if withValueOpt does not handle schema value") {
      val roundtrip = encodeAndDecode[CustomLongEnum](CustomLongEnum.Third)
      assert {
        roundtrip.swap.value.message ===
          """3 is not a member of CustomLongEnum (1, 2, 3)"""
      }
    }
  }

  describe("ShortVulcanEnum") {

    it("schema should be same as for underlying type") {
      assert {
        Codec[CustomShortEnum].schema.value.toString ===
          Codec[Short].schema.value.toString
      }
    }

    it("should roundtrip enumeration values") {
      val gen = Gen.oneOf[CustomShortEnum](CustomShortEnum.First, CustomShortEnum.Second)
      forAll(gen) { customEnum =>
        val roundtrip = encodeAndDecode(customEnum)
        assert(roundtrip.value === customEnum)
      }
    }

    it("should error if withValueOpt does not handle schema value") {
      val roundtrip = encodeAndDecode[CustomShortEnum](CustomShortEnum.Third)
      assert {
        roundtrip.swap.value.message ===
          """3 is not a member of CustomShortEnum (1, 2, 3)"""
      }
    }
  }

  describe("StringVulcanEnum") {

    it("schema should be enum") {
      assert {
        Codec[CustomStringEnum].schema.value.toString ===
          """{"type":"enum","name":"CustomStringEnum","namespace":"com.example","doc":"Custom enumeration","symbols":["first","second","third"]}"""
      }
    }

    it("should roundtrip enumeration values") {
      val gen = Gen.oneOf[CustomStringEnum](CustomStringEnum.First, CustomStringEnum.Second)
      forAll(gen) { customEnum =>
        val roundtrip = encodeAndDecode(customEnum)
        assert(roundtrip.value === customEnum)
      }
    }

    it("should error if withValueOpt does not handle schema value") {
      val roundtrip = encodeAndDecode[CustomStringEnum](CustomStringEnum.Third)
      assert {
        roundtrip.swap.value.message ===
          """Error decoding com.example.CustomStringEnum: third is not a member of CustomStringEnum (first, second, third)"""
      }
    }
  }
}
