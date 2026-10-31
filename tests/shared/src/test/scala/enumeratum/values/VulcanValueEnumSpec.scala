/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package enumeratum.values

import vulcan.AvroException
import vulcan.BaseSuite
import vulcan.Codec

object VulcanValueEnumSpec extends BaseSuite {

  /** Encodes the value, and decodes it using the schema of the codec as the writer schema. */
  private def encodeAndDecode[A](a: A)(implicit codec: Codec[A]): Either[AvroException, A] =
    for {
      encoder <- codec.encoder
      value <- encoder.encode(a)
      schema <- codec.schema
      decoder <- codec.decoder(schema)
      decoded <- decoder.decode(value)
    } yield decoded

  pureTest("ByteVulcanEnum schema should be same as for underlying type") {
    expect.same(Codec[Byte].schema.map(_.toString), Codec[CustomByteEnum].schema.map(_.toString))
  }

  pureTest("ByteVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomByteEnum](CustomByteEnum.First, CustomByteEnum.Second)) { customEnum =>
      expect.same(Right(customEnum), encodeAndDecode(customEnum))
    }
  }

  pureTest("ByteVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.same(
      Left("3 is not a member of CustomByteEnum (1, 2, 3)"),
      encodeAndDecode[CustomByteEnum](CustomByteEnum.Third).left.map(_.message)
    )
  }

  pureTest("CharVulcanEnum schema should be same as for underlying type") {
    expect.same(Codec[Char].schema.map(_.toString), Codec[CustomCharEnum].schema.map(_.toString))
  }

  pureTest("CharVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomCharEnum](CustomCharEnum.First, CustomCharEnum.Second)) { customEnum =>
      expect.same(Right(customEnum), encodeAndDecode(customEnum))
    }
  }

  pureTest("CharVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.same(
      Left("3 is not a member of CustomCharEnum (1, 2, 3)"),
      encodeAndDecode[CustomCharEnum](CustomCharEnum.Third).left.map(_.message)
    )
  }

  pureTest("IntVulcanEnum schema should be same as for underlying type") {
    expect.same(Codec[Int].schema.map(_.toString), Codec[CustomIntEnum].schema.map(_.toString))
  }

  pureTest("IntVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomIntEnum](CustomIntEnum.First, CustomIntEnum.Second)) { customEnum =>
      expect.same(Right(customEnum), encodeAndDecode(customEnum))
    }
  }

  pureTest("IntVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.same(
      Left("3 is not a member of CustomIntEnum (1, 2, 3)"),
      encodeAndDecode[CustomIntEnum](CustomIntEnum.Third).left.map(_.message)
    )
  }

  pureTest("LongVulcanEnum schema should be same as for underlying type") {
    expect.same(Codec[Long].schema.map(_.toString), Codec[CustomLongEnum].schema.map(_.toString))
  }

  pureTest("LongVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomLongEnum](CustomLongEnum.First, CustomLongEnum.Second)) { customEnum =>
      expect.same(Right(customEnum), encodeAndDecode(customEnum))
    }
  }

  pureTest("LongVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.same(
      Left("3 is not a member of CustomLongEnum (1, 2, 3)"),
      encodeAndDecode[CustomLongEnum](CustomLongEnum.Third).left.map(_.message)
    )
  }

  pureTest("ShortVulcanEnum schema should be same as for underlying type") {
    expect.same(Codec[Short].schema.map(_.toString), Codec[CustomShortEnum].schema.map(_.toString))
  }

  pureTest("ShortVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomShortEnum](CustomShortEnum.First, CustomShortEnum.Second)) { customEnum =>
      expect.same(Right(customEnum), encodeAndDecode(customEnum))
    }
  }

  pureTest("ShortVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.same(
      Left("3 is not a member of CustomShortEnum (1, 2, 3)"),
      encodeAndDecode[CustomShortEnum](CustomShortEnum.Third).left.map(_.message)
    )
  }

  pureTest("StringVulcanEnum schema should be enum") {
    expect.same(
      Right(
        """{"type":"enum","name":"CustomStringEnum","namespace":"com.example","doc":"Custom enumeration","symbols":["first","second","third"]}"""
      ),
      Codec[CustomStringEnum].schema.map(_.toString)
    )
  }

  pureTest("StringVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomStringEnum](CustomStringEnum.First, CustomStringEnum.Second)) { customEnum =>
      expect.same(Right(customEnum), encodeAndDecode(customEnum))
    }
  }

  pureTest("StringVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.same(
      Left(
        "Error decoding com.example.CustomStringEnum: third is not a member of CustomStringEnum (first, second, third)"
      ),
      encodeAndDecode[CustomStringEnum](CustomStringEnum.Third).left.map(_.message)
    )
  }
}
