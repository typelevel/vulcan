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
    expect.eql(Codec[Byte].schema.value.toString, Codec[CustomByteEnum].schema.value.toString)
  }

  pureTest("ByteVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomByteEnum](CustomByteEnum.First, CustomByteEnum.Second)) { customEnum =>
      expect.same(customEnum, encodeAndDecode(customEnum).value)
    }
  }

  pureTest("ByteVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.eql(
      "3 is not a member of CustomByteEnum (1, 2, 3)",
      encodeAndDecode[CustomByteEnum](CustomByteEnum.Third).swap.value.message
    )
  }

  pureTest("CharVulcanEnum schema should be same as for underlying type") {
    expect.eql(Codec[Char].schema.value.toString, Codec[CustomCharEnum].schema.value.toString)
  }

  pureTest("CharVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomCharEnum](CustomCharEnum.First, CustomCharEnum.Second)) { customEnum =>
      expect.same(customEnum, encodeAndDecode(customEnum).value)
    }
  }

  pureTest("CharVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.eql(
      "3 is not a member of CustomCharEnum (1, 2, 3)",
      encodeAndDecode[CustomCharEnum](CustomCharEnum.Third).swap.value.message
    )
  }

  pureTest("IntVulcanEnum schema should be same as for underlying type") {
    expect.eql(Codec[Int].schema.value.toString, Codec[CustomIntEnum].schema.value.toString)
  }

  pureTest("IntVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomIntEnum](CustomIntEnum.First, CustomIntEnum.Second)) { customEnum =>
      expect.same(customEnum, encodeAndDecode(customEnum).value)
    }
  }

  pureTest("IntVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.eql(
      "3 is not a member of CustomIntEnum (1, 2, 3)",
      encodeAndDecode[CustomIntEnum](CustomIntEnum.Third).swap.value.message
    )
  }

  pureTest("LongVulcanEnum schema should be same as for underlying type") {
    expect.eql(Codec[Long].schema.value.toString, Codec[CustomLongEnum].schema.value.toString)
  }

  pureTest("LongVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomLongEnum](CustomLongEnum.First, CustomLongEnum.Second)) { customEnum =>
      expect.same(customEnum, encodeAndDecode(customEnum).value)
    }
  }

  pureTest("LongVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.eql(
      "3 is not a member of CustomLongEnum (1, 2, 3)",
      encodeAndDecode[CustomLongEnum](CustomLongEnum.Third).swap.value.message
    )
  }

  pureTest("ShortVulcanEnum schema should be same as for underlying type") {
    expect.eql(Codec[Short].schema.value.toString, Codec[CustomShortEnum].schema.value.toString)
  }

  pureTest("ShortVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomShortEnum](CustomShortEnum.First, CustomShortEnum.Second)) { customEnum =>
      expect.same(customEnum, encodeAndDecode(customEnum).value)
    }
  }

  pureTest("ShortVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.eql(
      "3 is not a member of CustomShortEnum (1, 2, 3)",
      encodeAndDecode[CustomShortEnum](CustomShortEnum.Third).swap.value.message
    )
  }

  pureTest("StringVulcanEnum schema should be enum") {
    expect.eql(
      """{"type":"enum","name":"CustomStringEnum","namespace":"com.example","doc":"Custom enumeration","symbols":["first","second","third"]}""",
      Codec[CustomStringEnum].schema.value.toString
    )
  }

  pureTest("StringVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomStringEnum](CustomStringEnum.First, CustomStringEnum.Second)) { customEnum =>
      expect.same(customEnum, encodeAndDecode(customEnum).value)
    }
  }

  pureTest("StringVulcanEnum should error if withValueOpt does not handle schema value") {
    expect.eql(
      "Error decoding com.example.CustomStringEnum: third is not a member of CustomStringEnum (first, second, third)",
      encodeAndDecode[CustomStringEnum](CustomStringEnum.Third).swap.value.message
    )
  }
}
