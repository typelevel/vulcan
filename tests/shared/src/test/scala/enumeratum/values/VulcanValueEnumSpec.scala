/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package enumeratum.values

import vulcan.BaseSuite
import vulcan.Codec

object VulcanValueEnumSpec extends BaseSuite {
  pureTest("ByteVulcanEnum schema should be same as for underlying type") {
    expect.same(Codec[Byte].schema.map(_.toString), Codec[CustomByteEnum].schema.map(_.toString))
  }

  pureTest("ByteVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomByteEnum](CustomByteEnum.First, CustomByteEnum.Second))(expectRoundtrip(_))
  }

  pureTest("ByteVulcanEnum should error if withValueOpt does not handle schema value") {
    expectDecodeError[CustomByteEnum](
      Codec[CustomByteEnum].encoder.flatMap(_.encode(CustomByteEnum.Third)),
      "3 is not a member of CustomByteEnum (1, 2, 3)"
    )
  }

  pureTest("CharVulcanEnum schema should be same as for underlying type") {
    expect.same(Codec[Char].schema.map(_.toString), Codec[CustomCharEnum].schema.map(_.toString))
  }

  pureTest("CharVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomCharEnum](CustomCharEnum.First, CustomCharEnum.Second))(expectRoundtrip(_))
  }

  pureTest("CharVulcanEnum should error if withValueOpt does not handle schema value") {
    expectDecodeError[CustomCharEnum](
      Codec[CustomCharEnum].encoder.flatMap(_.encode(CustomCharEnum.Third)),
      "3 is not a member of CustomCharEnum (1, 2, 3)"
    )
  }

  pureTest("IntVulcanEnum schema should be same as for underlying type") {
    expect.same(Codec[Int].schema.map(_.toString), Codec[CustomIntEnum].schema.map(_.toString))
  }

  pureTest("IntVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomIntEnum](CustomIntEnum.First, CustomIntEnum.Second))(expectRoundtrip(_))
  }

  pureTest("IntVulcanEnum should error if withValueOpt does not handle schema value") {
    expectDecodeError[CustomIntEnum](
      Codec[CustomIntEnum].encoder.flatMap(_.encode(CustomIntEnum.Third)),
      "3 is not a member of CustomIntEnum (1, 2, 3)"
    )
  }

  pureTest("LongVulcanEnum schema should be same as for underlying type") {
    expect.same(Codec[Long].schema.map(_.toString), Codec[CustomLongEnum].schema.map(_.toString))
  }

  pureTest("LongVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomLongEnum](CustomLongEnum.First, CustomLongEnum.Second))(expectRoundtrip(_))
  }

  pureTest("LongVulcanEnum should error if withValueOpt does not handle schema value") {
    expectDecodeError[CustomLongEnum](
      Codec[CustomLongEnum].encoder.flatMap(_.encode(CustomLongEnum.Third)),
      "3 is not a member of CustomLongEnum (1, 2, 3)"
    )
  }

  pureTest("ShortVulcanEnum schema should be same as for underlying type") {
    expect.same(Codec[Short].schema.map(_.toString), Codec[CustomShortEnum].schema.map(_.toString))
  }

  pureTest("ShortVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomShortEnum](CustomShortEnum.First, CustomShortEnum.Second))(expectRoundtrip(_))
  }

  pureTest("ShortVulcanEnum should error if withValueOpt does not handle schema value") {
    expectDecodeError[CustomShortEnum](
      Codec[CustomShortEnum].encoder.flatMap(_.encode(CustomShortEnum.Third)),
      "3 is not a member of CustomShortEnum (1, 2, 3)"
    )
  }

  pureTest("StringVulcanEnum schema should be enum") {
    expectSchema[CustomStringEnum] {
      """{"type":"enum","name":"CustomStringEnum","namespace":"com.example","doc":"Custom enumeration","symbols":["first","second","third"]}"""
    }
  }

  pureTest("StringVulcanEnum should roundtrip enumeration values") {
    forEach(List[CustomStringEnum](CustomStringEnum.First, CustomStringEnum.Second))(expectRoundtrip(_))
  }

  pureTest("StringVulcanEnum should error if withValueOpt does not handle schema value") {
    expectDecodeError[CustomStringEnum](
      Codec[CustomStringEnum].encoder.flatMap(_.encode(CustomStringEnum.Third)),
      "Error decoding com.example.CustomStringEnum: third is not a member of CustomStringEnum (first, second, third)"
    )
  }
}
