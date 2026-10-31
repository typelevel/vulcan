/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import vulcan.BaseSuite
import vulcan.generic.examples.*

object DerivationSpec extends BaseSuite {
  pureTest("Codec deriveEnum schema should derive name, namespace, doc") {
    expectSchema[CaseClassEnumAvroDoc] {
      """{"type":"enum","name":"CaseClassEnumAvroDoc","namespace":"vulcan.generic.examples","doc":"documentation","symbols":["first"]}"""
    }
  }

  pureTest("Codec deriveEnum schema should use namespace annotation") {
    expectSchema[SealedTraitEnumDerived] {
      """{"type":"enum","name":"SealedTraitEnumDerived","namespace":"com.example","symbols":["first","second"]}"""
    }
  }

  pureTest("Codec deriveEnum schema should use enclosing objects as namespace") {
    expectSchema[AvroRecordDefault.Enum] {
      """{"type":"enum","name":"Enum","namespace":"vulcan.generic.examples.AvroRecordDefault","symbols":["A","B"]}"""
    }
  }

  test("Codec deriveEnum should roundtrip") { forallRoundtrip[SealedTraitEnumDerived] }

  pureTest("Codec deriveFixed schema should derive name, namespace, doc") {
    expectSchema[FixedAvroDoc] {
      """{"type":"fixed","name":"FixedAvroDoc","namespace":"vulcan.generic.examples","doc":"Some documentation","size":1}"""
    }
  }

  pureTest("Codec deriveFixed schema should derive overridden name") {
    expectSchema[FixedAvroName] {
      """{"type":"fixed","name":"FixedOtherName","namespace":"vulcan.generic.examples","size":1}"""
    }
  }

  pureTest("Codec deriveFixed schema should use alias annotations") {
    expectSchema[FixedAvroAlias] {
      """{"type":"fixed","name":"FixedAvroAlias","namespace":"vulcan.generic.examples","size":1,"aliases":["FixedOtherAlias","FixedSecondAlias"]}"""
    }
  }

  pureTest("Codec deriveFixed schema should use namespace annotation") {
    expectSchema[FixedNamespace] {
      """{"type":"fixed","name":"FixedNamespace","namespace":"vulcan.generic.examples.overridden","doc":"Some documentation","size":1}"""
    }
  }

  test("Codec deriveFixed should roundtrip") { forallRoundtrip[FixedNamespace] }
}
