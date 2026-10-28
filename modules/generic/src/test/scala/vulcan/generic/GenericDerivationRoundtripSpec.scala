/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import vulcan.BaseSpec
import vulcan.generic.examples.*
import vulcan.v2.RoundtripHelpers

final class GenericDerivationRoundtripSpec extends BaseSpec with RoundtripHelpers {

  describe("derive") {
    it("CaseClassAvroNamespace") { roundtrip[CaseClassAvroNamespace] }
    it("CaseClassField") { roundtrip[CaseClassField] }
    it("SealedTraitCaseClassAvroNamespace") { roundtrip[SealedTraitCaseClassAvroNamespace] }
    it("SealedTraitCaseClassCustom") { roundtrip[SealedTraitCaseClassCustom] }
    it("CaseClassAvroNullDefault") { roundtrip[CaseClassAvroNullDefault] }
    it("CaseClassFieldAvroNullDefault") { roundtrip[CaseClassFieldAvroNullDefault] }
    it("CaseClassAndFieldAvroNullDefault") { roundtrip[CaseClassAndFieldAvroNullDefault] }
  }
}
