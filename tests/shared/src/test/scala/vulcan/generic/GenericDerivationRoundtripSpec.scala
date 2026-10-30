/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import vulcan.BaseSuite
import vulcan.RoundtripHelpers
import vulcan.generic.examples.*

object GenericDerivationRoundtripSpec extends BaseSuite with RoundtripHelpers {
  test("derive CaseClassAvroNamespace") { roundtrip[CaseClassAvroNamespace] }
  test("derive CaseClassField") { roundtrip[CaseClassField] }
  test("derive SealedTraitCaseClassAvroNamespace") { roundtrip[SealedTraitCaseClassAvroNamespace] }
  test("derive SealedTraitCaseClassCustom") { roundtrip[SealedTraitCaseClassCustom] }
  test("derive CaseClassAvroNullDefault") { roundtrip[CaseClassAvroNullDefault] }
  test("derive CaseClassFieldAvroNullDefault") { roundtrip[CaseClassFieldAvroNullDefault] }
  test("derive CaseClassAndFieldAvroNullDefault") { roundtrip[CaseClassAndFieldAvroNullDefault] }
}
