/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import vulcan.BaseSuite
import vulcan.generic.examples.*

object GenericDerivationRoundtripSpec extends BaseSuite {
  test("derive CaseClassAvroNamespace") { forallRoundtrip[CaseClassAvroNamespace] }
  test("derive CaseClassField") { forallRoundtrip[CaseClassField] }
  test("derive SealedTraitCaseClassAvroNamespace") { forallRoundtrip[SealedTraitCaseClassAvroNamespace] }
  test("derive SealedTraitCaseClassCustom") { forallRoundtrip[SealedTraitCaseClassCustom] }
  test("derive CaseClassAvroNullDefault") { forallRoundtrip[CaseClassAvroNullDefault] }
  test("derive CaseClassFieldAvroNullDefault") { forallRoundtrip[CaseClassFieldAvroNullDefault] }
  test("derive CaseClassAndFieldAvroNullDefault") { forallRoundtrip[CaseClassAndFieldAvroNullDefault] }
}
