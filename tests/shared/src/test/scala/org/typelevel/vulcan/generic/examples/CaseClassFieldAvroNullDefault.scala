/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import java.time.LocalDate
import org.scalacheck.Arbitrary
import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

final case class CaseClassFieldAvroNullDefault(
  @AvroNullDefault(true) int: Option[Int],
  long: Option[Long],
  @AvroNullDefault(true) string: Option[String],
  date: Option[LocalDate],
  @AvroNullDefault(true) map: Option[Map[String, String]],
  caseClassValueClass: Option[CaseClassValueClass],
  @AvroNullDefault(true) sealedTraitEnumDerived: Option[SealedTraitEnumDerived]
)

object CaseClassFieldAvroNullDefault {
  implicit val caseClassFieldAvroNullDefaultArbitrary: Arbitrary[CaseClassFieldAvroNullDefault] =
    Arbitrary(CaseClassNullableFields.genFieldProduct.map((apply _).tupled))

  implicit val codec: Codec[CaseClassFieldAvroNullDefault] =
    Codec.derive
}
