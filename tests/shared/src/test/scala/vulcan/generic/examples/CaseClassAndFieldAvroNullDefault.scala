/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import java.time.LocalDate
import org.scalacheck.Arbitrary
import vulcan.Codec
import vulcan.generic.*

@AvroNullDefault(true)
final case class CaseClassAndFieldAvroNullDefault(
  @AvroNullDefault(false) int: Option[Int],
  long: Option[Long],
  @AvroNullDefault(false) string: Option[String],
  date: Option[LocalDate],
  @AvroNullDefault(false) map: Option[Map[String, String]],
  caseClassValueClass: Option[CaseClassValueClass],
  @AvroNullDefault(false) sealedTraitEnumDerived: Option[SealedTraitEnumDerived]
)

object CaseClassAndFieldAvroNullDefault {
  implicit val caseClassAndFieldAvroNullDefaultArbitrary: Arbitrary[CaseClassAndFieldAvroNullDefault] =
    Arbitrary(CaseClassNullableFields.genFieldProduct.map((apply _).tupled))

  implicit val codec: Codec[CaseClassAndFieldAvroNullDefault] =
    Codec.derive
}
