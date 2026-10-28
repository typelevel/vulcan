/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import cats.Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import vulcan.generic.*
import vulcan.v2.Codec

@AvroNamespace("com.example")
final case class CaseClassAvroNamespace(value: Option[String])

object CaseClassAvroNamespace {
  implicit val caseClassAvroNamespaceArbitrary: Arbitrary[CaseClassAvroNamespace] =
    Arbitrary(arbitrary[Option[String]].map(apply))

  implicit val caseClassAvroNamespaceEq: Eq[CaseClassAvroNamespace] =
    Eq.fromUniversalEquals

  implicit val codec: Codec[CaseClassAvroNamespace] =
    Codec.derive
}
