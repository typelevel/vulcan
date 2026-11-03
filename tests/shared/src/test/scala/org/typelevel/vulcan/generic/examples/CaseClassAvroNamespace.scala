/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.generic.examples

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.generic.*

@AvroNamespace("com.example")
final case class CaseClassAvroNamespace(value: Option[String])

object CaseClassAvroNamespace {
  implicit val caseClassAvroNamespaceArbitrary: Arbitrary[CaseClassAvroNamespace] =
    Arbitrary(arbitrary[Option[String]].map(apply))

  implicit val codec: Codec[CaseClassAvroNamespace] =
    Codec.derive
}
