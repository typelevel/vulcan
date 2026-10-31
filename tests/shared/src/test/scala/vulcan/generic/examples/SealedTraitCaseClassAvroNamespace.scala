/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import vulcan.Codec
import vulcan.generic.*

sealed trait SealedTraitCaseClassAvroNamespace

// out of order to verify that ordering in derived schema is by name
@AvroNamespace("com.example")
final case class SecondInSealedTraitCaseClassAvroNamespace(value: String)
  extends SealedTraitCaseClassAvroNamespace
final case class FirstInSealedTraitCaseClassAvroNamespace(value: Int)
  extends SealedTraitCaseClassAvroNamespace

object SealedTraitCaseClassAvroNamespace {
  implicit val sealedTraitCaseClassAvroNamespaceArbitrary: Arbitrary[SealedTraitCaseClassAvroNamespace] =
    Arbitrary(
      Gen.oneOf(
        arbitrary[Int].map(FirstInSealedTraitCaseClassAvroNamespace(_)),
        arbitrary[String].map(SecondInSealedTraitCaseClassAvroNamespace(_))
      )
    )

  implicit val codec: Codec[SealedTraitCaseClassAvroNamespace] =
    Codec.derive
}
