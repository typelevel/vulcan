/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.examples

import vulcan.v2.Codec

sealed trait SealedTraitCaseClassSingle

object SealedTraitCaseClassSingle {
  implicit val codec: Codec[SealedTraitCaseClassSingle] =
    Codec.union { alt =>
      alt[CaseClassInSealedTraitCaseClassSingle]
    }
}

final case class CaseClassInSealedTraitCaseClassSingle(value: Int)
    extends SealedTraitCaseClassSingle

object CaseClassInSealedTraitCaseClassSingle {
  implicit val codec: Codec[CaseClassInSealedTraitCaseClassSingle] =
    Codec.record(
      name = "CaseClassInSealedTraitCaseClassSingle",
      namespace = "vulcan.examples"
    ) { field =>
      field("value", _.value).map(apply)
    }
}
