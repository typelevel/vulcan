/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.examples

import org.typelevel.vulcan.Codec

sealed trait SealedTraitCaseClassSingle

object SealedTraitCaseClassSingle {
  implicit val codec: Codec[SealedTraitCaseClassSingle] =
    Codec.union { alt =>
      alt[CaseClassInSealedTraitCaseClassSingle]
    }
}

final case class CaseClassInSealedTraitCaseClassSingle(value: Int) extends SealedTraitCaseClassSingle

object CaseClassInSealedTraitCaseClassSingle {
  implicit val codec: Codec[CaseClassInSealedTraitCaseClassSingle] =
    Codec.record(
      name = "CaseClassInSealedTraitCaseClassSingle",
      namespace = "org.typelevel.vulcan.examples"
    ) { field =>
      field("value", _.value).map(apply)
    }
}
