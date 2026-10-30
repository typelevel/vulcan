/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.benchmarks

import vulcan.Codec
import vulcan.generic.*

/**
  * Codecs for orders derived using the generic module, instead of the hand-written codecs.
  *
  * The benchmarks for the current version and for version 1.x have copies of this
  * file, so they compare the same derived codecs.
  */
object DerivedOrder {
  private implicit val price: Codec[BigDecimal] =
    Codec.decimal(precision = 10, scale = 2)

  private implicit val customer: Codec[Customer] =
    Codec.derive

  private implicit val item: Codec[Item] =
    Codec.derive

  private implicit val card: Codec[Payment.Card] =
    Codec.derive

  private implicit val invoice: Codec[Payment.Invoice] =
    Codec.derive

  private implicit val payment: Codec[Payment] =
    Codec.derive

  val codec: Codec[Order] =
    Codec.derive
}
