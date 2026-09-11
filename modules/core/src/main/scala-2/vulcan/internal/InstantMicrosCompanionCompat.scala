/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import vulcan.Codec

import java.time.Instant
import scala.runtime.AbstractFunction1

/** Preserves `AbstractFunction1` from Scala 2 synthesized case class companion. */
private[vulcan] abstract class InstantMicrosCompanionCompat
    extends AbstractFunction1[Instant, Codec.InstantMicros]
