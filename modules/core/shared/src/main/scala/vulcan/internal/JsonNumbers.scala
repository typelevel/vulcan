/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import io.circe.Json
import io.circe.JsonNumber

private[vulcan] object JsonNumbers {
  def float(value: Float): Json =
    if (isNegativeZero(value.toDouble)) NegativeZero else Json.fromFloatOrString(value)

  def double(value: Double): Json =
    if (isNegativeZero(value)) NegativeZero else Json.fromDoubleOrString(value)

  /** Negative zero with its sign, which `toString` drops on Scala.js. */
  private val NegativeZero: Json =
    Json.fromJsonNumber(JsonNumber.fromDecimalStringUnsafe("-0.0"))

  private def isNegativeZero(value: Double): Boolean =
    value == 0d && 1d / value < 0d
}
