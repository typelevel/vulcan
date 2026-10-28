/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import io.circe.Json
import io.circe.JsonNumber

/** JSON numbers for floats and doubles, formatted the same as the Java Avro library on every platform. */
private[vulcan] object JsonNumbers {

  /** The float as a JSON number, or as a string if it is not finite. */
  def float(value: Float): Json =
    if (java.lang.Float.isFinite(value)) decimal(NumberFormat.float(value))
    else Json.fromString(value.toString)

  /** The double as a JSON number, or as a string if it is not finite. */
  def double(value: Double): Json =
    if (java.lang.Double.isFinite(value)) decimal(NumberFormat.double(value))
    else Json.fromString(value.toString)

  /** Decimal numbers are printed verbatim, while circe prints doubles using the platform. */
  private def decimal(value: String): Json =
    Json.fromJsonNumber(JsonNumber.fromDecimalStringUnsafe(value))
}
