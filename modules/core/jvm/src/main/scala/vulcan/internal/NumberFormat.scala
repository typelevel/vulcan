/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

/** Formats finite numbers the same as the Java Avro library, which uses `toString`. */
private[vulcan] object NumberFormat {
  def double(value: Double): String =
    java.lang.Double.toString(value)

  def float(value: Float): String =
    java.lang.Float.toString(value)
}
