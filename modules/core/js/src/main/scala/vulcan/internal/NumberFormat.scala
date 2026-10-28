/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import java.math.BigDecimal
import java.math.MathContext
import java.math.RoundingMode
import scala.scalajs.js.JSNumberOps.*

/**
  * Formats finite numbers the same as the Java Avro library, which uses `toString` on the JVM.
  */
private[vulcan] object NumberFormat {
  def double(value: Double): String =
    if (value == 0d) zero(1d / value < 0d)
    else {
      val magnitude = Math.abs(value)
      val shortest = magnitude.toExponential()
      val selected =
        if (shortest.indexOf('.') >= 0) shortest
        else {
          // Two digits can be closer when one is enough, for example 4.9E-324 instead of 5E-324
          val twoDigits = magnitude.toExponential(1)
          if (java.lang.Double.parseDouble(twoDigits) == magnitude) twoDigits else shortest
        }

      val index = selected.indexOf('e')
      val digits = selected.substring(0, index).replace(".", "")
      sign(value) + format(stripTrailingZeros(digits), Integer.parseInt(selected.substring(index + 1)))
    }

  def float(value: Float): String =
    if (value == 0f) zero(1f / value < 0f)
    else {
      val decimal = select(Math.abs(value)).stripTrailingZeros
      val digits = decimal.unscaledValue.toString
      sign(value.toDouble) + format(digits, digits.length - 1 - decimal.scale)
    }

  /** The decimal which the JVM selects for the positive float. */
  private def select(value: Float): BigDecimal = {
    val bits = java.lang.Float.floatToIntBits(value)
    val exact = new BigDecimal(value.toDouble)

    // Decimals between the halfway points to adjacent floats round to the float
    val lower = exact.add(new BigDecimal(java.lang.Float.intBitsToFloat(bits - 1).toDouble)).multiply(Half)
    val above = java.lang.Float.intBitsToFloat(bits + 1)
    val upper =
      if (java.lang.Float.isInfinite(above)) exact.add(exact.subtract(lower))
      else exact.add(new BigDecimal(above.toDouble)).multiply(Half)

    // Decimals exactly halfway round to the float with an even significand
    val even = (bits & 1) == 0

    def rounds(decimal: BigDecimal): Boolean = {
      val fromLower = decimal.compareTo(lower)
      val fromUpper = decimal.compareTo(upper)
      (fromLower > 0 || (fromLower == 0 && even)) && (fromUpper < 0 || (fromUpper == 0 && even))
    }

    // The closest decimals below and above with the number of digits, if they round to the float
    def candidates(digits: Int): List[BigDecimal] =
      List(RoundingMode.FLOOR, RoundingMode.CEILING)
        .map(mode => exact.round(new MathContext(digits, mode)))
        .filter(rounds)

    def closest(first: BigDecimal, second: BigDecimal): BigDecimal = {
      val comparison = first.subtract(exact).abs.compareTo(second.subtract(exact).abs)
      if (comparison < 0) first
      else if (comparison > 0) second
      else if (first.stripTrailingZeros.unscaledValue.testBit(0)) second
      else first
    }

    var digits = 1
    var selected = candidates(digits)
    while (selected.isEmpty) {
      digits += 1
      selected = candidates(digits)
    }

    // Two digits can be closer when one is enough, for example 1.4E-45 instead of 1E-45
    if (digits == 1) selected = selected ++ candidates(2)
    selected.reduce(closest)
  }

  /** Formats the digits, without trailing zeros, where the exponent is for the first digit. */
  private def format(digits: String, exponent: Int): String =
    if (exponent >= -3 && exponent < 0)
      "0." + "0" * (-exponent - 1) + digits
    else if (exponent >= 0 && exponent < 7)
      if (digits.length <= exponent + 1) digits + "0" * (exponent + 1 - digits.length) + ".0"
      else digits.substring(0, exponent + 1) + "." + digits.substring(exponent + 1)
    else if (digits.length == 1)
      digits + ".0E" + exponent
    else
      digits.substring(0, 1) + "." + digits.substring(1) + "E" + exponent

  private def stripTrailingZeros(digits: String): String = {
    var end = digits.length
    while (end > 1 && digits.charAt(end - 1) == '0') end -= 1
    digits.substring(0, end)
  }

  private def sign(value: Double): String =
    if (value < 0d) "-" else ""

  private def zero(negative: Boolean): String =
    if (negative) "-0.0" else "0.0"

  private val Half: BigDecimal =
    new BigDecimal("0.5")
}
