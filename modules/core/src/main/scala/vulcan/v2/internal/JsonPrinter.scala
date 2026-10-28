/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.internal

import io.circe.Json

private[v2] object JsonPrinter {

  /** Print `Json` without whitespace, matching the Java Avro library for compatibility. */
  def print(json: Json): String =
    json.fold(
      "null",
      boolean => if (boolean) "true" else "false",
      number => Json.fromJsonNumber(number).noSpaces,
      quote,
      values => values.map(print).mkString("[", ",", "]"),
      fields =>
        fields.toIterable
          .map { case (key, value) => s"${quote(key)}:${print(value)}" }
          .mkString("{", ",", "}")
    )

  private def quote(string: String): String =
    if (string.forall(char => char >= ' ' && char != '"' && char != '\\')) "\"" + string + "\""
    else string.iterator.map(escape).mkString("\"", "", "\"")

  private def escape(char: Char): String =
    char match {
      case '"' => "\\\""
      case '\\' => "\\\\"
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case _ if char < ' ' =>
        s"\\u00${HexDigits.charAt(char >> 4)}${HexDigits.charAt(char & 0xf)}"
      case _ =>
        char.toString
    }

  private val HexDigits: String = "0123456789ABCDEF"
}
