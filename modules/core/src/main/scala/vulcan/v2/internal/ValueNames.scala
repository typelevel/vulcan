/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.internal

import vulcan.v2.AvroValue

private[v2] object ValueNames {

  /** The name of the type of the value, for use in error messages. */
  def typeName(value: AvroValue): String =
    value match {
      case AvroValue.Null => "null"
      case _: AvroValue.Boolean => "boolean"
      case _: AvroValue.Int => "int"
      case _: AvroValue.Long => "long"
      case _: AvroValue.Float => "float"
      case _: AvroValue.Double => "double"
      case _: AvroValue.Bytes => "bytes"
      case _: AvroValue.String => "string"
      case _: AvroValue.Record => "record"
      case _: AvroValue.Enum => "enum"
      case _: AvroValue.Fixed => "fixed"
      case _: AvroValue.Array => "array"
      case _: AvroValue.Map => "map"
      case _: AvroValue.Union => "union"
    }
}
