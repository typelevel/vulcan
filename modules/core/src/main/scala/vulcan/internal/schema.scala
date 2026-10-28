/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import java.nio.ByteBuffer
import java.util as ju
import org.apache.avro.Schema
import org.apache.avro.SchemaFormatter
import org.apache.avro.generic.GenericEnumSymbol
import org.apache.avro.generic.GenericFixed
import org.apache.avro.generic.IndexedRecord
import vulcan.internal.converters.collection.*

private[vulcan] object schema {
  private val jsonPrettyFormatter: SchemaFormatter =
    SchemaFormatter.getInstance("json/pretty")

  final def prettyPrint(schema: Schema): String =
    jsonPrettyFormatter.format(schema)

  final def adaptForSchema(encoded: Any): Any =
    encoded match {
      case bytes: ByteBuffer =>
        bytes.array()
      case genericEnum: GenericEnumSymbol[_] =>
        genericEnum.toString()
      case fixed: GenericFixed =>
        fixed.bytes()
      case record: IndexedRecord =>
        record.getSchema.getFields.asScala.zipWithIndex
          .foldLeft(Map.empty[String, Any]) { case (map, (field, index)) =>
            map.updated(field.name, adaptForSchema(record.get(index)))
          }
          .asJava
      case array: ju.Collection[?] =>
        array.asScala.map(adaptForSchema).asJava
      case map: ju.Map[?, ?] =>
        // Avro only supports maps with String key, so we only have to adapt the values
        map.asScala.map { case (key, value) => key -> adaptForSchema(value) }.asJava
      case _ =>
        encoded
    }
}
