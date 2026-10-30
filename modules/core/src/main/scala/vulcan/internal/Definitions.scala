/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import scala.collection.mutable
import vulcan.Schema

/** The named schemas defined in a writer schema, for resolving references to them. */
private[vulcan] final class Definitions(writerSchema: Schema) {
  private lazy val definitions: collection.Map[String, Schema.Named] =
    SchemaNames.definitions(writerSchema)

  def get(fullName: String): Option[Schema.Named] =
    definitions.get(fullName)

  /**
    * Returns the specified [[Schema]] but with the first reference to
    * each named schema defined outside of it replaced by its definition.
    */
  def expanded(schema: Schema): Schema = {
    val defined = mutable.Set.empty[String]

    def expand(schema: Schema): Schema =
      schema match {
        case record: Schema.Record =>
          defined += record.fullName
          record.withFields(record.fields.map(field => field.withSchema(expand(field.schema))))

        case named: Schema.Named =>
          defined += named.fullName
          named

        case array: Schema.Array => array.withItems(expand(array.items))
        case map: Schema.Map => map.withValues(expand(map.values))
        case union: Schema.Union => union.withTypes(union.types.map(expand))
        case ref: Schema.Ref if !defined.contains(ref.fullName) => get(ref.fullName).fold(schema)(expand)
        case _ => schema
      }

    expand(schema)
  }
}
