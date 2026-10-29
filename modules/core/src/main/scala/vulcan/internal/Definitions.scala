/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import vulcan.Schema

/** The named schemas defined in a writer schema, for resolving references to them. */
private[vulcan] final class Definitions(writerSchema: Schema) {
  private lazy val definitions: collection.Map[String, Schema.Named] =
    SchemaNames.definitions(writerSchema)

  def get(fullName: String): Option[Schema.Named] =
    definitions.get(fullName)
}
