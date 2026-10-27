/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.internal

import vulcan.v2.Schema

/** The named schemas defined in a writer schema, for resolving references to them. */
private[v2] final class Definitions(writerSchema: Schema) {
  private lazy val definitions: collection.Map[String, Schema.Named] =
    SchemaNames.definitions(writerSchema)

  def get(fullName: String): Option[Schema.Named] =
    definitions.get(fullName)
}
