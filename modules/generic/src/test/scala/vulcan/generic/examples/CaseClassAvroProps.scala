/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic.examples

import io.circe.Json
import io.circe.JsonObject
import vulcan.Codec
import vulcan.generic.*

@AvroName("CaseClassAvroProps")
final case class CaseClassAvroProps(
  @AvroName("value")
  @AvroProps(JsonObject("prop1" -> Json.fromString("A"), "prop2" -> Json.fromString("B")))
  value: Option[String]
)

object CaseClassAvroProps {
  implicit val codec: Codec[CaseClassAvroProps] = Codec.derive
}
