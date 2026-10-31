/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.examples

import cats.syntax.all.*
import io.circe.Json
import io.circe.JsonObject
import vulcan.Codec
import vulcan.Schema

final case class CaseClassTwoFields(name: String, age: Int)

object CaseClassTwoFields {
  implicit val codec: Codec[CaseClassTwoFields] =
    Codec.record(
      name = "CaseClassTwoFields",
      namespace = "vulcan.examples",
      doc = Some("some documentation for example"),
      aliases = Seq("FirstAlias", "SecondAlias"),
      props = JsonObject("custom" -> Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3)))
    ) { field =>
      (
        field(
          name = "name",
          access = _.name,
          doc = Some("some doc"),
          default = Some("default name"),
          order = Schema.Field.Order.Descending,
          aliases = Seq("TheAlias"),
          props = JsonObject("custom" -> Json.fromString("value"))
        ),
        field("age", _.age)
      ).mapN(CaseClassTwoFields(_, _))
    }
}
