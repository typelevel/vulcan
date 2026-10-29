/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import io.circe.Json
import io.circe.JsonObject
import vulcan.BaseSpec
final class AvroPropsSpec extends BaseSpec {
  describe("AvroProps") {
    it("should provide props via props") {
      forAll { (name: String, value: String) =>
        val props = JsonObject(name -> Json.fromString(value))
        assert(new AvroProps(props).props == props)
      }
    }

    it("should include props in toString") {
      forAll { (name: String, value: String) =>
        val props = JsonObject(name -> Json.fromString(value))
        assert(new AvroProps(props).toString.contains(props.toString))
      }
    }

    it("should provide an extractor for props") {
      forAll { (name: String, value: String) =>
        assert(new AvroProps(JsonObject(name -> Json.fromString(value))) match {
          case AvroProps(props) => props.contains(name)
        })
      }
    }
  }
}
