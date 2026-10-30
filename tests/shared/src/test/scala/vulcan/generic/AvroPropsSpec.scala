/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import io.circe.Json
import io.circe.JsonObject
import vulcan.BaseSuite

object AvroPropsSpec extends BaseSuite {
  test("AvroProps should provide props via props") {
    forall { (name: String, value: String) =>
      val props = JsonObject(name -> Json.fromString(value))
      expect.eql(props, new AvroProps(props).props)
    }
  }

  test("AvroProps should include props in toString") {
    forall { (name: String, value: String) =>
      val props = JsonObject(name -> Json.fromString(value))
      expect(new AvroProps(props).toString.contains(props.toString))
    }
  }

  test("AvroProps should provide an extractor for props") {
    forall { (name: String, value: String) =>
      matches(new AvroProps(JsonObject(name -> Json.fromString(value)))) { case AvroProps(props) =>
        expect(props.contains(name))
      }
    }
  }
}
