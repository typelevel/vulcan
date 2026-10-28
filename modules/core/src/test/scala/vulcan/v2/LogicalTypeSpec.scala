/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import io.circe.Json
import io.circe.JsonObject
import vulcan.BaseSpec

final class LogicalTypeSpec extends BaseSpec {
  describe("LogicalType") {
    it("should use the names from the specification") {
      assert(LogicalType.bigDecimal.name == "big-decimal")
      assert(LogicalType.date.name == "date")
      assert(LogicalType.decimal(10, 2).name == "decimal")
      assert(LogicalType.duration.name == "duration")
      assert(LogicalType.localTimestampMicros.name == "local-timestamp-micros")
      assert(LogicalType.localTimestampMillis.name == "local-timestamp-millis")
      assert(LogicalType.localTimestampNanos.name == "local-timestamp-nanos")
      assert(LogicalType.timeMicros.name == "time-micros")
      assert(LogicalType.timeMillis.name == "time-millis")
      assert(LogicalType.timestampMicros.name == "timestamp-micros")
      assert(LogicalType.timestampMillis.name == "timestamp-millis")
      assert(LogicalType.timestampNanos.name == "timestamp-nanos")
      assert(LogicalType.uuid.name == "uuid")
    }

    it("should be equal when names and parameters are equal") {
      assert(LogicalType("custom") == LogicalType("custom", JsonObject.empty))
      assert(LogicalType.decimal(10, 2) == LogicalType.decimal(10, 2))
      assert(LogicalType.decimal(10, 2) != LogicalType.decimal(10, 3))
    }

    it("should include the parameters in toString") {
      assert(LogicalType.date.toString == "LogicalType(date)")
      assert {
        LogicalType
          .decimal(10, 2)
          .toString == """LogicalType(decimal, {"precision":10,"scale":2})"""
      }
    }

    describe("decimal") {
      it("should have precision and scale parameters") {
        assert {
          LogicalType.decimal(10, 2).params ==
            JsonObject("precision" -> Json.fromInt(10), "scale" -> Json.fromInt(2))
        }
      }

      it("should extract the precision and scale") {
        val extracted =
          LogicalType.decimal(10, 2) match {
            case LogicalType.Decimal(precision, scale) => Some((precision, scale))
            case _ => None
          }

        assert(extracted == Some((10, 2)))
      }

      it("should not extract other logical types") {
        assert(LogicalType.Decimal.unapply(LogicalType.date).isEmpty)
      }
    }
  }
}
