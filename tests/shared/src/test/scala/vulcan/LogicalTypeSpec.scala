/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.syntax.all.*
import io.circe.Json
import io.circe.JsonObject

object LogicalTypeSpec extends BaseSuite {
  pureTest("LogicalType should use the names from the specification") {
    expect.eql("big-decimal", LogicalType.bigDecimal.name) &&
    expect.eql("date", LogicalType.date.name) &&
    expect.eql("decimal", LogicalType.decimal(10, 2).name) &&
    expect.eql("duration", LogicalType.duration.name) &&
    expect.eql("local-timestamp-micros", LogicalType.localTimestampMicros.name) &&
    expect.eql("local-timestamp-millis", LogicalType.localTimestampMillis.name) &&
    expect.eql("local-timestamp-nanos", LogicalType.localTimestampNanos.name) &&
    expect.eql("time-micros", LogicalType.timeMicros.name) &&
    expect.eql("time-millis", LogicalType.timeMillis.name) &&
    expect.eql("timestamp-micros", LogicalType.timestampMicros.name) &&
    expect.eql("timestamp-millis", LogicalType.timestampMillis.name) &&
    expect.eql("timestamp-nanos", LogicalType.timestampNanos.name) &&
    expect.eql("uuid", LogicalType.uuid.name)
  }

  pureTest("LogicalType should be equal when names and parameters are equal") {
    expect.eql(LogicalType("custom", JsonObject.empty), LogicalType("custom")) &&
    expect.eql(LogicalType.decimal(10, 2), LogicalType.decimal(10, 2)) &&
    expect(clue(LogicalType.decimal(10, 2)) != clue(LogicalType.decimal(10, 3)))
  }

  pureTest("LogicalType should include the parameters in toString") {
    expect.eql("LogicalType(date)", LogicalType.date.toString) &&
    expect.eql(
      """LogicalType(decimal, {"precision":10,"scale":2})""",
      LogicalType.decimal(10, 2).toString
    )
  }

  pureTest("LogicalType show should have a Show instance consistent with toString") {
    val logicalType = LogicalType.decimal(10, 2)
    expect.eql(logicalType.toString, logicalType.show)
  }

  pureTest("LogicalType decimal should have precision and scale parameters") {
    expect.eql(
      JsonObject("precision" -> Json.fromInt(10), "scale" -> Json.fromInt(2)),
      LogicalType.decimal(10, 2).params
    )
  }

  pureTest("LogicalType decimal should extract the precision and scale") {
    matches(LogicalType.decimal(10, 2)) { case LogicalType.Decimal(precision, scale) =>
      expect.eql((10, 2), (precision, scale))
    }
  }

  pureTest("LogicalType decimal should not extract other logical types") {
    expect.same(None, LogicalType.Decimal.unapply(LogicalType.date))
  }

  pureTest(
    "LogicalType decimal should only use integers without fraction and exponent, same as the Java Avro library"
  ) {
    def logicalType(params: String): Either[AvroException, Option[LogicalType]] =
      Schema.parse(s"""{"type":"bytes","logicalType":"decimal",$params}""").map(_.logicalType)

    expect.same(Right(Some(LogicalType.decimal(4, 2))), logicalType(""""precision":4,"scale":2""")) &&
    expect.same(Right(None), logicalType(""""precision":4.0,"scale":2""")) &&
    expect.same(Right(None), logicalType(""""precision":4e0,"scale":2""")) &&
    expect.same(Right(None), logicalType(""""precision":4,"scale":2.0"""))
  }
}
