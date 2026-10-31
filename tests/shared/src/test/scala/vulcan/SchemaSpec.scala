/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json
import io.circe.JsonObject

object SchemaSpec extends BaseSuite {
  private val props: JsonObject =
    JsonObject("avro.java.string" -> Json.fromString("String"))

  private def fixed(size: Int): Schema.Fixed =
    Schema.Fixed(Schema.Name("Fixed"), size)

  private def withLogicalTypeProp(name: String): JsonObject =
    JsonObject("logicalType" -> Json.fromString(name))

  pureTest("Schema Name should include the namespace in the full name") {
    expect.eql("com.example.Person", Schema.Name("Person", Some("com.example")).fullName)
  }

  pureTest("Schema Name should use the name as full name in the null namespace") {
    expect.eql("Person", Schema.Name("Person").fullName)
  }

  pureTest("Schema Name should treat an empty namespace as the null namespace") {
    expect.same(Schema.Name("Person", None), Schema.Name("Person", Some("")))
  }

  pureTest("Schema Name should treat names with dots as full names, ignoring the namespace") {
    val name = Schema.Name("com.example.Person", Some("other"))
    expect.eql("Person", name.name) &&
    expect.eql(Some("com.example"), name.namespace) &&
    expect.same(name, Schema.Name("com.example.Person")) &&
    expect.same(Schema.Name("Person"), Schema.Name(".Person", Some("other")))
  }

  pureTest("Schema Name should allow full names for named schemas") {
    expect.eql(
      """{"type":"record","name":"Person","namespace":"com.example","fields":[]}""",
      Schema.Record(Schema.Name("com.example.Person"), Vector.empty).toString
    )
  }

  pureTest("Schema Record should not have optional attributes by default") {
    val record = Schema.Record(Schema.Name("Person"), Vector.empty)
    expect.eql(None, record.doc) &&
    expect.eql(Vector.empty, record.aliases) &&
    expect.eql(JsonObject.empty, record.props)
  }

  pureTest("Schema Record should replace attributes using modifiers") {
    val field = Schema.Field("name", Schema.String)
    val record =
      Schema
        .Record(Schema.Name("Person", Some("com.example")), Vector.empty)
        .withFields(Vector(field))
        .withDoc(Some("documentation"))
        .withAliases(Vector("Human"))
        .withProps(JsonObject("custom" -> Json.True))

    expect.eql("com.example.Person", record.fullName) &&
    expect.same(Vector(field), record.fields) &&
    expect.eql(Some("documentation"), record.doc) &&
    expect.eql(Vector("Human"), record.aliases) &&
    expect.eql(JsonObject("custom" -> Json.True), record.props)
  }

  pureTest("Schema Field should have ascending order and no default by default") {
    val field = Schema.Field("name", Schema.String)
    expect.same(Schema.Field.Order.Ascending, field.order) &&
    expect.eql(None, field.default)
  }

  pureTest("Schema Field should distinguish a null default from no default") {
    val field = Schema.Field("name", Schema.Union(Vector(Schema.Null, Schema.String)))
    val withNullDefault = field.withDefault(Some(Json.Null))
    expect.eql(Some(Json.Null), withNullDefault.default) &&
    expect(clue(withNullDefault) != clue(field))
  }

  pureTest("Schema equality should be structural") {
    expect.same(Schema.Array(Schema.Int), Schema.Array(Schema.Int)) &&
    expect(clue(Schema.Array(Schema.Int)) != clue(Schema.Array(Schema.Long))) &&
    expect(clue(Schema.Int.withProps(JsonObject("custom" -> Json.True))) != clue(Schema.Int))
  }

  pureTest("Schema primitive should be the companion object without props") {
    expect.eql(JsonObject.empty, Schema.String.props) &&
    expect(clue(Schema.String.withProps(JsonObject.empty)) eq Schema.String) &&
    expect(clue(Schema.String.withProps(props).withProps(JsonObject.empty)) eq Schema.String)
  }

  pureTest("Schema primitive should be equal with equal props") {
    expect.same(Schema.String.withProps(props), Schema.String.withProps(props)) &&
    expect(clue(Schema.String.withProps(props)) != clue(Schema.String))
  }

  pureTest("Schema primitive should match on the type regardless of props") {
    val schema: Schema = Schema.String.withProps(props)

    matches(schema) { case _: Schema.String =>
      success
    }
  }

  pureTest("Schema isNullable should be true for null") {
    expect(Schema.Null.isNullable)
  }

  pureTest("Schema isNullable should be true for unions including null") {
    expect(Schema.Union(Vector(Schema.Int, Schema.Null)).isNullable)
  }

  pureTest("Schema isNullable should be false for unions without null") {
    expect(!Schema.Union(Vector(Schema.Int, Schema.String)).isNullable)
  }

  pureTest("Schema isNullable should be false for other schemas") {
    expect.all(!Schema.Int.isNullable, !Schema.Array(Schema.Null).isNullable)
  }

  pureTest("Schema logicalType should add the logical type followed by its parameters to the props") {
    val bytes = Schema.Bytes.withLogicalType(LogicalType.decimal(10, 2))
    expect.eql(List("logicalType", "precision", "scale"), bytes.props.keys.toList) &&
    expect.eql(Some(Json.fromString("decimal")), bytes.props("logicalType"))
  }

  pureTest("Schema logicalType should be empty without a logical type") {
    expect.same(None, Schema.Long.logicalType)
  }

  pureTest("Schema logicalType should return logical types which are valid for the underlying type") {
    forEach(List(LogicalType.date, LogicalType.timeMillis)) { logicalType =>
      expect.same(Some(logicalType), Schema.Int.withLogicalType(logicalType).logicalType)
    } &&
    forEach(
      List(
        LogicalType.timeMicros,
        LogicalType.timestampMillis,
        LogicalType.timestampMicros,
        LogicalType.timestampNanos,
        LogicalType.localTimestampMillis,
        LogicalType.localTimestampMicros,
        LogicalType.localTimestampNanos
      )
    ) { logicalType =>
      expect.same(Some(logicalType), Schema.Long.withLogicalType(logicalType).logicalType)
    } &&
    forEach(List(LogicalType.decimal(10, 2), LogicalType.bigDecimal)) { logicalType =>
      expect.same(Some(logicalType), Schema.Bytes.withLogicalType(logicalType).logicalType)
    } &&
    expect.same(Some(LogicalType.uuid), Schema.String.withLogicalType(LogicalType.uuid).logicalType) &&
    expect.same(Some(LogicalType.uuid), fixed(16).withLogicalType(LogicalType.uuid).logicalType) &&
    expect.same(Some(LogicalType.duration), fixed(12).withLogicalType(LogicalType.duration).logicalType) &&
    expect.same(
      Some(LogicalType.decimal(9, 2)),
      fixed(4).withLogicalType(LogicalType.decimal(9, 2)).logicalType
    )
  }

  pureTest("Schema logicalType should ignore logical types which are invalid for the underlying type") {
    expect.same(None, Schema.String.withProps(withLogicalTypeProp("timestamp-millis")).logicalType) &&
    expect.same(None, Schema.Int.withProps(withLogicalTypeProp("timestamp-millis")).logicalType) &&
    expect.same(None, Schema.Long.withProps(withLogicalTypeProp("date")).logicalType) &&
    expect.same(None, Schema.Bytes.withProps(withLogicalTypeProp("uuid")).logicalType) &&
    expect.same(None, fixed(8).withLogicalType(LogicalType.uuid).logicalType) &&
    expect.same(None, fixed(11).withLogicalType(LogicalType.duration).logicalType)
  }

  pureTest("Schema logicalType should ignore decimals with invalid precision or scale") {
    expect.same(None, Schema.Bytes.withLogicalType(LogicalType.decimal(0, 0)).logicalType) &&
    expect.same(None, Schema.Bytes.withLogicalType(LogicalType.decimal(2, 3)).logicalType) &&
    expect.same(None, Schema.Bytes.withLogicalType(LogicalType.decimal(2, -1)).logicalType) &&
    expect.same(None, Schema.Bytes.withProps(withLogicalTypeProp("decimal")).logicalType) &&
    expect.same(None, fixed(4).withLogicalType(LogicalType.decimal(10, 2)).logicalType)
  }

  pureTest("Schema logicalType should default the decimal scale to zero") {
    val bytes =
      Schema.Bytes.withProps(withLogicalTypeProp("decimal").add("precision", Json.fromInt(4)))

    expect.same(Some(LogicalType.decimal(4, 0)), bytes.logicalType)
  }

  pureTest("Schema logicalType should return logical types without built-in support") {
    val custom = LogicalType("custom")
    expect.same(Some(custom), Schema.Long.withLogicalType(custom).logicalType) &&
    expect.same(
      Some(custom),
      Schema
        .Record(Schema.Name("Record"), Vector.empty)
        .withProps(withLogicalTypeProp("custom"))
        .logicalType
    )
  }

  pureTest("Schema logicalType should be empty for unions and references") {
    expect.same(None, Schema.Union(Vector(Schema.Int)).logicalType) &&
    expect.same(None, Schema.Ref(Schema.Name("Record")).logicalType)
  }
}
