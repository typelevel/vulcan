/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json
import io.circe.JsonObject

final class SchemaSpec extends BaseSpec {
  describe("Schema") {
    describe("Name") {
      it("should include the namespace in the full name") {
        assert(Schema.Name("Person", Some("com.example")).fullName == "com.example.Person")
      }

      it("should use the name as full name in the null namespace") {
        assert(Schema.Name("Person").fullName == "Person")
      }

      it("should treat an empty namespace as the null namespace") {
        assert(Schema.Name("Person", Some("")) == Schema.Name("Person", None))
      }
    }

    describe("Record") {
      it("should not have optional attributes by default") {
        val record = Schema.Record(Schema.Name("Person"), Vector.empty)
        assert(record.doc.isEmpty)
        assert(record.aliases.isEmpty)
        assert(record.props.isEmpty)
      }

      it("should replace attributes using modifiers") {
        val field = Schema.Field("name", Schema.String)
        val record =
          Schema
            .Record(Schema.Name("Person", Some("com.example")), Vector.empty)
            .withFields(Vector(field))
            .withDoc(Some("documentation"))
            .withAliases(Vector("Human"))
            .withProps(JsonObject("custom" -> Json.True))

        assert(record.fullName == "com.example.Person")
        assert(record.fields == Vector(field))
        assert(record.doc == Some("documentation"))
        assert(record.aliases == Vector("Human"))
        assert(record.props == JsonObject("custom" -> Json.True))
      }
    }

    describe("Field") {
      it("should have ascending order and no default by default") {
        val field = Schema.Field("name", Schema.String)
        assert(field.order == Schema.Field.Order.Ascending)
        assert(field.default.isEmpty)
      }

      it("should distinguish a null default from no default") {
        val field = Schema.Field("name", Schema.Union(Vector(Schema.Null, Schema.String)))
        val withNullDefault = field.withDefault(Some(Json.Null))
        assert(withNullDefault.default == Some(Json.Null))
        assert(withNullDefault != field)
      }
    }

    describe("equality") {
      it("should be structural") {
        assert(Schema.Array(Schema.Int) == Schema.Array(Schema.Int))
        assert(Schema.Array(Schema.Int) != Schema.Array(Schema.Long))
        assert(Schema.Int.withProps(JsonObject("custom" -> Json.True)) != Schema.Int)
      }
    }

    describe("primitive") {
      val props = JsonObject("avro.java.string" -> Json.fromString("String"))

      it("should be the companion object without props") {
        assert(Schema.String.props.isEmpty)
        assert(Schema.String.withProps(JsonObject.empty) eq Schema.String)
        assert(Schema.String.withProps(props).withProps(JsonObject.empty) eq Schema.String)
      }

      it("should be equal with equal props") {
        assert(Schema.String.withProps(props) == Schema.String.withProps(props))
        assert(Schema.String.withProps(props) != Schema.String)
      }

      it("should match on the type regardless of props") {
        val schema: Schema = Schema.String.withProps(props)

        assert(schema match {
          case _: Schema.String => true
          case _ => false
        })
      }
    }

    describe("isNullable") {
      it("should be true for null") {
        assert(Schema.Null.isNullable)
      }

      it("should be true for unions including null") {
        assert(Schema.Union(Vector(Schema.Int, Schema.Null)).isNullable)
      }

      it("should be false for unions without null") {
        assert(!Schema.Union(Vector(Schema.Int, Schema.String)).isNullable)
      }

      it("should be false for other schemas") {
        assert(!Schema.Int.isNullable)
        assert(!Schema.Array(Schema.Null).isNullable)
      }
    }

    describe("logicalType") {
      def fixed(size: Int): Schema.Fixed =
        Schema.Fixed(Schema.Name("Fixed"), size)

      def withLogicalTypeProp(name: String): JsonObject =
        JsonObject("logicalType" -> Json.fromString(name))

      it("should add the logical type followed by its parameters to the props") {
        val bytes = Schema.Bytes.withLogicalType(LogicalType.decimal(10, 2))
        assert(bytes.props.keys.toList == List("logicalType", "precision", "scale"))
        assert(bytes.props("logicalType") == Some(Json.fromString("decimal")))
      }

      it("should be empty without a logical type") {
        assert(Schema.Long.logicalType.isEmpty)
      }

      it("should return logical types which are valid for the underlying type") {
        List(LogicalType.date, LogicalType.timeMillis).foreach { logicalType =>
          assert(Schema.Int.withLogicalType(logicalType).logicalType == Some(logicalType))
        }

        List(
          LogicalType.timeMicros,
          LogicalType.timestampMillis,
          LogicalType.timestampMicros,
          LogicalType.timestampNanos,
          LogicalType.localTimestampMillis,
          LogicalType.localTimestampMicros,
          LogicalType.localTimestampNanos
        ).foreach { logicalType =>
          assert(Schema.Long.withLogicalType(logicalType).logicalType == Some(logicalType))
        }

        List(LogicalType.decimal(10, 2), LogicalType.bigDecimal).foreach { logicalType =>
          assert(Schema.Bytes.withLogicalType(logicalType).logicalType == Some(logicalType))
        }

        assert {
          Schema.String.withLogicalType(LogicalType.uuid).logicalType == Some(LogicalType.uuid)
        }

        assert(fixed(16).withLogicalType(LogicalType.uuid).logicalType == Some(LogicalType.uuid))

        assert {
          fixed(12).withLogicalType(LogicalType.duration).logicalType == Some(LogicalType.duration)
        }

        assert {
          fixed(4).withLogicalType(LogicalType.decimal(9, 2)).logicalType ==
            Some(LogicalType.decimal(9, 2))
        }
      }

      it("should ignore logical types which are invalid for the underlying type") {
        assert(Schema.String.withProps(withLogicalTypeProp("timestamp-millis")).logicalType.isEmpty)
        assert(Schema.Int.withProps(withLogicalTypeProp("timestamp-millis")).logicalType.isEmpty)
        assert(Schema.Long.withProps(withLogicalTypeProp("date")).logicalType.isEmpty)
        assert(Schema.Bytes.withProps(withLogicalTypeProp("uuid")).logicalType.isEmpty)
        assert(fixed(8).withLogicalType(LogicalType.uuid).logicalType.isEmpty)
        assert(fixed(11).withLogicalType(LogicalType.duration).logicalType.isEmpty)
      }

      it("should ignore decimals with invalid precision or scale") {
        assert(Schema.Bytes.withLogicalType(LogicalType.decimal(0, 0)).logicalType.isEmpty)
        assert(Schema.Bytes.withLogicalType(LogicalType.decimal(2, 3)).logicalType.isEmpty)
        assert(Schema.Bytes.withLogicalType(LogicalType.decimal(2, -1)).logicalType.isEmpty)
        assert(Schema.Bytes.withProps(withLogicalTypeProp("decimal")).logicalType.isEmpty)
        assert(fixed(4).withLogicalType(LogicalType.decimal(10, 2)).logicalType.isEmpty)
      }

      it("should default the decimal scale to zero") {
        val bytes =
          Schema.Bytes.withProps(withLogicalTypeProp("decimal").add("precision", Json.fromInt(4)))

        assert(bytes.logicalType == Some(LogicalType.decimal(4, 0)))
      }

      it("should return logical types without built-in support") {
        val custom = LogicalType("custom")
        assert(Schema.Long.withLogicalType(custom).logicalType == Some(custom))

        assert {
          Schema
            .Record(Schema.Name("Record"), Vector.empty)
            .withProps(withLogicalTypeProp("custom"))
            .logicalType == Some(custom)
        }
      }

      it("should be empty for unions and references") {
        assert(Schema.Union(Vector(Schema.Int)).logicalType.isEmpty)
        assert(Schema.Ref(Schema.Name("Record")).logicalType.isEmpty)
      }
    }
  }
}
