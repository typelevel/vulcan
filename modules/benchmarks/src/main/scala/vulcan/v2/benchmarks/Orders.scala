/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.benchmarks

import scodec.bits.ByteVector
import vulcan.v2.{AvroValue, LogicalType, Schema}

/** An order with nested records, an array, a map, an enum, unions, and logical types. */
object Orders {
  private val namespace: Option[String] =
    Some("vulcan.benchmarks")

  private val nullableString: Schema =
    Schema.Union(Vector(Schema.Null, Schema.String))

  private val customer: Schema =
    Schema.Record(
      Schema.Name("Customer", namespace),
      Vector(
        Schema.Field("id", Schema.Long),
        Schema.Field("name", Schema.String),
        Schema.Field("email", nullableString)
      )
    )

  private val item: Schema =
    Schema.Record(
      Schema.Name("Item", namespace),
      Vector(
        Schema.Field("sku", Schema.String),
        Schema.Field("quantity", Schema.Int),
        Schema.Field("price", Schema.Bytes.withLogicalType(LogicalType.decimal(10, 2)))
      )
    )

  private val status: Schema =
    Schema.Enum(Schema.Name("Status", namespace), Vector("Pending", "Paid", "Shipped"))

  val schema: Schema =
    Schema.Record(
      Schema.Name("Order", namespace),
      Vector(
        Schema.Field("id", Schema.String.withLogicalType(LogicalType.uuid)),
        Schema.Field("customer", customer),
        Schema.Field("items", Schema.Array(item)),
        Schema.Field("tags", Schema.Map(Schema.String)),
        Schema.Field("status", status),
        Schema.Field("createdAt", Schema.Long.withLogicalType(LogicalType.timestampMillis)),
        Schema.Field("total", Schema.Double),
        Schema.Field("notes", nullableString)
      )
    )

  val value: AvroValue =
    AvroValue.Record(
      Vector(
        AvroValue.String("0f8fad5b-d9cb-469f-a165-70867728950e"),
        AvroValue.Record(
          Vector(
            AvroValue.Long(1234567L),
            AvroValue.String("Jane Doe"),
            AvroValue.Union(1, AvroValue.String("jane.doe@example.com"))
          )
        ),
        AvroValue.Array((1 to 5).toVector.map { index =>
          AvroValue.Record(
            Vector(
              AvroValue.String(s"SKU-$index"),
              AvroValue.Int(index),
              AvroValue.Bytes(ByteVector.fromInt(index * 1999))
            )
          )
        }),
        AvroValue.Map(
          Vector(
            "channel" -> AvroValue.String("web"),
            "campaign" -> AvroValue.String("autumn"),
            "region" -> AvroValue.String("eu")
          )
        ),
        AvroValue.Enum("Paid"),
        AvroValue.Long(1767225600000L),
        AvroValue.Double(49.95),
        AvroValue.Union(0, AvroValue.Null)
      )
    )
}
