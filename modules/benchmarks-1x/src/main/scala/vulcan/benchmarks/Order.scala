/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.benchmarks

import cats.syntax.all.*
import java.time.Instant
import java.util.UUID
import vulcan.AvroError
import vulcan.Codec

/**
  * An order with nested records, an array, a map, an enum, unions and logical types.
  *
  * The benchmarks for the current version and for version 1.x have copies of this
  * file, which only differ in the error type, so they compare the same schema and values.
  */
final case class Order(
  id: UUID,
  customer: Customer,
  items: List[Item],
  tags: Map[String, String],
  status: Status,
  payment: Payment,
  createdAt: Instant,
  total: Double,
  notes: Option[String]
)

object Order {
  implicit val codec: Codec[Order] =
    Codec.record("Order", "vulcan.benchmarks") { field =>
      (
        field("id", _.id),
        field("customer", _.customer),
        field("items", _.items),
        field("tags", _.tags),
        field("status", _.status),
        field("payment", _.payment),
        field("createdAt", _.createdAt),
        field("total", _.total),
        field("notes", _.notes)
      ).mapN(Order(_, _, _, _, _, _, _, _, _))
    }

  val example: Order =
    Order(
      id = UUID.fromString("0f8fad5b-d9cb-469f-a165-70867728950e"),
      customer = Customer(1234567L, "Jane Doe", Some("jane.doe@example.com")),
      items = (1 to 5).toList.map(index => Item(s"SKU-$index", index, BigDecimal(index * 1999L, 2))),
      tags = Map("channel" -> "web", "campaign" -> "autumn", "region" -> "eu"),
      status = Status.Paid,
      payment = Payment.Card("4242"),
      createdAt = Instant.ofEpochMilli(1767225600000L),
      total = 49.95,
      notes = None
    )
}

final case class Customer(id: Long, name: String, email: Option[String])

object Customer {
  implicit val codec: Codec[Customer] =
    Codec.record("Customer", "vulcan.benchmarks") { field =>
      (
        field("id", _.id),
        field("name", _.name),
        field("email", _.email)
      ).mapN(Customer(_, _, _))
    }
}

final case class Item(sku: String, quantity: Int, price: BigDecimal)

object Item {
  implicit val codec: Codec[Item] =
    Codec.record("Item", "vulcan.benchmarks") { field =>
      (
        field("sku", _.sku),
        field("quantity", _.quantity),
        field("price", _.price)(Codec.decimal(precision = 10, scale = 2))
      ).mapN(Item(_, _, _))
    }
}

sealed trait Status

object Status {
  case object Pending extends Status
  case object Paid extends Status
  case object Shipped extends Status

  implicit val codec: Codec[Status] =
    Codec.enumeration[Status](
      name = "Status",
      namespace = "vulcan.benchmarks",
      symbols = List("Pending", "Paid", "Shipped"),
      encode = _.toString,
      decode = {
        case "Pending" => Right(Pending)
        case "Paid" => Right(Paid)
        case "Shipped" => Right(Shipped)
        case symbol => Left(AvroError(s"Unknown status $symbol"))
      }
    )
}

sealed trait Payment

object Payment {
  final case class Card(last4: String) extends Payment

  object Card {
    implicit val codec: Codec[Card] =
      Codec.record("Card", "vulcan.benchmarks") { field =>
        field("last4", _.last4).map(Card(_))
      }
  }

  final case class Invoice(reference: String, dueDays: Int) extends Payment

  object Invoice {
    implicit val codec: Codec[Invoice] =
      Codec.record("Invoice", "vulcan.benchmarks") { field =>
        (
          field("reference", _.reference),
          field("dueDays", _.dueDays)
        ).mapN(Invoice(_, _))
      }
  }

  implicit val codec: Codec[Payment] =
    Codec.union(alt => alt[Card] |+| alt[Invoice])
}
