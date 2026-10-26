/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import scodec.bits.ByteVector

/** Avro value interpreted according to a [[Schema]]. */
sealed abstract class AvroValue extends Product with Serializable

object AvroValue {
  case object Null extends AvroValue

  final case class Boolean(value: scala.Boolean) extends AvroValue

  final case class Int(value: scala.Int) extends AvroValue

  final case class Long(value: scala.Long) extends AvroValue

  /** Float value, where `NaN` is equal to itself, and `0.0` and `-0.0` are different. */
  final case class Float(value: scala.Float) extends AvroValue {
    override def equals(that: Any): scala.Boolean =
      that match {
        case Float(other) => java.lang.Float.compare(value, other) == 0
        case _            => false
      }

    override def hashCode: scala.Int =
      java.lang.Float.floatToIntBits(value)
  }

  /** Double value, where `NaN` is equal to itself, and `0.0` and `-0.0` are different. */
  final case class Double(value: scala.Double) extends AvroValue {
    override def equals(that: Any): scala.Boolean =
      that match {
        case Double(other) => java.lang.Double.compare(value, other) == 0
        case _             => false
      }

    override def hashCode: scala.Int = {
      val bits = java.lang.Double.doubleToLongBits(value)
      (bits ^ (bits >>> 32)).toInt
    }
  }

  final case class Bytes(value: ByteVector) extends AvroValue

  final case class String(value: Predef.String) extends AvroValue

  /** Record value, with field values in the order of the record schema fields. */
  final case class Record(values: Vector[AvroValue]) extends AvroValue

  final case class Enum(symbol: Predef.String) extends AvroValue

  final case class Fixed(value: ByteVector) extends AvroValue

  final case class Array(items: Vector[AvroValue]) extends AvroValue

  final case class Map(entries: Vector[(Predef.String, AvroValue)]) extends AvroValue

  /** Union value, with the index of the union schema type. */
  final case class Union(index: scala.Int, value: AvroValue) extends AvroValue
}
