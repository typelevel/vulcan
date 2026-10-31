/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Hash
import cats.Show
import io.circe.Json
import io.circe.JsonObject
import vulcan.internal.SchemaValidator

/** Logical type annotating an underlying Avro type. */
sealed abstract class LogicalType {
  def name: String

  def params: JsonObject
}

object LogicalType {
  private final case class LogicalTypeImpl(
    override val name: String,
    override val params: JsonObject
  ) extends LogicalType {
    override def toString: String =
      if (params.isEmpty) s"LogicalType($name)"
      else s"LogicalType($name, ${Json.fromJsonObject(params).noSpaces})"
  }

  def apply(name: String): LogicalType =
    apply(name, JsonObject.empty)

  def apply(name: String, params: JsonObject): LogicalType =
    LogicalTypeImpl(name, params)

  def decimal(precision: Int, scale: Int): LogicalType =
    apply(
      "decimal",
      JsonObject(
        "precision" -> Json.fromInt(precision),
        "scale" -> Json.fromInt(scale)
      )
    )

  val bigDecimal: LogicalType = apply("big-decimal")

  val date: LogicalType = apply("date")

  val duration: LogicalType = apply("duration")

  val localTimestampMicros: LogicalType = apply("local-timestamp-micros")

  val localTimestampMillis: LogicalType = apply("local-timestamp-millis")

  val localTimestampNanos: LogicalType = apply("local-timestamp-nanos")

  val timeMicros: LogicalType = apply("time-micros")

  val timeMillis: LogicalType = apply("time-millis")

  val timestampMicros: LogicalType = apply("timestamp-micros")

  val timestampMillis: LogicalType = apply("timestamp-millis")

  val timestampNanos: LogicalType = apply("timestamp-nanos")

  val uuid: LogicalType = apply("uuid")

  /** Extracts the precision and scale of a decimal logical type. */
  object Decimal {
    def unapply(logicalType: LogicalType): Option[(Int, Int)] =
      if (logicalType.name == "decimal")
        decimalParams(logicalType.params)
      else None
  }

  implicit final val logicalTypeHash: Hash[LogicalType] =
    Hash.fromUniversalHashCode

  implicit final val logicalTypeShow: Show[LogicalType] =
    Show.fromToString

  private val knownNames: Set[String] =
    Set(
      "big-decimal",
      "date",
      "decimal",
      "duration",
      "local-timestamp-micros",
      "local-timestamp-millis",
      "local-timestamp-nanos",
      "time-micros",
      "time-millis",
      "timestamp-micros",
      "timestamp-millis",
      "timestamp-nanos",
      "uuid"
    )

  /** Returns the logical type of the schema. Ignores invalid combinations. */
  private[vulcan] def of(schema: Schema): Option[LogicalType] =
    schema.props("logicalType").flatMap(_.asString).flatMap { name =>
      schema match {
        case _: Schema.Int =>
          name match {
            case "date" => Some(date)
            case "time-millis" => Some(timeMillis)
            case other => unknown(other)
          }

        case _: Schema.Long =>
          name match {
            case "time-micros" => Some(timeMicros)
            case "timestamp-millis" => Some(timestampMillis)
            case "timestamp-micros" => Some(timestampMicros)
            case "timestamp-nanos" => Some(timestampNanos)
            case "local-timestamp-millis" => Some(localTimestampMillis)
            case "local-timestamp-micros" => Some(localTimestampMicros)
            case "local-timestamp-nanos" => Some(localTimestampNanos)
            case other => unknown(other)
          }

        case _: Schema.Bytes =>
          name match {
            case "decimal" => decimalOf(schema.props, maxPrecision = Long.MaxValue)
            case "big-decimal" => Some(bigDecimal)
            case other => unknown(other)
          }

        case _: Schema.String =>
          name match {
            case "uuid" => Some(uuid)
            case other => unknown(other)
          }

        case fixed: Schema.Fixed =>
          name match {
            case "decimal" => decimalOf(schema.props, maxPrecision(fixed.size))
            case "uuid" => if (fixed.size == 16) Some(uuid) else None
            case "duration" => if (fixed.size == 12) Some(duration) else None
            case other => unknown(other)
          }

        case _ =>
          unknown(name)
      }
    }

  private def unknown(name: String): Option[LogicalType] =
    if (knownNames.contains(name)) None else Some(apply(name))

  private def decimalOf(props: JsonObject, maxPrecision: Long): Option[LogicalType] =
    decimalParams(props).collect {
      case (precision, scale) if precision <= maxPrecision =>
        decimal(precision, scale)
    }

  private def decimalParams(params: JsonObject): Option[(Int, Int)] =
    for {
      precision <- params("precision").flatMap(SchemaValidator.intValue)
      scale <- params("scale").fold(Option(0))(SchemaValidator.intValue)
      if precision > 0 && scale >= 0 && scale <= precision
    } yield (precision, scale)

  /** Maximum number of digits which fit in the fixed size. */
  private def maxPrecision(size: Int): Long =
    math.round(math.floor(math.log10(2) * (8 * size - 1)))
}
