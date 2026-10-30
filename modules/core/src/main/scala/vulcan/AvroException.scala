/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json
import scala.util.control.NoStackTrace
import vulcan.internal.JsonPrinter
import vulcan.internal.SchemaNames
import vulcan.internal.ValueNames

/**
  * Exception raised when generating a schema, or when encoding or decoding fails.
  *
  * Use `AvroException(message)` or `AvroException(message, cause)` to
  * create exceptions, for example when decoding fails in custom codecs.
  */
class AvroException private[vulcan] (val message: String, val cause: Option[Throwable] = None)
  extends RuntimeException(message, cause.orNull)
  with NoStackTrace

object AvroException {

  /**
    * Returns a new exception with the specified message.
    */
  def apply(message: String): AvroException =
    new AvroException(message)

  /**
    * Returns a new exception with the specified message and cause.
    */
  def apply(message: String, cause: Throwable): AvroException =
    new AvroException(message, Some(cause))

  /**
    * Exception raised when the Avro binary encoding of a value
    * exceeds the maximum size of byte arrays.
    */
  final class BinarySizeExceeded private[vulcan] (maxSize: Int)
    extends AvroException(s"Unable to write more than $maxSize bytes")

  /**
    * Exception raised when a schema has different definitions
    * of named schemas with the same full name.
    */
  final class ConflictingDefinitions private[vulcan] (fullName: String)
    extends AvroException(s"Conflicting definitions of named schema $fullName")

  /**
    * Exception raised when encoding a date where the number
    * of days since the epoch does not fit in an `Int`.
    */
  final class DateSizeExceeded private[vulcan] (epochDays: Long)
    extends AvroException(
      s"Unable to encode date as epoch days of $epochDays exceeds the maximum integer size"
    )

  /**
    * Exception raised when encoding a decimal with
    * a scale other than the scale of the schema.
    */
  final class DecimalScaleMismatch private[vulcan] (scale: Int, expectedScale: Int)
    extends AvroException(s"Unable to encode decimal with scale $scale as scale $expectedScale")

  /**
    * Exception raised when decoding a decimal with a precision
    * exceeding the precision of the schema.
    */
  final class DecodeDecimalPrecisionExceeded private[vulcan] (precision: Int, expectedPrecision: Int)
    extends AvroException(
      s"Unable to decode decimal with precision $precision exceeding schema precision $expectedPrecision"
    )

  /**
    * Exception raised when decoding a fixed value with a number
    * of bytes other than the size of the schema.
    */
  final class DecodeFixedSizeMismatch private[vulcan] (size: Int, fixedSize: Int)
    extends AvroException(s"Got $size bytes, expected fixed size $fixedSize")

  /**
    * Exception raised when decoding an enum symbol which is not one
    * of the symbols of the schema, and the codec has no default.
    */
  final class DecodeSymbolNotInSchema private[vulcan] (symbol: String, symbols: Seq[String])
    extends AvroException(s"$symbol is not one of schema symbols ${symbols.mkString("[", ", ", "]")}")

  /**
    * Exception raised when a record schema has
    * more than one field with the same name.
    */
  final class DuplicateField private[vulcan] (name: String, record: Schema.Record)
    extends AvroException(s"""Duplicate field "$name" in record ${record.fullName}""")

  /**
    * Exception raised when an enum schema has
    * the same symbol more than once.
    */
  final class DuplicateSymbol private[vulcan] (symbol: String, enumeration: Schema.Enum)
    extends AvroException(s"""Duplicate symbol "$symbol" in enum ${enumeration.fullName}""")

  /**
    * Exception raised when a union schema has the same type more than
    * once, where named schemas are identified by their full names.
    */
  final class DuplicateUnionType private[vulcan] (key: String, union: Schema.Union)
    extends AvroException(s"Duplicate $key in union $union")

  /**
    * Exception raised when decoding a non-empty collection from an empty array.
    */
  final class EmptyCollection private[vulcan] () extends AvroException("Got unexpected empty collection")

  /**
    * Exception raised when decoding a decimal from empty bytes.
    */
  final class EmptyDecimal private[vulcan] ()
    extends AvroException("Unable to decode decimal from empty bytes")

  /**
    * Exception raised when encoding a decimal with a precision
    * exceeding the precision of the schema.
    */
  final class EncodeDecimalPrecisionExceeded private[vulcan] (precision: Int, expectedPrecision: Int)
    extends AvroException(
      s"Unable to encode decimal with precision $precision exceeding schema precision $expectedPrecision"
    )

  /**
    * Exception raised when encoding a fixed value with
    * more bytes than the size of the schema.
    */
  final class EncodeFixedSizeExceeded private[vulcan] (size: Int, fixedSize: Int)
    extends AvroException(s"Got $size bytes, expected maximum fixed size $fixedSize")

  /**
    * Exception raised when encoding a value as an enum symbol
    * which is not one of the symbols of the schema.
    */
  final class EncodeSymbolNotInSchema private[vulcan] (symbol: String, symbols: Seq[String])
    extends AvroException(s"Symbol $symbol is not part of schema symbols [${symbols.mkString(", ")}]")

  /**
    * Exception raised when encoding a time where the number of
    * units since the epoch does not fit in a `Long`.
    */
  final class EpochSizeExceeded private[vulcan] (value: Any, unit: String)
    extends AvroException(s"Unable to encode $value as epoch $unit exceeding the maximum long size")

  /**
    * Exception raised when decoding a type fails, with the
    * exception for the failure as the cause.
    */
  final class ErrorDecodingType private[vulcan] (typeName: String, cause: AvroException)
    extends AvroException(s"Error decoding $typeName: ${cause.message}", Some(cause))

  /**
    * Exception raised when encoding a type fails, with the
    * exception for the failure as the cause.
    */
  final class ErrorEncodingType private[vulcan] (typeName: String, cause: AvroException)
    extends AvroException(s"Error encoding $typeName: ${cause.message}", Some(cause))

  /**
    * Exception raised when encoding a value with a union
    * codec where no alternative includes the value.
    */
  final class ExhaustedAlternatives private[vulcan] ()
    extends AvroException("Exhausted alternatives for union")

  /**
    * Exception raised when a schema for Avro binary decoding has a
    * record which always contains itself, so values are infinite.
    */
  final class InfiniteRecord private[vulcan] (record: Schema.Record)
    extends AvroException(s"Unable to decode record ${record.fullName}, since it always contains itself")

  /**
    * Exception raised when decoding Avro binary fails, because
    * the bytes are invalid for the writer schema.
    */
  final class InvalidBinary private[vulcan] (details: String) extends AvroException(details)

  /**
    * Exception raised when decoding a `Char` from a string
    * which does not have exactly one character.
    */
  final class InvalidChar private[vulcan] (length: Int)
    extends AvroException(s"Got unexpected String with length $length, expected length 1")

  /**
    * Exception raised when the precision of a decimal codec is not positive.
    */
  final class InvalidDecimalPrecision private[vulcan] (precision: Int)
    extends AvroException(s"Invalid decimal precision $precision: must be positive")

  /**
    * Exception raised when the scale of a decimal codec
    * is negative, or exceeds the precision.
    */
  final class InvalidDecimalScale private[vulcan] (scale: Int, precision: Int)
    extends AvroException(
      if (scale < 0) s"Invalid decimal scale $scale: cannot be negative"
      else s"Invalid decimal scale $scale: cannot exceed precision $precision"
    )

  /**
    * Exception raised when the default of an enum
    * schema is not one of its symbols.
    */
  final class InvalidEnumDefault private[vulcan] (default: String, enumeration: Schema.Enum)
    extends AvroException(s"""Default "$default" is not a symbol of enum ${enumeration.fullName}""")

  /**
    * Exception raised when encoding an enum value with a
    * symbol which is not one of the symbols of the schema.
    */
  final class InvalidEnumValue private[vulcan] (symbol: String, fullName: String)
    extends AvroException(s"Invalid symbol $symbol for enum $fullName")

  /**
    * Exception raised when the default of a record field
    * is not a valid value for the schema of the field.
    */
  final class InvalidFieldDefault private[vulcan] (
    default: Json,
    field: Schema.Field,
    record: Schema.Record
  ) extends AvroException(
      s"""Invalid default ${JsonPrinter.print(default)} for field "${field.name}" """ +
        s"with schema ${field.schema} in record ${record.fullName}"
    )

  /**
    * Exception raised when the size of a fixed schema is negative.
    */
  final class InvalidFixedSize private[vulcan] (fixed: Schema.Fixed)
    extends AvroException(s"Invalid size ${fixed.size} for fixed ${fixed.fullName}")

  /**
    * Exception raised when encoding a fixed value with a
    * number of bytes other than the size of the schema.
    */
  final class InvalidFixedValue private[vulcan] (size: Long, fullName: String, fixedSize: Long)
    extends AvroException(s"Invalid fixed with $size bytes for fixed $fullName with size $fixedSize")

  /**
    * Exception raised when decoding Avro JSON fails, because
    * the JSON is invalid for the writer schema.
    */
  final class InvalidJson private[vulcan] (details: String) extends AvroException(details)

  /**
    * Exception raised when a name, namespace, alias, field name,
    * or enum symbol of a schema is not a valid name.
    */
  final class InvalidName private[vulcan] (description: String, reason: String)
    extends AvroException(s"Invalid $description: $reason")

  /**
    * Exception raised when encoding a record value with a number
    * of values other than the number of fields of the schema.
    */
  final class InvalidRecordValue private[vulcan] (values: Int, fullName: String, fields: Int)
    extends AvroException(s"Invalid record with $values values for record $fullName with $fields fields")

  /**
    * Exception raised when parsing or decoding a schema from JSON fails, for
    * example because the JSON is invalid or a required property is missing.
    */
  final class InvalidSchema private[vulcan] (details: String, cause: Option[Throwable] = None)
    extends AvroException(details, cause)

  /**
    * Exception raised when a union value has an index
    * which is not the index of a type of the union.
    */
  final class InvalidUnionValue private[vulcan] (index: Int, types: Int)
    extends AvroException(s"Invalid index $index for union with $types types")

  /**
    * Exception raised when decoding a `UUID` from a string which is not a valid `UUID`.
    */
  final class InvalidUuid private[vulcan] (string: String)
    extends AvroException(s"Invalid UUID string: $string")

  /**
    * Exception raised when a value is not a value of the schema,
    * for example when encoding an `int` value for a `string` schema.
    */
  final class InvalidValue private[vulcan] (value: AvroValue, schema: String)
    extends AvroException(s"Invalid ${ValueNames.typeName(value)} value for $schema")

  /**
    * Exception raised when decoding with a union codec which
    * has no alternative for the type of the writer schema.
    */
  final class MissingAlternative private[vulcan] (writerSchema: Schema)
    extends AvroException(s"Missing alternative for ${SchemaNames.describe(writerSchema)} in union")

  /**
    * Exception raised when decoding a record where the writer schema
    * is missing a field of the codec, and the field has no default.
    */
  final class MissingField private[vulcan] (name: String)
    extends AvroException(s"Record writer schema is missing field '$name'")

  /**
    * Exception raised when a union schema directly contains another union schema.
    */
  final class NestedUnion private[vulcan] (union: Schema.Union)
    extends AvroException(s"Unions cannot directly contain other unions: $union")

  /**
    * Exception raised when a named schema without a
    * namespace has the name of a primitive type.
    */
  final class ReservedName private[vulcan] (name: String)
    extends AvroException(s"Named schemas cannot be named after primitive type $name")

  /**
    * Exception raised when the name of a schema or field
    * property is reserved, for example `type` or `name`.
    */
  final class ReservedProperty private[vulcan] (key: String, description: String)
    extends AvroException(s"""Invalid property "$key" of $description: name is reserved""")

  /**
    * Exception raised when a schema references a name which is not defined.
    */
  final class UndefinedName private[vulcan] (name: String) extends AvroException(s"Undefined name $name")

  /**
    * Exception raised when decoding with a fixed codec,
    * where the writer schema has a different size.
    */
  final class UnexpectedFixedSize private[vulcan] (size: Int, expectedSize: Int)
    extends AvroException(s"Got unexpected fixed size $size, expected fixed size $expectedSize")

  /**
    * Exception raised when decoding with a codec for a logical type, where the writer
    * schema has a different logical type, an invalid logical type, or no logical type.
    */
  final class UnexpectedLogicalType private[vulcan] (writerSchema: Schema)
    extends AvroException(
      (writerSchema.logicalType, writerSchema.props("logicalType").flatMap(_.asString)) match {
        case (Some(logicalType), _) =>
          s"Got unexpected logical type ${logicalType.name}"
        case (None, Some(name)) =>
          s"Got invalid logical type $name for ${SchemaNames.typeName(writerSchema)}"
        case (None, None) =>
          "Got unexpected missing logical type"
      }
    )

  /**
    * Exception raised when decoding with a codec, where the
    * writer schema has a type which the codec cannot decode.
    */
  final class UnexpectedSchemaType private[vulcan] (writerSchema: Schema, expectedType: String)
    extends AvroException(
      s"Got unexpected schema type ${SchemaNames.typeName(writerSchema)}, expected schema type $expectedType"
    )

  /**
    * Exception raised when adding a logical type to a
    * schema with a type which has no logical types.
    */
  final class UnsupportedLogicalType private[vulcan] (logicalType: LogicalType, schema: Schema)
    extends AvroException(
      s"Unable to add logical type ${logicalType.name} to ${SchemaNames.describe(schema)}"
    )

  /**
    * Exception raised when decoding a value outside the range of the
    * decoded type, for example an `Int` outside the range of `Byte`.
    */
  final class ValueOutOfRange private[vulcan] (typeName: String, value: Long, min: Long, max: Long)
    extends AvroException(s"Got unexpected $typeName value $value, expected value in range $min to $max")
}
