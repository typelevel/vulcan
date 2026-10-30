/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import cats.syntax.all.*
import io.circe.Json
import io.circe.JsonNumber
import io.circe.JsonObject
import scala.collection.mutable
import vulcan.AvroException
import vulcan.AvroException.ConflictingDefinitions
import vulcan.AvroException.DuplicateField
import vulcan.AvroException.DuplicateSymbol
import vulcan.AvroException.DuplicateUnionType
import vulcan.AvroException.InvalidEnumDefault
import vulcan.AvroException.InvalidFieldDefault
import vulcan.AvroException.InvalidFixedSize
import vulcan.AvroException.InvalidName
import vulcan.AvroException.NestedUnion
import vulcan.AvroException.ReservedName
import vulcan.AvroException.ReservedProperty
import vulcan.AvroException.UndefinedName
import vulcan.Schema

private[vulcan] object SchemaValidator {

  /** Validate the schema, matching the Java Avro library for compatibility. */
  def validate(schema: Schema, validateDefaults: Boolean): Either[AvroException, Unit] =
    new Validation(validateDefaults).validate(schema)

  /**
    * Returns `true` if the named schemas have the same definition, ignoring docs and aliases.
    *
    * Named schemas within the definitions are compared by name.
    */
  def equivalent(first: Schema.Named, second: Schema.Named): Boolean =
    (first eq second) || definition(first) == definition(second)

  /** The value of an integral number which fits in an `Int`, same as the Java Avro library. */
  def intValue(json: Json): Option[Int] =
    json.asNumber.filter(isIntegral).flatMap(_.toInt)

  /** The value of an integral number which fits in a `Long`, same as the Java Avro library. */
  def longValue(json: Json): Option[Long] =
    json.asNumber.filter(isIntegral).flatMap(_.toLong)

  /** Returns `true` if the number is written without fraction and exponent. */
  private def isIntegral(number: JsonNumber): Boolean =
    Json.fromJsonNumber(number).noSpaces.forall(char => char == '-' || (char >= '0' && char <= '9'))

  /** The definition without docs and aliases, with named schemas replaced by references. */
  private def definition(named: Schema.Named): Schema.Named =
    named match {
      case record: Schema.Record =>
        record
          .withDoc(None)
          .withAliases(Vector.empty)
          .withFields(record.fields.map { field =>
            field.withDoc(None).withAliases(Vector.empty).withSchema(references(field.schema))
          })

      case enumeration: Schema.Enum =>
        enumeration.withDoc(None).withAliases(Vector.empty)

      case fixed: Schema.Fixed =>
        fixed.withDoc(None).withAliases(Vector.empty)
    }

  private def references(schema: Schema): Schema =
    schema match {
      case named: Schema.Named => Schema.Ref(named.name)
      case array: Schema.Array => array.withItems(references(array.items))
      case map: Schema.Map => map.withValues(references(map.values))
      case union: Schema.Union => union.withTypes(union.types.map(references))
      case _ => schema
    }

  private final class Validation(validateDefaults: Boolean) {
    private val defined = mutable.Map.empty[String, Schema.Named]

    def validate(schema: Schema): Either[AvroException, Unit] =
      schema match {
        case named: Schema.Named =>
          validateNamed(named)

        case ref: Schema.Ref =>
          if (defined.contains(ref.fullName)) Right(())
          else Left(new UndefinedName(ref.fullName))

        case union: Schema.Union =>
          validateUnionTypes(union) >> union.types.traverse_(validate)

        case array: Schema.Array =>
          validateProps(array.props, SchemaEncoder.SchemaReserved, "array") >>
            validate(array.items)

        case map: Schema.Map =>
          validateProps(map.props, SchemaEncoder.SchemaReserved, "map") >>
            validate(map.values)

        case _ =>
          validateProps(schema.props, SchemaEncoder.SchemaReserved, SchemaNames.typeName(schema))
      }

    private def validateNamed(named: Schema.Named): Either[AvroException, Unit] =
      defined.get(named.fullName) match {
        case Some(existing) if existing eq named =>
          Right(())

        case Some(existing) =>
          if (equivalent(existing, named)) validateNested(named)
          else Left(new ConflictingDefinitions(named.fullName))

        case None =>
          validateFullName(named.name) >> {
            defined.update(named.fullName, named)

            named.aliases.traverse_ { alias =>
              val aliasName = Schema.Name(alias, named.name.namespace).name
              validateName(aliasName, s"""alias "$alias" of ${named.fullName}""")
            } >> {
              val reserved = named match {
                case _: Schema.Enum => SchemaEncoder.EnumReserved
                case _ => SchemaEncoder.SchemaReserved
              }

              validateProps(named.props, reserved, SchemaNames.describe(named))
            } >> {
              named match {
                case record: Schema.Record =>
                  validateFields(record)

                case enumeration: Schema.Enum =>
                  validateSymbols(enumeration)

                case fixed: Schema.Fixed =>
                  if (fixed.size >= 0) Right(())
                  else Left(new InvalidFixedSize(fixed))
              }
            }
          }
      }

    /** Validates the named schemas within an equivalent definition. */
    private def validateNested(named: Schema.Named): Either[AvroException, Unit] =
      named match {
        case record: Schema.Record => record.fields.traverse_(field => validate(field.schema))
        case _ => Right(())
      }

    private def validateFullName(name: Schema.Name): Either[AvroException, Unit] =
      name.namespace.toList.flatMap(_.split("\\.", -1).toList).traverse_ { part =>
        validateName(part, s"""namespace part "$part" of ${name.fullName}""")
      } >>
        validateName(name.name, s"""name "${name.name}"""") >> {
          if (name.namespace.isEmpty && SchemaNames.PrimitiveNames.contains(name.name))
            Left(new ReservedName(name.name))
          else Right(())
        }

    private def validateFields(record: Schema.Record): Either[AvroException, Unit] =
      record.fields
        .foldLeftM(Set.empty[String]) { (names, field) =>
          if (names.contains(field.name))
            Left(new DuplicateField(field.name, record))
          else {
            val description = s"""field "${field.name}" in record ${record.fullName}"""
            validateName(field.name, description) >>
              validateProps(field.props, SchemaEncoder.FieldReserved, description) >>
              validate(field.schema) >>
              validateDefault(record, field) >>
              Right(names + field.name)
          }
        }
        .void

    private def validateProps(
      props: JsonObject,
      reserved: Set[String],
      description: => String
    ): Either[AvroException, Unit] =
      props.keys.find(reserved.contains) match {
        case Some(key) =>
          Left(new ReservedProperty(key, description))

        case None =>
          Right(())
      }

    private def validateDefault(
      record: Schema.Record,
      field: Schema.Field
    ): Either[AvroException, Unit] =
      field.default match {
        case Some(default) if validateDefaults && !isValidDefault(field.schema, default) =>
          Left(new InvalidFieldDefault(default, field, record))

        case _ =>
          Right(())
      }

    private def validateSymbols(enumeration: Schema.Enum): Either[AvroException, Unit] =
      enumeration.symbols.foldLeftM(Set.empty[String]) { (symbols, symbol) =>
        if (symbols.contains(symbol))
          Left(new DuplicateSymbol(symbol, enumeration))
        else
          validateName(symbol, s"""symbol "$symbol" in enum ${enumeration.fullName}""")
            .as(symbols + symbol)
      } >> {
        enumeration.default match {
          case Some(default) if !enumeration.symbols.contains(default) =>
            Left(new InvalidEnumDefault(default, enumeration))

          case _ =>
            Right(())
        }
      }

    private def validateUnionTypes(union: Schema.Union): Either[AvroException, Unit] =
      union.types
        .foldLeftM(Set.empty[String]) { (keys, schema) =>
          schema match {
            case _: Schema.Union =>
              Left(new NestedUnion(union))

            case _ =>
              val key = SchemaNames.unionKey(schema)
              if (keys.contains(key)) Left(new DuplicateUnionType(key, union))
              else Right(keys + key)
          }
        }
        .void

    /** Returns `true` if the default is valid, where union defaults can match any type. */
    private def isValidDefault(schema: Schema, default: Json): Boolean =
      schema match {
        case _: Schema.Null => default.isNull
        case _: Schema.Boolean => default.isBoolean
        case _: Schema.Int => intValue(default).isDefined
        case _: Schema.Long => longValue(default).isDefined
        case _: Schema.Float | _: Schema.Double =>
          default.isNumber || default.asString.exists(NonFiniteNumbers.contains)
        case _: Schema.Bytes | _: Schema.String | _: Schema.Enum | _: Schema.Fixed =>
          default.isString
        case array: Schema.Array =>
          default.asArray.exists(_.forall(isValidDefault(array.items, _)))
        case map: Schema.Map =>
          default.asObject.exists(_.values.forall(isValidDefault(map.values, _)))
        case union: Schema.Union =>
          union.types.exists(isValidDefault(_, default))
        case record: Schema.Record =>
          default.asObject.exists { fields =>
            record.fields.forall { field =>
              fields(field.name).orElse(field.default).exists(isValidDefault(field.schema, _))
            }
          }
        case ref: Schema.Ref =>
          defined.get(ref.fullName).exists(isValidDefault(_, default))
      }

    private def validateName(name: String, description: => String): Either[AvroException, Unit] =
      SchemaNames.invalid(name) match {
        case Some(reason) => Left(new InvalidName(description, reason))
        case None => Right(())
      }
  }

  /** Strings used by the Java Avro library for non-finite numbers. */
  private val NonFiniteNumbers: Set[String] =
    Set("NaN", "Infinity", "-Infinity")
}
