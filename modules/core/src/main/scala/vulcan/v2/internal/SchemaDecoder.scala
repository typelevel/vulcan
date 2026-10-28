/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.internal

import cats.syntax.all.*
import io.circe.Json
import io.circe.JsonObject
import scala.collection.mutable
import scala.util.Try
import vulcan.AvroError
import vulcan.v2.Schema

private[v2] object SchemaDecoder {

  /**
    * Decode `Schema` from `Json`, matching the Java Avro library for compatibility.
    *
    * Names are resolved in document order, the same way as the Java Avro library, and named schemas
    * can be referenced before they are defined. Named schemas are then defined where they first
    * occur, with references to enclosing named schemas as [[Schema.Ref]].
    *
    * Field defaults are not validated, since schemas in schema registries can predate validation.
    */
  def decode(json: Json): Either[AvroError, Schema] = {
    val decoding = new Decoding(definedNames(json))

    decoding.decode(json, None).flatMap { decoded =>
      val schema = new Resolution(decoding.definitions).resolve(decoded, Set.empty)
      SchemaValidator.validate(schema, validateDefaults = false).as(schema)
    }
  }

  /** Returns the names of all named schemas defined in the schema, by full name. */
  private def definedNames(json: Json): Map[String, Schema.Name] = {
    val names = mutable.Map.empty[String, Schema.Name]

    def collect(json: Json, namespace: Option[String]): Unit =
      json.arrayOrObject(
        (),
        _.foreach(collect(_, namespace)),
        obj =>
          obj("type").flatMap(_.asString) match {
            case Some(kind @ ("record" | "enum" | "fixed")) =>
              decodeName(obj, namespace).foreach { name =>
                if (!names.contains(name.fullName))
                  names.update(name.fullName, name)

                if (kind == "record")
                  for {
                    fields <- obj("fields").flatMap(_.asArray)
                    field <- fields
                    schema <- field.asObject.flatMap(_("type"))
                  } collect(schema, name.namespace)
              }

            case Some("array") =>
              obj("items").foreach(collect(_, namespace))

            case Some("map") =>
              obj("values").foreach(collect(_, namespace))

            case _ =>
              ()
          }
      )

    collect(json, None)
    names.toMap
  }

  /** Decodes schemas in document order, with references to named schemas as [[Schema.Ref]]. */
  private final class Decoding(names: Map[String, Schema.Name]) {

    /** The first definition of every named schema, added once the definition has been decoded. */
    val definitions: mutable.Map[String, Schema.Named] =
      mutable.Map.empty

    def decode(json: Json, namespace: Option[String]): Either[AvroError, Schema] =
      json.fold(
        invalidSchema(json),
        _ => invalidSchema(json),
        _ => invalidSchema(json),
        name => decodeReference(name, namespace),
        types => types.traverse(decode(_, namespace)).map(Schema.Union(_)),
        obj => decodeObject(obj, namespace)
      )

    private def decodeObject(
      obj: JsonObject,
      namespace: Option[String]
    ): Either[AvroError, Schema] =
      obj("type").flatMap(_.asString) match {
        case Some("record") =>
          decodeRecord(obj, namespace)

        case Some("enum") =>
          decodeEnum(obj, namespace)

        case Some("fixed") =>
          decodeFixed(obj, namespace)

        case Some("array") =>
          obj("items")
            .toRight(AvroError(s"Missing items in array ${print(obj)}"))
            .flatMap(decode(_, namespace))
            .map(Schema.Array(_).withProps(props(obj, SchemaEncoder.SchemaReserved)))

        case Some("map") =>
          obj("values")
            .toRight(AvroError(s"Missing values in map ${print(obj)}"))
            .flatMap(decode(_, namespace))
            .map(Schema.Map(_).withProps(props(obj, SchemaEncoder.SchemaReserved)))

        case Some("error") =>
          Left(AvroError(s"Unsupported type error in schema ${print(obj)}"))

        // Same as earlier versions of the Java Avro library, named schemas can be referenced by type
        case Some(name) =>
          primitive(name, props(obj, SchemaEncoder.SchemaReserved)) match {
            case Some(schema) => Right(schema)
            case None => decodeReference(name, namespace)
          }

        case None =>
          Left(AvroError(s"Missing type in schema ${print(obj)}"))
      }

    /**
      * Resolves the name the same way as the Java Avro library: relative to the namespace, then in
      * the null namespace, preferring named schemas defined before the reference.
      */
    private def decodeReference(
      name: String,
      namespace: Option[String]
    ): Either[AvroError, Schema] =
      primitive(name, JsonObject.empty) match {
        case Some(schema) =>
          Right(schema)

        case None =>
          val fullName =
            if (name.contains('.')) name
            else namespace.fold(name)(namespace => s"$namespace.$name")

          definitions
            .get(fullName)
            .orElse(definitions.get(name))
            .map(_.name)
            .orElse(names.get(fullName))
            .orElse(names.get(name))
            .map(Schema.Ref(_))
            .toRight(AvroError(s"Undefined name $name"))
      }

    private def decodeRecord(
      obj: JsonObject,
      namespace: Option[String]
    ): Either[AvroError, Schema] =
      for {
        name <- decodeName(obj, namespace)
        fieldsJson <- obj("fields")
          .flatMap(_.asArray)
          .toRight(AvroError(s"Missing fields in record ${print(obj)}"))
        fields <- fieldsJson.traverse(decodeField(_, name.namespace))
        aliases <- decodeAliases(obj)
        record <- define {
          Schema
            .Record(name, fields)
            .withDoc(doc(obj))
            .withAliases(aliases)
            .withProps(props(obj, SchemaEncoder.SchemaReserved))
        }
      } yield record

    private def decodeField(
      json: Json,
      namespace: Option[String]
    ): Either[AvroError, Schema.Field] =
      json.asObject.toRight(AvroError(s"Invalid field ${print(json)}")).flatMap { obj =>
        for {
          name <- obj("name")
            .flatMap(_.asString)
            .toRight(AvroError(s"Missing name in field ${print(obj)}"))
          schemaJson <- obj("type").toRight(AvroError(s"Missing type in field ${print(obj)}"))
          schema <- decode(schemaJson, namespace)
          order <- decodeOrder(obj)
          aliases <- decodeAliases(obj)
        } yield Schema
          .Field(name, schema)
          .withDoc(doc(obj))
          .withDefault(obj("default").map(decodeDefault(schema, _)))
          .withOrder(order)
          .withAliases(aliases)
          .withProps(props(obj, SchemaEncoder.FieldReserved))
      }

    private def decodeEnum(obj: JsonObject, namespace: Option[String]): Either[AvroError, Schema] =
      for {
        name <- decodeName(obj, namespace)
        symbolsJson <- obj("symbols")
          .flatMap(_.asArray)
          .toRight(AvroError(s"Missing symbols in enum ${print(obj)}"))
        symbols <- symbolsJson.traverse { symbol =>
          symbol.asString.toRight(
            AvroError(s"Invalid symbol ${print(symbol)} in enum ${print(obj)}")
          )
        }
        aliases <- decodeAliases(obj)
        enumeration <- define {
          Schema
            .Enum(name, symbols)
            .withDefault(obj("default").flatMap(_.asString))
            .withDoc(doc(obj))
            .withAliases(aliases)
            .withProps(props(obj, SchemaEncoder.EnumReserved))
        }
      } yield enumeration

    private def decodeFixed(obj: JsonObject, namespace: Option[String]): Either[AvroError, Schema] =
      for {
        name <- decodeName(obj, namespace)
        size <- obj("size")
          .flatMap(SchemaValidator.intValue)
          .toRight(AvroError(s"Missing or invalid size in fixed ${print(obj)}"))
        aliases <- decodeAliases(obj)
        fixed <- define {
          Schema
            .Fixed(name, size)
            .withDoc(doc(obj))
            .withAliases(aliases)
            .withProps(props(obj, SchemaEncoder.SchemaReserved))
        }
      } yield fixed

    /** Defines the named schema, unless there is an equivalent definition already. */
    private def define(named: Schema.Named): Either[AvroError, Schema.Named] =
      definitions.get(named.fullName) match {
        case Some(existing) if !SchemaValidator.equivalent(existing, named) =>
          Left(AvroError(s"Conflicting definitions of named schema ${named.fullName}"))

        case Some(_) =>
          Right(named)

        case None =>
          definitions.update(named.fullName, named)
          Right(named)
      }
  }

  /**
    * Defines named schemas where they first occur, with the same definition for later occurrences,
    * except for references to enclosing named schemas.
    */
  private final class Resolution(definitions: collection.Map[String, Schema.Named]) {
    private val resolved = mutable.Map.empty[String, Schema.Named]

    def resolve(schema: Schema, enclosing: Set[String]): Schema =
      schema match {
        case named: Schema.Named =>
          resolveNamed(named, enclosing)

        case ref: Schema.Ref =>
          definitions.get(ref.fullName).fold[Schema](ref)(resolveNamed(_, enclosing))

        case array: Schema.Array =>
          array.withItems(resolve(array.items, enclosing))

        case map: Schema.Map =>
          map.withValues(resolve(map.values, enclosing))

        case union: Schema.Union =>
          union.withTypes(union.types.map(resolve(_, enclosing)))

        case _ =>
          schema
      }

    private def resolveNamed(named: Schema.Named, enclosing: Set[String]): Schema =
      if (enclosing.contains(named.fullName)) Schema.Ref(named.name)
      else resolved.getOrElse(named.fullName, resolveDefinition(named, enclosing))

    private def resolveDefinition(named: Schema.Named, enclosing: Set[String]): Schema.Named = {
      val definition =
        named match {
          case record: Schema.Record =>
            record.withFields(record.fields.map { field =>
              field.withSchema(resolve(field.schema, enclosing + record.fullName))
            })

          case _ =>
            named
        }

      resolved.update(named.fullName, definition)
      definition
    }
  }

  private def decodeName(
    obj: JsonObject,
    namespace: Option[String]
  ): Either[AvroError, Schema.Name] =
    obj("name")
      .flatMap(_.asString)
      .map(SchemaNames.resolve(_, obj("namespace").flatMap(_.asString).orElse(namespace)))
      .toRight(AvroError(s"Missing name in schema ${print(obj)}"))

  private def primitive(name: String, props: JsonObject): Option[Schema] =
    name match {
      case "null" => Some(Schema.Null.withProps(props))
      case "boolean" => Some(Schema.Boolean.withProps(props))
      case "int" => Some(Schema.Int.withProps(props))
      case "long" => Some(Schema.Long.withProps(props))
      case "float" => Some(Schema.Float.withProps(props))
      case "double" => Some(Schema.Double.withProps(props))
      case "bytes" => Some(Schema.Bytes.withProps(props))
      case "string" => Some(Schema.String.withProps(props))
      case _ => None
    }

  private def decodeOrder(obj: JsonObject): Either[AvroError, Schema.Field.Order] =
    obj("order") match {
      case None =>
        Right(Schema.Field.Order.Ascending)

      case Some(order) =>
        order.asString match {
          case Some(text) if text.equalsIgnoreCase("ascending") =>
            Right(Schema.Field.Order.Ascending)

          case Some(text) if text.equalsIgnoreCase("descending") =>
            Right(Schema.Field.Order.Descending)

          case Some(text) if text.equalsIgnoreCase("ignore") =>
            Right(Schema.Field.Order.Ignore)

          case _ =>
            Left(AvroError(s"Invalid order in field ${print(obj)}"))
        }
    }

  private def decodeAliases(obj: JsonObject): Either[AvroError, Vector[String]] =
    obj("aliases") match {
      case None =>
        Right(Vector.empty)

      case Some(aliases) =>
        aliases.asArray
          .flatMap(_.traverse(_.asString))
          .map(_.distinct)
          .toRight(AvroError(s"Invalid aliases in ${print(obj)}"))
    }

  /** Same as the Java Avro library, converts text defaults of floats and doubles to numbers. */
  private def decodeDefault(schema: Schema, default: Json): Json =
    schema match {
      case _: Schema.Float | _: Schema.Double =>
        default.asString.flatMap(text => Try(java.lang.Double.parseDouble(text)).toOption) match {
          case Some(double) => Json.fromDouble(double).getOrElse(Json.fromString(double.toString))
          case None => default
        }

      case _ =>
        default
    }

  private def doc(obj: JsonObject): Option[String] =
    obj("doc").flatMap(_.asString)

  private def props(obj: JsonObject, reserved: Set[String]): JsonObject =
    JsonObject.fromIterable(obj.toIterable.filterNot { case (key, _) => reserved.contains(key) })

  private def invalidSchema(json: Json): Either[AvroError, Schema] =
    Left(AvroError(s"Invalid schema ${print(json)}"))

  private def print(json: Json): String =
    JsonPrinter.print(json)

  private def print(obj: JsonObject): String =
    JsonPrinter.print(Json.fromJsonObject(obj))
}
