/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json
import io.circe.JsonObject
import org.scalacheck.Gen

/**
  * Generators for valid schemas.
  *
  * Names of named schemas include the path to the schema, so full names are unique.
  */
object SchemaGenerators {
  private val NameStart: Seq[Char] =
    ('a' to 'z') ++ ('A' to 'Z') :+ '_'

  private val NamePart: Seq[Char] =
    NameStart ++ ('0' to '9')

  /** Characters which have to be, or might be, escaped in JSON. */
  private val SpecialChars: Seq[Char] =
    Seq(' ', '"', '\\', '/', '\b', '\t', '\n', '\f', '\r', '\u0000', '\u0001', '\u001f', '\u007f', '\u0080',
      '\u009f', '\u00e9', '\u20ac', '\u2028')

  /** Reserved property names, together with `logicalType`. */
  private val ReservedNames: Set[String] =
    Set(
      "aliases",
      "default",
      "doc",
      "fields",
      "items",
      "logicalType",
      "name",
      "namespace",
      "order",
      "size",
      "symbols",
      "type",
      "values"
    )

  private val UnionKinds: List[String] =
    List(
      "null",
      "boolean",
      "int",
      "long",
      "float",
      "double",
      "bytes",
      "string",
      "array",
      "map",
      "record",
      "enum",
      "fixed"
    )

  def genSchema: Gen[Schema] =
    Gen.frequency(
      3 -> Gen.choose(0, 3).flatMap(depth => genSchema(depth, None, "0")),
      1 -> genReferences
    )

  /** A record with a schema, followed by fields with named schemas within the schema. */
  private def genReferences: Gen[Schema] =
    for {
      schema <- Gen.choose(1, 3).flatMap(depth => genSchema(depth, None, "0"))
      named <- Gen.someOf(namedSchemas(schema))
      name <- genSchemaName(None, "r")
    } yield {
      val references =
        named.toVector.zipWithIndex.map { case (reference, index) =>
          Schema.Field(s"reference$index", reference)
        }

      Schema.Record(name, Schema.Field("schema", schema) +: references)
    }

  private def namedSchemas(schema: Schema): List[Schema.Named] =
    schema match {
      case record: Schema.Record =>
        record :: record.fields.toList.flatMap(field => namedSchemas(field.schema))
      case named: Schema.Named => List(named)
      case array: Schema.Array => namedSchemas(array.items)
      case map: Schema.Map => namedSchemas(map.values)
      case union: Schema.Union => union.types.toList.flatMap(namedSchemas)
      case _ => Nil
    }

  private def genSchema(depth: Int, namespace: Option[String], path: String): Gen[Schema] =
    if (depth <= 0) genLeaf(namespace, path)
    else
      Gen.frequency(
        3 -> genLeaf(namespace, path),
        3 -> Gen.lzy(genRecord(depth, namespace, path)),
        1 -> Gen.lzy(genArray(depth, namespace, path)),
        1 -> Gen.lzy(genMap(depth, namespace, path)),
        2 -> Gen.lzy(genUnion(depth, namespace, path))
      )

  private def genLeaf(namespace: Option[String], path: String): Gen[Schema] =
    Gen.frequency(
      6 -> genPrimitive,
      1 -> genEnum(namespace, path),
      1 -> genFixed(namespace, path)
    )

  private def genPrimitive: Gen[Schema] =
    Gen.oneOf(genNull, genBoolean, genInt, genLong, genFloat, genDouble, genBytes, genString)

  private def genNull: Gen[Schema] =
    genProps.map(Schema.Null.withProps)

  private def genBoolean: Gen[Schema] =
    genProps.map(Schema.Boolean.withProps)

  private def genInt: Gen[Schema] =
    for {
      props <- genProps
      logicalType <- genOptional(
        Gen.oneOf(LogicalType.date, LogicalType.timeMillis, LogicalType("custom"))
      )
    } yield {
      val schema = Schema.Int.withProps(props)
      logicalType.fold(schema)(schema.withLogicalType)
    }

  private def genLong: Gen[Schema] =
    for {
      props <- genProps
      logicalType <- genOptional(
        Gen.oneOf(
          LogicalType.timeMicros,
          LogicalType.timestampMillis,
          LogicalType.timestampMicros,
          LogicalType.timestampNanos,
          LogicalType.localTimestampMillis,
          LogicalType.localTimestampMicros,
          LogicalType.localTimestampNanos
        )
      )
    } yield {
      val schema = Schema.Long.withProps(props)
      logicalType.fold(schema)(schema.withLogicalType)
    }

  private def genFloat: Gen[Schema] =
    genProps.map(Schema.Float.withProps)

  private def genDouble: Gen[Schema] =
    genProps.map(Schema.Double.withProps)

  private def genBytes: Gen[Schema] =
    for {
      props <- genProps
      logicalType <- genOptional(Gen.oneOf(genDecimal, Gen.const(LogicalType.bigDecimal)))
    } yield {
      val schema = Schema.Bytes.withProps(props)
      logicalType.fold(schema)(schema.withLogicalType)
    }

  private def genString: Gen[Schema] =
    for {
      props <- genProps
      logicalType <- genOptional(Gen.const(LogicalType.uuid))
    } yield {
      val schema = Schema.String.withProps(props)
      logicalType.fold(schema)(schema.withLogicalType)
    }

  private def genDecimal: Gen[LogicalType] =
    for {
      precision <- Gen.choose(1, 30)
      scale <- Gen.choose(0, precision)
    } yield LogicalType.decimal(precision, scale)

  private def genEnum(namespace: Option[String], path: String): Gen[Schema] =
    for {
      name <- genSchemaName(namespace, path)
      symbols <- Gen.choose(1, 4).flatMap(Gen.listOfN(_, genName)).map(_.distinct.toVector)
      default <- Gen.option(Gen.oneOf(symbols))
      doc <- Gen.option(genText)
      aliases <- genAliases
      props <- genProps
    } yield Schema
      .Enum(name, symbols)
      .withDefault(default)
      .withDoc(doc)
      .withAliases(aliases)
      .withProps(props)

  private def genFixed(namespace: Option[String], path: String): Gen[Schema] =
    for {
      name <- genSchemaName(namespace, path)
      size <- Gen.choose(0, 16)
      doc <- Gen.option(genText)
      aliases <- genAliases
      props <- genProps
      logicalType <- genOptional(
        Gen.oneOf(genDecimal, Gen.const(LogicalType.uuid), Gen.const(LogicalType.duration))
      )
    } yield {
      val schema = Schema.Fixed(name, size).withDoc(doc).withAliases(aliases).withProps(props)
      logicalType.fold(schema)(schema.withLogicalType)
    }

  private def genRecord(depth: Int, namespace: Option[String], path: String): Gen[Schema] =
    for {
      name <- genSchemaName(namespace, path)
      fieldNames <- Gen.choose(0, 3).flatMap(Gen.listOfN(_, genName)).map(_.distinct)
      fields <- Gen.sequence[List[Schema.Field], Schema.Field] {
        fieldNames.zipWithIndex.map { case (fieldName, index) =>
          genField(fieldName, depth - 1, name, s"${path}_$index")
        }
      }
      doc <- Gen.option(genText)
      aliases <- genAliases
      props <- genProps
    } yield Schema
      .Record(name, fields.toVector)
      .withDoc(doc)
      .withAliases(aliases)
      .withProps(props)

  private def genField(
    name: String,
    depth: Int,
    record: Schema.Name,
    path: String
  ): Gen[Schema.Field] =
    for {
      schema <- Gen.frequency(
        8 -> genSchema(depth, record.namespace, path),
        1 -> Gen.const(Schema.Union(Vector(Schema.Null, Schema.Ref(record))))
      )
      doc <- Gen.option(genText)
      default <- Gen.frequency(2 -> Gen.const(None), 1 -> genDefault(schema))
      order <- Gen.oneOf(
        Schema.Field.Order.Ascending,
        Schema.Field.Order.Descending,
        Schema.Field.Order.Ignore
      )
      aliases <- Gen.choose(0, 2).flatMap(Gen.listOfN(_, genName)).map(_.toVector)
      props <- genProps
    } yield Schema
      .Field(name, schema)
      .withDoc(doc)
      .withDefault(default)
      .withOrder(order)
      .withAliases(aliases)
      .withProps(props)

  private def genArray(depth: Int, namespace: Option[String], path: String): Gen[Schema] =
    for {
      items <- genSchema(depth - 1, namespace, s"${path}a")
      props <- genProps
    } yield Schema.Array(items).withProps(props)

  private def genMap(depth: Int, namespace: Option[String], path: String): Gen[Schema] =
    for {
      values <- genSchema(depth - 1, namespace, s"${path}m")
      props <- genProps
    } yield Schema.Map(values).withProps(props)

  /** Unions with types of different kinds, without nested unions. */
  private def genUnion(depth: Int, namespace: Option[String], path: String): Gen[Schema] =
    for {
      count <- Gen.choose(1, 4)
      kinds <- Gen.pick(count, UnionKinds)
      types <- Gen.sequence[List[Schema], Schema] {
        kinds.toList.zipWithIndex.map { case (kind, index) =>
          val typePath = s"${path}u$index"
          kind match {
            case "null" => genNull
            case "boolean" => genBoolean
            case "int" => genInt
            case "long" => genLong
            case "float" => genFloat
            case "double" => genDouble
            case "bytes" => genBytes
            case "string" => genString
            case "array" => genArray(depth, namespace, typePath)
            case "map" => genMap(depth, namespace, typePath)
            case "record" => genRecord(depth - 1, namespace, typePath)
            case "enum" => genEnum(namespace, typePath)
            case _ => genFixed(namespace, typePath)
          }
        }
      }
    } yield Schema.Union(types.toVector)

  /** A default which is valid for the schema, using the first type of unions. */
  private def genDefault(schema: Schema): Gen[Option[Json]] =
    schema match {
      case _: Schema.Null => Gen.const(Some(Json.Null))
      case _: Schema.Boolean => Gen.oneOf(Json.True, Json.False).map(Some(_))
      case _: Schema.Int => Gen.choose(Int.MinValue, Int.MaxValue).map(n => Some(Json.fromInt(n)))
      case _: Schema.Long =>
        Gen.choose(Long.MinValue, Long.MaxValue).map(n => Some(Json.fromLong(n)))
      case _: Schema.Float | _: Schema.Double =>
        Gen.choose(-1e12, 1e12).map(Json.fromDouble)
      case _: Schema.Bytes | _: Schema.String | _: Schema.Fixed =>
        genText.map(text => Some(Json.fromString(text)))
      case enumeration: Schema.Enum =>
        Gen.oneOf(enumeration.symbols).map(symbol => Some(Json.fromString(symbol)))
      case _: Schema.Array =>
        Gen.const(Some(Json.arr()))
      case _: Schema.Map =>
        Gen.const(Some(Json.obj()))
      case union: Schema.Union =>
        union.types.headOption.fold(Gen.const(Option.empty[Json]))(genDefault)
      case _: Schema.Record | _: Schema.Ref =>
        Gen.const(None)
    }

  private def genSchemaName(namespace: Option[String], path: String): Gen[Schema.Name] =
    for {
      name <- genName
      space <- Gen.frequency(3 -> Gen.const(namespace), 1 -> Gen.const(None), 2 -> genNamespace)
    } yield Schema.Name(s"${name}_$path", space)

  private def genAliases: Gen[Vector[String]] =
    Gen
      .choose(0, 2)
      .flatMap(Gen.listOfN(_, Gen.oneOf(genName, genFullName)))
      .map(_.toVector)

  private def genFullName: Gen[String] =
    for {
      namespace <- genNamespace
      name <- genName
    } yield namespace.fold(name)(namespace => s"$namespace.$name")

  private def genNamespace: Gen[Option[String]] =
    Gen.option(Gen.choose(1, 3).flatMap(Gen.listOfN(_, genName)).map(_.mkString(".")))

  private def genName: Gen[String] =
    for {
      head <- Gen.oneOf(NameStart)
      size <- Gen.choose(0, 4)
      tail <- Gen.listOfN(size, Gen.oneOf(NamePart))
    } yield (head :: tail).mkString

  private def genProps: Gen[JsonObject] =
    Gen.frequency(
      3 -> Gen.const(JsonObject.empty),
      1 -> Gen
        .choose(1, 2)
        .flatMap(Gen.listOfN(_, Gen.zip(genPropKey, genJson)))
        .map(JsonObject.fromIterable)
    )

  private def genPropKey: Gen[String] =
    genName.map(name => if (ReservedNames.contains(name)) s"${name}_" else name)

  private def genJson: Gen[Json] =
    Gen.oneOf(
      Gen.const(Json.Null),
      Gen.oneOf(Json.True, Json.False),
      Gen.choose(Int.MinValue, Int.MaxValue).map(Json.fromInt),
      Gen.choose(Long.MinValue, Long.MaxValue).map(Json.fromLong),
      Gen.choose(-1e12, 1e12).map(Json.fromDoubleOrNull),
      genText.map(Json.fromString),
      Gen.listOfN(2, genText).map(texts => Json.fromValues(texts.map(Json.fromString))),
      Gen.zip(genPropKey, genText).map { case (key, text) =>
        Json.obj(key -> Json.fromString(text))
      }
    )

  private def genText: Gen[String] =
    Gen
      .choose(0, 8)
      .flatMap(Gen.listOfN(_, Gen.frequency(4 -> Gen.alphaNumChar, 1 -> Gen.oneOf(SpecialChars))))
      .map(_.mkString)

  private def genOptional[A](gen: Gen[A]): Gen[Option[A]] =
    Gen.frequency(2 -> Gen.const(None), 1 -> gen.map(Some(_)))
}
