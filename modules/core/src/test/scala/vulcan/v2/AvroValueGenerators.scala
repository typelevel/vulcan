/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import org.scalacheck.{Arbitrary, Gen}
import scala.collection.mutable
import scodec.bits.ByteVector

/** Generators for values which are valid for a schema. */
object AvroValueGenerators {

  /** Schemas together with a value which is valid for the schema. */
  def genSchemaAndValue: Gen[(Schema, AvroValue)] =
    SchemaGenerators.genSchema.flatMap(schema => genValue(schema).map((schema, _)))

  def genValue(schema: Schema): Gen[AvroValue] =
    genValue(schema, namedSchemas(schema), 0)

  /** The named schemas defined in the schema, by full name. */
  def namedSchemas(schema: Schema): collection.Map[String, Schema.Named] = {
    val definitions = mutable.Map.empty[String, Schema.Named]

    def collect(schema: Schema): Unit =
      schema match {
        case named: Schema.Named if !definitions.contains(named.fullName) =>
          definitions.update(named.fullName, named)
          named match {
            case record: Schema.Record => record.fields.foreach(field => collect(field.schema))
            case _                     => ()
          }

        case array: Schema.Array => collect(array.items)
        case map: Schema.Map     => collect(map.values)
        case union: Schema.Union => union.types.foreach(collect)
        case _                   => ()
      }

    collect(schema)
    definitions
  }

  /** Unions choose types without references beyond this depth, so values are finite. */
  private val MaxDepth: Int = 3

  private def genValue(
    schema: Schema,
    definitions: collection.Map[String, Schema.Named],
    depth: Int
  ): Gen[AvroValue] =
    schema match {
      case _: Schema.Null    => Gen.const(AvroValue.Null)
      case _: Schema.Boolean => Arbitrary.arbitrary[Boolean].map(AvroValue.Boolean(_))
      case _: Schema.Int     => Arbitrary.arbitrary[Int].map(AvroValue.Int(_))
      case _: Schema.Long    => Arbitrary.arbitrary[Long].map(AvroValue.Long(_))
      case _: Schema.Float   =>
        Arbitrary.arbitrary[Int].map(bits => AvroValue.Float(java.lang.Float.intBitsToFloat(bits)))
      case _: Schema.Double =>
        Arbitrary.arbitrary[Long].map { bits =>
          AvroValue.Double(java.lang.Double.longBitsToDouble(bits))
        }
      case _: Schema.Bytes       => genBytes(Gen.choose(0, 8)).map(AvroValue.Bytes(_))
      case _: Schema.String      => genString.map(AvroValue.String(_))
      case record: Schema.Record =>
        Gen
          .sequence[Vector[AvroValue], AvroValue] {
            record.fields.map(field => genValue(field.schema, definitions, depth + 1))
          }
          .map(AvroValue.Record(_))
      case enumeration: Schema.Enum =>
        Gen.oneOf(enumeration.symbols).map(AvroValue.Enum(_))
      case fixed: Schema.Fixed =>
        genBytes(Gen.const(fixed.size)).map(AvroValue.Fixed(_))
      case array: Schema.Array =>
        Gen
          .choose(0, 3)
          .flatMap(Gen.listOfN(_, genValue(array.items, definitions, depth + 1)))
          .map(items => AvroValue.Array(items.toVector))
      case map: Schema.Map =>
        for {
          keys <- Gen.choose(0, 3).flatMap(Gen.listOfN(_, genString)).map(_.distinct)
          values <- Gen.sequence[Vector[AvroValue], AvroValue] {
            keys.map(_ => genValue(map.values, definitions, depth + 1))
          }
        } yield AvroValue.Map(keys.toVector.zip(values))
      case union: Schema.Union =>
        val indices =
          if (depth < MaxDepth) union.types.indices
          else union.types.indices.filterNot(index => containsReference(union.types(index)))

        Gen.oneOf(indices).flatMap { index =>
          genValue(union.types(index), definitions, depth + 1).map(AvroValue.Union(index, _))
        }
      case ref: Schema.Ref =>
        genValue(definitions(ref.fullName), definitions, depth + 1)
    }

  /** Returns `true` if the schema contains a reference outside of unions. */
  private def containsReference(schema: Schema): Boolean =
    schema match {
      case _: Schema.Ref         => true
      case record: Schema.Record => record.fields.exists(field => containsReference(field.schema))
      case array: Schema.Array   => containsReference(array.items)
      case map: Schema.Map       => containsReference(map.values)
      case _                     => false
    }

  private def genBytes(size: Gen[Int]): Gen[ByteVector] =
    size
      .flatMap(Gen.listOfN(_, Arbitrary.arbitrary[Byte]))
      .map(bytes => ByteVector(bytes.toArray))

  /** Strings without unpaired surrogates, which can be encoded as UTF-8. */
  private def genString: Gen[String] =
    Gen
      .choose(0, 8)
      .flatMap { size =>
        Gen.listOfN(
          size,
          Gen.frequency(
            4 -> Gen.alphaNumChar.map(_.toInt),
            1 -> Gen.choose(0x80, 0xd7ff),
            1 -> Gen.choose(0xe000, 0x10ffff)
          )
        )
      }
      .map(_.map(codePoint => new String(Character.toChars(codePoint))).mkString)
}
