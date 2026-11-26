/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.internal

import org.typelevel.vulcan.AvroException
import org.typelevel.vulcan.AvroValue
import org.typelevel.vulcan.Codec
import org.typelevel.vulcan.Schema
import scala.collection.mutable

/**
  * The named schemas defined in a writer schema, for resolving references to them.
  * Also keeps resolved decoders, so each codec resolves each named writer schema once.
  */
private[vulcan] final class Definitions(writerSchema: Schema) {
  private lazy val definitions: collection.Map[String, Schema.Named] =
    SchemaNames.definitions(writerSchema)

  private val decoders: mutable.Map[(Codec[_], String), Definitions.Reference[_]] =
    mutable.Map.empty

  def get(fullName: String): Option[Schema.Named] =
    definitions.get(fullName)

  /**
    * Returns the decoder of the codec for the named writer schema, resolving it
    * the first time, where recursive references to the decoder being resolved
    * forward to it once resolved.
    */
  def decoder[A](codec: Codec[A], named: Schema.Named)(
    resolve: => AvroValue => Either[AvroException, A]
  ): AvroValue => Either[AvroException, A] =
    decoders.get((codec, named.fullName)) match {
      case Some(reference) =>
        reference.asInstanceOf[Definitions.Reference[A]].resolved

      case None =>
        val reference = new Definitions.Reference[A]
        decoders.update((codec, named.fullName), reference)
        val decoder = resolve
        reference.decoder = decoder
        decoder
    }

  /**
    * Returns the specified [[Schema]] but with the first reference to
    * each named schema defined outside of it replaced by its definition.
    */
  def expanded(schema: Schema): Schema = {
    val defined = mutable.Set.empty[String]
    val records = mutable.Map.empty[String, Schema.Record]

    def expand(schema: Schema): Schema =
      schema match {
        case record: Schema.Record =>
          records.get(record.fullName) match {
            case Some(expanded) =>
              expanded

            case None =>
              defined += record.fullName
              val fields = record.fields.map(field => field.withSchema(expand(field.schema)))
              val expanded = record.withFields(fields)
              records.update(record.fullName, expanded)
              expanded
          }

        case named: Schema.Named =>
          defined += named.fullName
          named

        case array: Schema.Array => array.withItems(expand(array.items))
        case map: Schema.Map => map.withValues(expand(map.values))
        case union: Schema.Union => union.withTypes(union.types.map(expand))
        case ref: Schema.Ref if !defined.contains(ref.fullName) => get(ref.fullName).fold(schema)(expand)
        case _ => schema
      }

    expand(schema)
  }
}

private object Definitions {

  /** A decoder, which is being resolved if references are recursive. */
  private final class Reference[A] extends (AvroValue => Either[AvroException, A]) {
    var decoder: AvroValue => Either[AvroException, A] =
      null

    def resolved: AvroValue => Either[AvroException, A] =
      if (decoder != null) decoder else this

    override def apply(value: AvroValue): Either[AvroException, A] =
      decoder(value)
  }
}
