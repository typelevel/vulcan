/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.data.Chain
import cats.free.FreeApplicative
import cats.implicits.*
import io.circe.JsonObject
import magnolia1.*
import scodec.bits.ByteVector
import vulcan.internal.Names

package object generic {
  implicit final class MagnoliaCodec private[generic] (
    private val codec: Codec.type
  ) extends AnyVal {
    final def join[A](caseClass: CaseClass[Codec, A]): Codec[A] =
      if (caseClass.isValueClass) {
        val param = caseClass.parameters.head
        param.typeclass.imap(value => caseClass.rawConstruct(List(value)))(param.dereference)
      } else {

        Codec
          .record[A](
            name = caseClass.annotations
              .collectFirst { case AvroName(namespace) => namespace }
              .getOrElse(caseClass.typeName.short),
            namespace = caseClass.annotations
              .collectFirst { case AvroNamespace(namespace) => namespace }
              .getOrElse(caseClass.typeName.owner),
            doc = caseClass.annotations.collectFirst { case AvroDoc(doc) =>
              doc
            },
            aliases = caseClass.annotations.collectFirst { case AvroAlias(alias) =>
              alias
            }.toList
          ) { (f: Codec.FieldBuilder[A]) =>
            val nullDefaultBase = caseClass.annotations
              .collectFirst { case AvroNullDefault(enabled) => enabled }
              .getOrElse(false)

            caseClass.parameters.toList
              .traverse[FreeApplicative[Codec.Field[A, *], *], Any] { param =>
                def nullDefaultField =
                  param.annotations
                    .collectFirst { case AvroNullDefault(nullDefault) =>
                      nullDefault
                    }
                    .getOrElse(nullDefaultBase)

                def renamedField =
                  param.annotations
                    .collectFirst { case AvroName(newName) =>
                      newName
                    }

                def getProps =
                  param.annotations
                    .collectFirst { case AvroProps(props) =>
                      props
                    }

                implicit val codec = param.typeclass

                f(
                  name = renamedField.getOrElse(param.label),
                  access = param.dereference,
                  doc = param.annotations.collectFirst { case AvroDoc(doc) =>
                    doc
                  },
                  aliases = param.annotations.collectFirst { case AvroAlias(alias) =>
                    alias
                  }.toList,
                  default = param.default.orElse(
                    if (codec.schema.exists(_.isNullable) && nullDefaultField)
                      Some(None.asInstanceOf[param.PType]) // TODO: remove cast
                    else None
                  ),
                  props = getProps.getOrElse(JsonObject.empty)
                ).widen
              }
              .map(caseClass.rawConstruct(_))
          }
      }

    /**
      * Returns a `Codec` instance for the specified type, deriving details from the type, as long
      * as the type is a `case class` or `sealed trait`.
      */
    final def derive[A]: Codec[A] =
      macro Magnolia.gen[A]

    final def split[A](sealedTrait: SealedTrait[Codec, A]): Codec[A] = {

      Codec
        .union[A](alt =>
          Chain.fromSeq(sealedTrait.subtypes).flatMap { subtype =>
            alt(subtype.typeclass, Prism.instance(subtype.cast.lift)(identity))
          }
        )
        .withTypeName(sealedTrait.typeName.full)
    }

    final type Typeclass[A] = Codec[A]
  }

  /**
    * Returns an enum `Codec` for type `A`, deriving details like the name, namespace, and
    * [[AvroDoc]] documentation from the type `A` at compile time.
    *
    * @group Derive
    */
  final def deriveEnum[A](
    symbols: Seq[String],
    encode: A => String,
    decode: String => Either[AvroError, A]
  )(implicit names: Names[A]): Codec[A] =
    Codec.enumeration(
      name = names.typeName,
      symbols = symbols,
      encode = encode,
      decode = decode,
      namespace = names.namespace,
      doc = names.doc,
      aliases = names.aliasOf.toSeq
    )

  /**
    * Returns a fixed `Codec` for type `A`, deriving details like the name, namespace, and
    * [[AvroDoc]] documentation from the type `A` at compile time.
    *
    * @group Derive
    */
  final def deriveFixed[A](
    size: Int,
    encode: A => ByteVector,
    decode: ByteVector => Either[AvroError, A]
  )(implicit names: Names[A]): Codec[A] =
    Codec.fixed(
      name = names.typeName,
      size = size,
      encode = encode,
      decode = decode,
      namespace = names.namespace,
      doc = names.doc,
      aliases = names.aliasOf.toSeq
    )
}
