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
import scala.deriving.Mirror
import scodec.bits.ByteVector
import vulcan.internal.Names

package object generic {

  implicit final class MagnoliaCodec private[generic] (
    private val codec: Codec.type
  ) extends Derivation[Codec] {
    inline def derive[A](using Mirror.Of[A]): Codec[A] = derived[A]

    /**
      * Returns a record `Codec` for the specified `case class`, using the `Codec`s of its fields.
      */
    final def join[A](caseClass: CaseClass[Codec, A]): Codec[A] =
      Codec
        .record[A](
          name = caseClass.annotations
            .collectFirst { case AvroName(namespace) => namespace }
            .getOrElse(caseClass.typeInfo.short),
          namespace = caseClass.annotations
            .collectFirst { case AvroNamespace(namespace) => namespace }
            .getOrElse(caseClass.typeInfo.owner),
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

          caseClass.params.toList
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
                access = param.deref,
                doc = param.annotations.collectFirst { case AvroDoc(doc) =>
                  doc
                },
                aliases = param.annotations.collectFirst { case AvroAlias(alias) =>
                  alias
                }.toList,
                default = param.default.orElse(
                  Option.when(codec.schema.exists(_.isNullable) && nullDefaultField)(
                    None.asInstanceOf[param.PType] // TODO: remove cast
                  )
                ),
                props = getProps.getOrElse(JsonObject.empty)
              ).widen
            }
            .map(caseClass.rawConstruct(_))
        }

    /**
      * Returns a union `Codec` for the specified `sealed trait`, using the `Codec`s of its subtypes.
      */
    final def split[A](sealedTrait: SealedTrait[Codec, A]): Codec[A] = {
      Codec
        .union[A](alt =>
          Chain
            .fromSeq(sealedTrait.subtypes.sortBy(_.typeInfo.full))
            .flatMap { subtype =>
              alt(subtype.typeclass, Prism.instance(subtype.cast.lift)(identity))
            }
        )
        .withTypeName(sealedTrait.typeInfo.full)
    }

    final type Typeclass[A] = Codec[A]
  }

  /**
    * Returns an enum `Codec` for type `A`, deriving details like the name, namespace, and
    * [[AvroDoc]] documentation from the type `A` at compile time.
    *
    * @group Derive
    */
  inline def deriveEnum[A](
    symbols: Seq[String],
    encode: A => String,
    decode: String => Either[AvroError, A]
  )(using names: Names[A]): Codec[A] =
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
  inline def deriveFixed[A](
    size: Int,
    encode: A => ByteVector,
    decode: ByteVector => Either[AvroError, A]
  )(using names: Names[A]): Codec[A] =
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
