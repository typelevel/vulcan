/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan.internal

import org.typelevel.vulcan.generic.AvroAlias
import org.typelevel.vulcan.generic.AvroDoc
import org.typelevel.vulcan.generic.AvroName
import org.typelevel.vulcan.generic.AvroNamespace
import org.typelevel.vulcan.internal.Names
import scala.quoted.*

object NamesMacro {
  inline def names[A]: Names[A] = ${ namesImpl[A] }

  def namesImpl[A: Type](using Quotes): Expr[Names[A]] =
    import quotes.reflect.*
    val symbol = TypeTree.of[A].symbol

    def getAnnotValue[Ann: Type]: Option[Expr[Ann]] =
      symbol.getAnnotation(TypeRepr.of[Ann].typeSymbol).map(p => p.asExprOf[Ann])

    def toExprOpt(e: Option[Expr[String]]): Expr[Option[String]] =
      e match {
        case Some(stringExp) => '{ Some($stringExp) }
        case None => '{ Option.empty[String] }
      }

    val typeName = getAnnotValue[AvroName].map(e => '{ $e.name }).getOrElse(Expr(symbol.name))

    // Full names of objects end with $, which is not part of the namespace
    val namespace = getAnnotValue[AvroNamespace]
      .map(e => '{ $e.namespace })
      .getOrElse(Expr(symbol.owner.fullName.replace("$.", ".").stripSuffix("$")))

    val doc = toExprOpt(getAnnotValue[AvroDoc].map(e => '{ $e.doc }))

    // Annotations are in reverse source order in Scala 3
    val aliases = Expr.ofList {
      symbol.annotations.reverse
        .filter(_.tpe.typeSymbol == TypeRepr.of[AvroAlias].typeSymbol)
        .map(annotation => '{ ${ annotation.asExprOf[AvroAlias] }.alias })
    }

    '{ Names[A]($typeName, $namespace, $doc, $aliases) }

}
