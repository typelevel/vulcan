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
import scala.reflect.macros.blackbox

private[vulcan] final class NamesMacro(val c: blackbox.Context) {
  import c.universe._

  final def names[A](implicit A: c.WeakTypeTag[A]): c.Expr[Names[A]] = {
    val symbol = A.tpe.typeSymbol

    // Complete the symbol so that its annotations are available
    val _ = symbol.typeSignature

    def annotationValues[T](implicit T: TypeTag[T]): List[Tree] =
      symbol.annotations.collect {
        case annotation if annotation.tree.tpe =:= T.tpe =>
          annotation.tree.children.last.duplicate
      }

    def annotationValue[T](implicit T: TypeTag[T]): Option[Tree] =
      annotationValues[T].headOption

    def optionTree(value: Option[Tree]): Tree =
      value.fold[Tree](q"_root_.scala.None")(value => q"_root_.scala.Some($value)")

    val name = symbol.name.decodedName.toString

    val typeName =
      annotationValue[AvroName].getOrElse(q"$name")

    val namespace =
      annotationValue[AvroNamespace].getOrElse(q"${symbol.fullName.dropRight(name.length + 1)}")

    val doc =
      optionTree(annotationValue[AvroDoc])

    val aliases =
      q"_root_.scala.List(..${annotationValues[AvroAlias]})"

    c.Expr[Names[A]] {
      q"_root_.org.typelevel.vulcan.internal.Names[${A.tpe}]($typeName, $namespace, $doc, $aliases)"
    }
  }
}
