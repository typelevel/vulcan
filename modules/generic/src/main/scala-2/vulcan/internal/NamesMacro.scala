/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import scala.reflect.macros.blackbox
import vulcan.generic.{AvroAlias, AvroDoc, AvroName, AvroNamespace}

private[vulcan] final class NamesMacro(val c: blackbox.Context) {
  import c.universe._

  final def names[A](implicit A: c.WeakTypeTag[A]): c.Expr[Names[A]] = {
    val symbol = A.tpe.typeSymbol

    // Complete the symbol so that its annotations are available
    val _ = symbol.typeSignature

    def annotationValue[T](implicit T: TypeTag[T]): Option[Tree] =
      symbol.annotations.collectFirst {
        case annotation if annotation.tree.tpe =:= T.tpe =>
          annotation.tree.children.last.duplicate
      }

    def optionTree(value: Option[Tree]): Tree =
      value.fold[Tree](q"_root_.scala.None")(value => q"_root_.scala.Some($value)")

    val name = symbol.name.decodedName.toString

    val typeName =
      annotationValue[AvroName].getOrElse(q"$name")

    val namespace =
      annotationValue[AvroNamespace].getOrElse(q"${symbol.fullName.dropRight(name.length + 1)}")

    val doc =
      optionTree(annotationValue[AvroDoc])

    val aliasOf =
      optionTree(annotationValue[AvroAlias])

    c.Expr[Names[A]] {
      q"_root_.vulcan.internal.Names[${A.tpe}]($typeName, $namespace, $doc, $aliasOf)"
    }
  }
}
