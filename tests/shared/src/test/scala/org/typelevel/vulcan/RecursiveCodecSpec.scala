/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package org.typelevel.vulcan

import cats.Defer
import cats.syntax.all.*
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.typelevel.vulcan.examples.CaseClassRecursive
import org.typelevel.vulcan.examples.SealedTraitRecursive
import weaver.Expectations
import weaver.SourceLocation

object RecursiveCodecSpec extends BaseSuite {
  final case class Tree[A](value: A, children: List[Tree[A]])

  object Tree {
    implicit def codec[A: Codec]: Codec[Tree[A]] =
      Codec.recursive { implicit codec =>
        Codec.record("Tree", "com.example") { field =>
          (field("value", _.value), field("children", _.children, default = Some(Nil))).mapN(Tree(_, _))
        }
      }

    implicit def treeArbitrary[A: Arbitrary]: Arbitrary[Tree[A]] = {
      def gen(depth: Int): Gen[Tree[A]] =
        for {
          value <- arbitrary[A]
          count <- if (depth > 0) Gen.choose(0, 3) else Gen.const(0)
          children <- Gen.listOfN(count, gen(depth - 1))
        } yield Tree(value, children)

      Arbitrary(Gen.choose(0, 3).flatMap(gen))
    }
  }

  final case class Menu(name: String, submenus: Map[String, Menu])

  object Menu {
    implicit lazy val codec: Codec[Menu] =
      Codec.record("Menu", "com.example") { field =>
        (field("name", _.name), field("submenus", _.submenus)).mapN(Menu(_, _))
      }
  }

  final case class Department(name: String, employees: List[Employee])

  object Department {
    implicit val codec: Codec[Department] =
      Codec.record("Department", "com.example") { field =>
        (field("name", _.name), field("employees", _.employees)).mapN(Department(_, _))
      }
  }

  final case class Employee(name: String, manages: Option[Department])

  object Employee {
    implicit val codec: Codec[Employee] =
      Codec.record("Employee", "com.example") { field =>
        (field("name", _.name), field("manages", _.manages)).mapN(Employee(_, _))
      }
  }

  final case class Defaults(value: Int, next: Option[Defaults], children: List[Defaults])

  object Defaults {
    implicit val codec: Codec[Defaults] =
      Codec.record("Defaults", "com.example") { field =>
        (
          field("value", _.value),
          field("next", _.next, default = Some(Some(Defaults(1, None, Nil)))),
          field("children", _.children, default = Some(List(Defaults(2, None, List(Defaults(3, None, Nil))))))
        ).mapN(Defaults(_, _, _))
      }
  }

  private def expectParsedSchema[A](implicit codec: Codec[A], loc: SourceLocation): Expectations =
    whenSuccess(codec.schema)(schema => expect.same(Right(schema), Schema.parse(schema.toString)))

  pureTest("Recursive codecs should refer to enclosing records by name in schemas") {
    expectSchema[CaseClassRecursive] {
      """{"type":"record","name":"CaseClassRecursive","namespace":"com.example","fields":[{"name":"value","type":"int"},{"name":"next","type":["null","CaseClassRecursive"],"default":null}]}"""
    } &&
    expectParsedSchema[CaseClassRecursive]
  }

  pureTest("Recursive codecs should support records which contain themselves in arrays") {
    expectSchema[Tree[Int]] {
      """{"type":"record","name":"Tree","namespace":"com.example","fields":[{"name":"value","type":"int"},{"name":"children","type":{"type":"array","items":"Tree"},"default":[]}]}"""
    } &&
    expectParsedSchema[Tree[Int]]
  }

  pureTest("Recursive codecs should support records which contain themselves in maps") {
    val menu = Menu("file", Map("open" -> Menu("open", Map.empty), "recent" -> Menu("recent", Map.empty)))

    expectSchema[Menu] {
      """{"type":"record","name":"Menu","namespace":"com.example","fields":[{"name":"name","type":"string"},{"name":"submenus","type":{"type":"map","values":"Menu"}}]}"""
    } &&
    expectParsedSchema[Menu] &&
    expectRoundtrip(menu)
  }

  pureTest("Recursive codecs should support mutually recursive records") {
    expectSchema[Department] {
      """{"type":"record","name":"Department","namespace":"com.example","fields":[{"name":"name","type":"string"},{"name":"employees","type":{"type":"array","items":{"type":"record","name":"Employee","fields":[{"name":"name","type":"string"},{"name":"manages","type":["null","Department"]}]}}}]}"""
    } &&
    expectSchema[Employee] {
      """{"type":"record","name":"Employee","namespace":"com.example","fields":[{"name":"name","type":"string"},{"name":"manages","type":["null",{"type":"record","name":"Department","fields":[{"name":"name","type":"string"},{"name":"employees","type":{"type":"array","items":"Employee"}}]}]}]}"""
    } &&
    expectParsedSchema[Department] &&
    expectParsedSchema[Employee] &&
    expectRoundtrip(Department("a", List(Employee("b", Some(Department("c", List(Employee("d", None))))))))
  }

  pureTest("Recursive codecs should define records once in unions with several recursive records") {
    expectSchema[SealedTraitRecursive] {
      """[{"type":"record","name":"Literal","namespace":"com.example","fields":[{"name":"value","type":"int"}]},{"type":"record","name":"Negate","namespace":"com.example","fields":[{"name":"expression","type":["Literal","Negate",{"type":"record","name":"Add","fields":[{"name":"left","type":["Literal","Negate","Add"]},{"name":"right","type":["Literal","Negate","Add"]}]}]}]},"com.example.Add"]"""
    } &&
    expectParsedSchema[SealedTraitRecursive]
  }

  pureTest("Recursive codecs should support defaults which contain the enclosing record") {
    val writerSchema =
      Schema.Record(Schema.Name("Defaults", Some("com.example")), Vector(Schema.Field("value", Schema.Int)))

    expectSchema[Defaults] {
      """{"type":"record","name":"Defaults","namespace":"com.example","fields":[{"name":"value","type":"int"},{"name":"next","type":["null","Defaults"],"default":{"value":1,"next":null,"children":[]}},{"name":"children","type":{"type":"array","items":"Defaults"},"default":[{"value":2,"next":null,"children":[{"value":3,"next":null,"children":[]}]}]}]}"""
    } &&
    expectParsedSchema[Defaults] &&
    expectDecode(
      Right(AvroValue.Record(Vector(AvroValue.Int(0)))),
      Right(writerSchema),
      Defaults(0, Some(Defaults(1, None, Nil)), List(Defaults(2, None, List(Defaults(3, None, Nil)))))
    )
  }

  test("Recursive codecs should roundtrip records which contain themselves") {
    forallRoundtrip[CaseClassRecursive]
  }

  test("Recursive codecs should roundtrip unions of records which contain the union") {
    forallRoundtrip[SealedTraitRecursive]
  }

  test("Recursive codecs should roundtrip records which contain themselves in arrays") {
    forallRoundtrip[Tree[String]]
  }

  pureTest("Recursive codecs should decode writer schemas without the recursive field") {
    val writerSchema =
      Schema.Record(
        Schema.Name("CaseClassRecursive", Some("com.example")),
        Vector(Schema.Field("value", Schema.Int))
      )

    expectDecode(
      Right(AvroValue.Record(Vector(AvroValue.Int(1)))),
      Right(writerSchema),
      CaseClassRecursive(1, None)
    )
  }

  pureTest("Recursive codecs should decode recursive writer schemas with an additional field") {
    val writerSchema =
      Schema.parse(
        """{"type":"record","name":"CaseClassRecursive","namespace":"com.example","fields":[{"name":"value","type":"int"},{"name":"next","type":["null","CaseClassRecursive"]},{"name":"extra","type":"string"}]}"""
      )

    val value =
      AvroValue.Record(
        Vector(
          AvroValue.Int(1),
          AvroValue.Union(
            1,
            AvroValue.Record(
              Vector(AvroValue.Int(2), AvroValue.Union(0, AvroValue.Null), AvroValue.String("b"))
            )
          ),
          AvroValue.String("a")
        )
      )

    expectDecode(Right(value), writerSchema, CaseClassRecursive(1, Some(CaseClassRecursive(2, None))))
  }

  pureTest("Recursive codecs should write values which codecs without recursion can decode") {
    implicit val codec: Codec[Int] =
      Codec.record[Int]("CaseClassRecursive", "com.example")(_("value", identity[Int])(Codec.int))

    val value = CaseClassRecursive(1, Some(CaseClassRecursive(2, None)))
    expectDecode(CaseClassRecursive.codec.encode(value), CaseClassRecursive.codec.schema, 1)
  }

  pureTest("Recursive codecs should error for a different codec for the same record within the record") {
    final case class Different(next: Option[Different])

    def different: Codec[Different] =
      Codec.record("Different", "com.example") { field =>
        field("next", _.next)(Codec.option(different)).map(Different(_))
      }

    implicit val codec: Codec[Different] =
      different

    expectSchemaError[Different] {
      "Record codec for com.example.Different contains a different codec for record com.example.Different; recursive codecs must refer to themselves, for example using Codec.recursive"
    }
  }

  pureTest("Recursive codecs should error for codecs which refer to themselves outside of records") {
    final case class Nested(values: List[Nested])

    implicit val codec: Codec[Nested] =
      Codec.recursive(codec => Codec.list(codec).imap(Nested(_))(_.values))

    expectSchemaError[Nested]("Recursive codecs can only refer to themselves within records")
  }

  pureTest("Recursive codecs should error for codecs which only defer to themselves") {
    implicit val codec: Codec[Int] =
      Defer[Codec].fix(codec => Defer[Codec].defer(codec))

    expectSchemaError[Int]("Recursive codecs can only refer to themselves within records") &&
    expectEncodeError(1, "Recursive codecs can only refer to themselves within records")
  }

  pureTest("Codec defer should use the codec, evaluated once when first used") {
    var evaluations = 0

    implicit val codec: Codec[Option[Int]] =
      Defer[Codec].defer {
        evaluations += 1
        Codec.option[Int]
      }

    expect.eql(0, evaluations) &&
    expectSchema[Option[Int]]("""["null","int"]""") &&
    expect.eql(Option(Option.empty[Int]), codec.nullValue) &&
    expectRoundtrip(Option(1)) &&
    expect.eql(1, evaluations)
  }

  pureTest("Recursive codecs should encode values nested deeper than binary decoders allow") {
    val limit = BinaryDecoder.MaxDepth
    val codec = CaseClassRecursive.codec

    def nested(depth: Int): CaseClassRecursive =
      (1 until depth).foldLeft(CaseClassRecursive(0, None))((next, _) => CaseClassRecursive(0, Some(next)))

    val decoded =
      for {
        schema <- codec.schema
        encoder <- codec.binaryEncoder
        decoder <- codec.binaryDecoder(schema)
        atLimit <- encoder.encode(nested(limit))
        beyondLimit <- encoder.encode(nested(limit + 1))
      } yield (decoder.decode(atLimit), decoder.decode(beyondLimit).left.map(_.message))

    val error =
      s"Invalid depth ${limit + 1} at position ${2 * limit}, exceeding the limit of $limit nested records, arrays and maps"

    expect.same(Right((Right(nested(limit)), Left(error))), decoded)
  }
}
