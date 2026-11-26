# Codecs

@:api(org.typelevel.vulcan.Codec) is the central concept in the library. @:api(org.typelevel.vulcan.Codec)s represent an Avro `schema`, together with encoders and decoders for converting between Scala types and Avro. There are default @:api(org.typelevel.vulcan.Codec)s defined for many standard library types. For example, we can check what the default `Option[Instant]` encoding is by asking for a @:api(org.typelevel.vulcan.Codec) instance.

```scala mdoc:reset-object
import java.time.Instant
import org.typelevel.vulcan.Codec

Codec[Option[Instant]]
```

In some cases, it's not possible to generate Avro schemas. This is why @:api(org.typelevel.vulcan.Codec) `schema`s are wrapped in `Either` with error type @:api(org.typelevel.vulcan.AvroException). For example, Avro doesn't support nested unions, so what would happen when we try to ask for a @:api(org.typelevel.vulcan.Codec) for `Option[Option[Instant]]`?

```scala mdoc
Codec[Option[Option[Instant]]]
```

A @:api(org.typelevel.vulcan.Codec) compiles encoders for its schema, and decoders for the schema values were written with, called the writer schema. They are compiled when requested, so request them once and reuse them for any number of values. Encoding and decoding might also be unsuccessful, so results are wrapped in `Either` with error type @:api(org.typelevel.vulcan.AvroException). For example, what happens if we try to decode `Int`s written using a `Boolean` schema?

```scala mdoc
import org.typelevel.vulcan.AvroValue
import org.typelevel.vulcan.Schema

for {
  decoder <- Codec[Int].decoder(Schema.Boolean)
  result <- decoder.decode(AvroValue.Boolean(true))
} yield result
```

@:api(org.typelevel.vulcan.Codec)s encode Scala types as `AvroValue`s, and decode `AvroValue`s using the schema they were written with. Encoded values are not checked against the schema until they are written as Avro binary or JSON, so tests should be used to ensure @:api(org.typelevel.vulcan.Codec)s work as intended. This becomes important when we define @:api(org.typelevel.vulcan.Codec)s from scratch.

@:api(org.typelevel.vulcan.Codec)s form [invariant functors][invariant], which means you can easily use an existing @:api(org.typelevel.vulcan.Codec) to encode and decode a different type, by mapping back-and-forth between the @:api(org.typelevel.vulcan.Codec)'s existing type argument and the new type. This becomes useful when dealing with newtypes, like `InstallationTime` in the following example.

```scala mdoc
final case class InstallationTime(value: Instant)

Codec[Instant].imap(InstallationTime(_))(_.value)
```

When we have a newtype where we ensure values are valid, we can use `narrow` instead.

```scala mdoc
import org.typelevel.vulcan.AvroException

sealed abstract case class SerialNumber(value: String)

object SerialNumber {
  def apply(value: String): Either[AvroException, SerialNumber] =
    if(value.length == 12 && value.forall(_.isDigit))
      Right(new SerialNumber(value) {})
    else Left(AvroException(s"$value is not a serial number"))
}

Codec[String].narrow(SerialNumber(_))(_.value)
```

When encoding might be unsuccessful instead, we can use `widen`, and when both directions might be unsuccessful, we can use `exmap`.

## Decimals

Avro decimals closely correspond to `BigDecimal`s with a fixed precision and scale.

`Codec.decimal` can be used to create a @:api(org.typelevel.vulcan.Codec) for `BigDecimal` given the precision and scale. When encoding, the @:api(org.typelevel.vulcan.Codec) checks the precision and scale of the `BigDecimal` to make sure it matches the precision and scale defined in the schema. When decoding, we check the precision is not exceeded.

```scala mdoc
Codec.decimal(precision = 10, scale = 2)
```

## Enumerations

Avro enumerations closely correspond to `sealed trait`s with `case object`s.

We can use `Codec.enumeration​` to specify an encoding.

```scala mdoc
sealed trait Fruit
case object Apple extends Fruit
case object Banana extends Fruit
case object Cherry extends Fruit

Codec.enumeration[Fruit](
  name = "Fruit",
  namespace = "com.example",
  doc = Some("A selection of different fruits"),
  symbols = List("apple", "banana", "cherry"),
  encode = {
    case Apple  => "apple"
    case Banana => "banana"
    case Cherry => "cherry"
  },
  decode = {
    case "apple"  => Right(Apple)
    case "banana" => Right(Banana)
    case "cherry" => Right(Cherry)
    case other    => Left(AvroException(s"$other is not a Fruit"))
  },
  default = Some(Banana)
)
```

Derivation for enumeration types can be partly automated using the [generic](derivation.md#enumerations) or [enumeratum](integrations/enumeratum.md) modules.

## Fixed

Avro fixed types correspond to `ByteVector`s with a fixed size.

We can use `Codec.fixed` to define a codec.

```scala mdoc
import scodec.bits.ByteVector

sealed abstract case class Pence(value: Byte)

object Pence {
  def apply(value: Byte): Either[AvroException, Pence] =
    if(0 <= value && value < 100) Right(new Pence(value) {})
    else Left(AvroException(s"Expected pence value, got $value"))
}

Codec.fixed[Pence](
  name = "Pence",
  namespace = "com.example",
  size = 1,
  encode = pence => ByteVector(pence.value),
  decode = bytes => Pence(bytes.head),
  doc = Some("Amount of pence as a single byte")
)
```

## Records

Avro records closely correspond to `case class`es.

We can use `Codec.record` to specify a record encoding.

```scala mdoc
import cats.implicits._

final case class Person(firstName: String, lastName: String, age: Option[Int])

Codec.record[Person](
  name = "Person",
  namespace = "com.example",
  doc = Some("Person with a full name and optional age")
) { field =>
  field("fullName", p => s"${p.firstName} ${p.lastName}") *>
  (
    field("firstName", _.firstName),
    field("lastName", _.lastName, doc = Some("the last name")),
    field("age", _.age, default = Some(None))
  ).mapN(Person(_, _, _))
}
```

For generic derivation support, refer to the [generic module](derivation.md#records).

## Recursive Types

Avro records can contain themselves, for example in an optional field or in an array. Record @:api(org.typelevel.vulcan.Codec)s build their fields on first use, so the @:api(org.typelevel.vulcan.Codec) of a recursive type can refer to itself, and the schema refers to the enclosing record by name.

```scala mdoc
final case class Category(name: String, subcategories: List[Category])

object Category {
  given Codec[Category] =
    Codec.record[Category](
      name = "Category",
      namespace = "com.example"
    ) { field =>
      (
        field("name", _.name),
        field("subcategories", _.subcategories, default = Some(Nil))
      ).mapN(Category(_, _))
    }
}

Codec[Category]
```

Recursive @:api(org.typelevel.vulcan.Codec)s must refer to themselves, rather than to a new @:api(org.typelevel.vulcan.Codec) for the same record, otherwise an error is raised. When the @:api(org.typelevel.vulcan.Codec) is a `def`, for example for a generic type, `Codec.recursive` can be used to pass the @:api(org.typelevel.vulcan.Codec) to itself, like in the following example.

```scala mdoc
final case class Node[A](value: A, next: Option[Node[A]])

object Node {
   given [A: Codec]: Codec[Node[A]] =
    Codec.recursive { implicit codec =>
      Codec.record[Node[A]](
        name = "Node",
        namespace = "com.example"
      ) { field =>
        (
          field("value", _.value),
          field("next", _.next)
        ).mapN(Node(_, _))
      }
    }
}

Codec[Node[Int]]
```

## Unions

Avro unions closely correspond to `sealed trait`s.

We can use `Codec.union` to specify a union encoding.

```scala mdoc
sealed trait FirstOrSecond

final case class First(value: Int) extends FirstOrSecond
object First {
  implicit val codec: Codec[First] =
    Codec[Int].imap(apply)(_.value)
}

final case class Second(value: String) extends FirstOrSecond
object Second {
  implicit val codec: Codec[Second] =
    Codec[String].imap(apply)(_.value)
}

Codec.union[FirstOrSecond] { alt =>
  alt[First] |+| alt[Second]
}
```

For generic derivation support, refer to the [generic module](derivation.md#unions).

[invariant]: https://typelevel.org/cats/typeclasses/invariant.html
