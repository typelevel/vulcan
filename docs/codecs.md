# Codecs

@:api(vulcan.Codec) is the central concept in the library. @:api(vulcan.Codec)s represent an Avro `schema`, together with an `encode` and `decode` function for converting between Scala types and types recognized by the Apache Avro library. There are default @:api(vulcan.Codec)s defined for many standard library types. For example, we can check what the default `Option[Instant]` encoding is by asking for a @:api(vulcan.Codec) instance.

```scala mdoc:reset-object
import java.time.Instant
import vulcan.Codec

Codec[Option[Instant]]
```

In some cases, it's not possible to generate Avro schemas. This is why @:api(vulcan.Codec) `schema`s are wrapped in `Either` with error type @:api(vulcan.AvroError). For example, Avro doesn't support nested unions, so what would happen when we try to ask for a @:api(vulcan.Codec) for `Option[Option[Instant]]`?

```scala mdoc
Codec[Option[Option[Instant]]]
```

Encoding and decoding with a @:api(vulcan.Codec) might also be unsuccessful, so results are wrapped in `Either` with error type @:api(vulcan.AvroError). Encoding accepts a value to be encoded according to the schema defined by the @:api(vulcan.Codec), and decoding accepts a value and a schema to decode the value against. For example, what happens if we try to decode `Int`s using a `Boolean` schema?

```scala mdoc
import vulcan.AvroValue
import vulcan.Schema

Codec[Int].decode(AvroValue.Int(10), Schema.Boolean)
```

@:api(vulcan.Codec)s encode Scala types as `AvroValue`s, and decode `AvroValue`s using the schema they were written with. Encoded values are not checked against the schema until they are written as Avro binary or JSON, so tests should be used to ensure @:api(vulcan.Codec)s work as intended. This becomes important when we define @:api(vulcan.Codec)s from scratch.

@:api(vulcan.Codec)s form [invariant functors][invariant], which means you can easily use an existing @:api(vulcan.Codec) to encode and decode a different type, by mapping back-and-forth between the @:api(vulcan.Codec)'s existing type argument and the new type. This becomes useful when dealing with newtypes, like `InstallationTime` in the following example.

```scala mdoc
final case class InstallationTime(value: Instant)

Codec[Instant].imap(InstallationTime(_))(_.value)
```

When we have a newtype where we ensure values are valid, we can use `imapError` instead.

```scala mdoc
import vulcan.AvroError

sealed abstract case class SerialNumber(value: String)

object SerialNumber {
  def apply(value: String): Either[AvroError, SerialNumber] =
    if(value.length == 12 && value.forall(_.isDigit))
      Right(new SerialNumber(value) {})
    else Left(AvroError(s"$value is not a serial number"))
}

Codec[String].imapError(SerialNumber(_))(_.value)
```

## Decimals

Avro decimals closely correspond to `BigDecimal`s with a fixed precision and scale.

`Codec.decimal` can be used to create a @:api(vulcan.Codec) for `BigDecimal` given the precision and scale. When encoding, the @:api(vulcan.Codec) checks the precision and scale of the `BigDecimal` to make sure it matches the precision and scale defined in the schema. When decoding, we check the precision is not exceeded.

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
    case other    => Left(AvroError(s"$other is not a Fruit"))
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
  def apply(value: Byte): Either[AvroError, Pence] =
    if(0 <= value && value < 100) Right(new Pence(value) {})
    else Left(AvroError(s"Expected pence value, got $value"))
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
