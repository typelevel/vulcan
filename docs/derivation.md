# Derivation

The `@GENERIC_MODULE_NAME@` module provides generic derivation of @:api(vulcan.Codec)s using [Magnolia](https://github.com/softwaremill/magnolia) for records and unions, and compile-time macros for enumerations and fixed types.

## Getting Started

To get started with [sbt](https://scala-sbt.org), add the following line to your `build.sbt` file.

```scala
libraryDependencies += "@ORGANIZATION@" %% "@GENERIC_MODULE_NAME@" % "@VERSION@"
```

## Enumerations

`deriveEnum` can be used to partly derive @:api(vulcan.Codec)s for enumeration types. Annotations like `@AvroDoc` can be used to customize the derivation.

```scala mdoc:reset-object
import vulcan.AvroError
import vulcan.generic._

@AvroNamespace("com.example")
@AvroDoc("A selection of different fruits")
sealed trait Fruit
case object Apple extends Fruit
case object Banana extends Fruit
case object Cherry extends Fruit

deriveEnum[Fruit](
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
  }
)
```

## Fixed

`deriveFixed` can be used to partly derive @:api(vulcan.Codec)s for fixed types. Annotations like `@AvroDoc` can be used to customize the derivation.

```scala mdoc
import scodec.bits.ByteVector
import vulcan.AvroError

sealed abstract case class Pence(value: Byte)

object Pence {
  def apply(value: Byte): Either[AvroError, Pence] =
    if(0 <= value && value < 100) Right(new Pence(value) {})
    else Left(AvroError(s"Expected pence value, got $value"))
}

deriveFixed[Pence](
  size = 1,
  encode = pence => ByteVector(pence.value),
  decode = bytes => Pence(bytes.head)
)
```

## Records

To derive @:api(vulcan.Codec)s for `case class`es or `sealed trait`s, we can use `Codec.derive`. Annotations like `@AvroDoc` and `@AvroNamespace` can be used to customize the documentation and namespace during derivation.

```scala mdoc
import vulcan.Codec
import vulcan.generic._

@AvroNamespace("com.example")
@AvroDoc("Person with a first name, last name, and optional age")
final case class Person(firstName: String, lastName: String, age: Option[Int])

Codec.derive[Person]
```

## Unions

While `case class`es correspond to Avro records, `sealed trait`s correspond to unions.

```scala mdoc
sealed trait FirstOrSecond

@AvroNamespace("com.example")
final case class First(value: Int) extends FirstOrSecond

@AvroNamespace("com.example")
final case class Second(value: String) extends FirstOrSecond

Codec.derive[FirstOrSecond]
```
