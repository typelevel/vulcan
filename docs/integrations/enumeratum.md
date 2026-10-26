# enumeratum

The `@ENUMERATUM_MODULE_NAME@` module provides @:api(vulcan.Codec)s for [Enumeratum](https://github.com/lloydmeta/enumeratum) enumerations.

## Getting Started

To get started with [sbt](https://scala-sbt.org), add the following line to your `build.sbt` file.

```scala
libraryDependencies += "@ORGANIZATION@" %% "@ENUMERATUM_MODULE_NAME@" % "@VERSION@"
```

## Usage

For regular `Enum`s, also mix in `VulcanEnum` to derive a @:api(vulcan.Codec) instance.

```scala mdoc:reset-object
import enumeratum.{Enum, EnumEntry, VulcanEnum}
import enumeratum.EnumEntry.Lowercase
import vulcan.Codec
import vulcan.generic.{AvroDoc, AvroNamespace}

@AvroNamespace("com.example")
@AvroDoc("The different card suits")
sealed trait Suit extends EnumEntry with Lowercase

object Suit extends Enum[Suit] with VulcanEnum[Suit] {
  case object Clubs extends Suit
  case object Diamonds extends Suit
  case object Hearts extends Suit
  case object Spades extends Suit

  val values = findValues
}

Codec[Suit]
```

Annotations like `@AvroDoc` can be used to customize the derivation.

For `ValueEnum`s, also mix in the matching `VulcanValueEnum` to derive a @:api(vulcan.Codec) instance.

```scala mdoc
import enumeratum.values.{StringEnum, StringEnumEntry, StringVulcanEnum}

@AvroNamespace("com.example")
@AvroDoc("The available colors")
sealed abstract class Color(val value: String) extends StringEnumEntry

object Color extends StringEnum[Color] with StringVulcanEnum[Color] {
  case object Red extends Color("red")
  case object Green extends Color("green")
  case object Blue extends Color("blue")

  val values = findValues
}

Codec[Color]
```

For `StringVulcanEnum`, the enumeration is encoded as an Avro enumeration. For the other `VulcanValueEnum`s (`ByteVulcanEnum`, `CharVulcanEnum`, `IntVulcanEnum`, `LongVulcanEnum`, and `ShortVulcanEnum`), the encoding relies on the encoding of the enumeration's value type (`Byte`, `Char`, `Int`, `Long`, and `Short`).

```scala mdoc
import enumeratum.values.{IntEnum, IntEnumEntry, IntVulcanEnum}

sealed abstract class Day(val value: Int) extends IntEnumEntry

object Day extends IntEnum[Day] with IntVulcanEnum[Day] {
  case object Monday extends Day(1)
  case object Tuesday extends Day(2)
  case object Wednesday extends Day(3)
  case object Thursday extends Day(4)
  case object Friday extends Day(5)
  case object Saturday extends Day(6)
  case object Sunday extends Day(7)

  val values = findValues
}

Codec[Day]
```
