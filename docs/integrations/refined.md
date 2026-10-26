# refined

The `@REFINED_MODULE_NAME@` module provides @:api(vulcan.Codec)s for [refined](https://github.com/fthomas/refined) refinement types.

## Getting Started

To get started with [sbt](https://scala-sbt.org), add the following line to your `build.sbt` file.

```scala
libraryDependencies += "@ORGANIZATION@" %% "@REFINED_MODULE_NAME@" % "@VERSION@"
```

## Usage

Refinement types are encoded using their base type (e.g. `Int` for `PosInt`). When decoding, @:api(vulcan.Codec)s check to ensure values conform to the predicate of the refinement type (e.g. `Positive` for `PosInt`), and raise an error for values which do not conform.

```scala mdoc:reset-object
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosInt
import vulcan.Codec
import vulcan.refined._

Codec[PosInt]

Codec.encode[PosInt](1)

Codec.decode[PosInt](0)
```
