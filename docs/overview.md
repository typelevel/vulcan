# Overview

Vulcan provides Avro schemas, encoders, and decoders for Scala types. It's written in pure Scala, without the official Apache Avro library, and works on the JVM, Scala.js, and Scala Native. The aims are reduced boilerplate code and improved type safety. In particular, the following features are supported.

- Schemas, encoders, and decoders for many standard library types.

- Ability to easily create schemas, encoders, and decoders for custom types.

- Derivation of schemas, encoders, and decoders for `case class`es and `sealed trait`s.

- Avro binary and JSON encodings, compatible with the official Apache Avro library.

- Decoding of values written with a different schema, following Avro's schema resolution.

Documentation is kept up-to-date, currently documenting v@VERSION@ on Scala @SCALA_DOCS_VERSION@.

## Getting Started

To get started with [sbt](https://scala-sbt.org), simply add the following line to your `build.sbt` file.

```scala
libraryDependencies += "@ORGANIZATION@" %% "@CORE_MODULE_NAME@" % "@VERSION@"
```

Published for Scala @SCALA_PUBLISH_VERSIONS@, [Scala.js](https://www.scala-js.org) @SCALA_JS_MAJOR_MINOR_VERSION@ and [Scala Native](https://scala-native.org) @SCALA_NATIVE_MAJOR_MINOR_VERSION@.

For changes between versions, please refer to the [release notes](https://github.com/typelevel/vulcan/releases).

### External Modules

Following is an incomplete list of third-party integrations.

- [fs2-kafka-vulcan](https://typelevel.org/fs2-kafka), which currently uses Vulcan 1.x.

### Signatures

Stable release artifacts are signed with the [`14CA 4AC4 FDB0 0D5F`](https://keys.openpgp.org/search?q=598648A65DF64506) key.

### Compatibility

Backwards binary-compatibility for the library is guaranteed between minor and patch versions.

Version `@MAJOR_VERSION@.a.b` is backwards binary-compatible with `@MAJOR_VERSION@.c.d` for any `a > c` or `a = c` and `b > d`.

Please note binary-compatibility is not guaranteed between milestone releases.

### Snapshot Releases

To use the latest snapshot release, add the following lines to your `build.sbt` file.

```scala
resolvers += Resolver.sonatypeCentralSnapshots

libraryDependencies += "@ORGANIZATION@" %% "@CORE_MODULE_NAME@" % "@SNAPSHOT_VERSION@"
```

## Dependencies

Refer to the table below for dependencies and version support across modules.

| Module                                                   | Dependencies                                                                                                                                                                                       | Scala                                   |
| -------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------- |
| `@CORE_MODULE_NAME@`                                     | [cats-free @CATS_VERSION@](https://github.com/typelevel/cats), [circe @CIRCE_VERSION@](https://github.com/circe/circe), [scodec-bits @SCODEC_BITS_VERSION@](https://github.com/scodec/scodec-bits) | Scala @CORE_CROSS_SCALA_VERSIONS@       |
| [`@ENUMERATUM_MODULE_NAME@`](integrations/enumeratum.md) | [enumeratum @ENUMERATUM_VERSION@](https://github.com/lloydmeta/enumeratum)                                                                                                                         | Scala @ENUMERATUM_CROSS_SCALA_VERSIONS@ |
| [`@GENERIC_MODULE_NAME@`](derivation.md)                 | [magnolia @MAGNOLIA_VERSION@](https://github.com/softwaremill/magnolia)                                                                                                                            | Scala @GENERIC_CROSS_SCALA_VERSIONS@    |
| [`@REFINED_MODULE_NAME@`](integrations/refined.md)       | [refined @REFINED_VERSION@](https://github.com/fthomas/refined)                                                                                                                                    | Scala @REFINED_CROSS_SCALA_VERSIONS@    |

## Inspiration

Library is heavily inspired by ideas from [avro4s](https://github.com/sksamuel/avro4s).

## Participation

Everyone is expected to follow the [Typelevel Code of Conduct](https://typelevel.org/code-of-conduct/) as described [here](https://github.com/typelevel/.github/blob/main/CODE_OF_CONDUCT.md).

## License

Licensed under the [Apache License 2.0](https://www.apache.org/licenses/LICENSE-2.0.html). Refer to the [license file](https://github.com/typelevel/vulcan/blob/main/license.txt).
