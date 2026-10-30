/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Eq
import cats.laws.discipline.InvariantTests
import cats.tests.CatsSuite
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import scala.util.Try
import scodec.bits.ByteVector

final class CodecInvariantSpec extends CatsSuite with EitherValues {
  val schemaGen: Gen[Either[AvroException, Schema]] =
    Gen.oneOf(
      Left(AvroException("error")),
      Codec[Int].schema,
      Codec[String].schema
    )

  val charsetGen: Gen[Charset] =
    Gen.oneOf(
      StandardCharsets.ISO_8859_1,
      StandardCharsets.US_ASCII,
      StandardCharsets.UTF_8,
      StandardCharsets.UTF_16
    )

  implicit val codecStringArbitrary: Arbitrary[Codec[String]] =
    Arbitrary {
      schemaGen.flatMap { schema =>
        charsetGen.map { charset =>
          Codec
            .instance[String](
              schema,
              s => Right(AvroValue.Bytes(ByteVector.view(s.getBytes(charset)))),
              {
                case _: Schema.String =>
                  Right {
                    case AvroValue.Bytes(bytes) => Right(new String(bytes.toArray, charset))
                    case value => Left(AvroException(s"Got unexpected value $value"))
                  }
                case schema =>
                  Left(AvroException(s"Got unexpected schema $schema"))
              }
            )
            .withTypeName("String")
        }
      }
    }

  implicit val codecStringEq: Eq[Codec[String]] =
    Eq.instance { (c1, c2) =>
      Try {
        forAll { (s: String) =>
          val e1 = c1.encode(s)
          val e2 = c2.encode(s)

          assert(e1.isRight == e2.isRight)
          if (e1.isRight && e2.isRight) {
            val d1 = c1.schema.flatMap(c1.decoder).flatMap(_.decode(e1.value))
            val d2 = c2.schema.flatMap(c2.decoder).flatMap(_.decode(e2.value))
            val _ = assert(d1.left.map(_.message) === d2.left.map(_.message))
          }
        }
      }.isSuccess
    }

  checkAll("Codec", InvariantTests[Codec].invariant[String, String, String])
}
