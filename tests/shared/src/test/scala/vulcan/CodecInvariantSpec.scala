/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Eq
import cats.laws.discipline.InvariantTests
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Test
import scodec.bits.ByteVector
import weaver.FunSuite
import weaver.discipline.Discipline

object CodecInvariantSpec extends FunSuite with Discipline {
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

  /** Returns the decoded string or error message, if the string can be encoded. */
  private def roundtrip(codec: Codec[String], s: String): Option[Either[String, String]] =
    codec.encode(s).toOption.map { encoded =>
      codec.schema.flatMap(codec.decoder).flatMap(_.decode(encoded)).left.map(_.message)
    }

  implicit val codecStringEq: Eq[Codec[String]] =
    Eq.instance { (c1, c2) =>
      val prop = Prop.forAll((s: String) => roundtrip(c1, s) == roundtrip(c2, s))
      Test.check(Test.Parameters.default, prop).passed
    }

  checkAll("Codec", InvariantTests[Codec].invariant[String, String, String])
}
