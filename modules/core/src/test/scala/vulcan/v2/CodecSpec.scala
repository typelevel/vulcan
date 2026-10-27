/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import cats.data._
import cats.syntax.all._
import io.circe.Json
import java.nio.charset.StandardCharsets
import org.scalacheck.Gen
import org.scalatest.Assertion
import scala.util.{Failure, Success, Try}
import scodec.bits._
import vulcan.{AvroError, BaseSpec}

final class CodecSpec extends BaseSpec with CodecSpecHelpers {
  describe("Codec") {
    describe("boolean") {
      describe("schema") {
        it("should be encoded as boolean") {
          assertSchemaIs[Boolean] {
            """"boolean""""
          }
        }
      }

      describe("encode") {
        it("should encode as boolean") {
          assertEncodeIs[Boolean](
            true,
            Right(AvroValue.Boolean(true))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not boolean") {
          assertDecodeError[Boolean](
            unsafeEncode(false),
            unsafeSchema[Long],
            "Error decoding Boolean: Got unexpected schema type long, expected schema type boolean"
          )
        }

        it("should error if value is not boolean") {
          assertDecodeError[Boolean](
            unsafeEncode(10L),
            unsafeSchema[Boolean],
            "Error decoding Boolean: Invalid long value for boolean"
          )
        }

        it("should error if value is null") {
          assertDecodeError[Boolean](
            AvroValue.Null,
            unsafeSchema[Boolean],
            "Error decoding Boolean: Invalid null value for boolean"
          )
        }

        it("should decode as Boolean") {
          val value = false
          assertDecodeIs[Boolean](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("byte") {
      describe("schema") {
        it("should be encoded as int") {
          assertSchemaIs[Byte] {
            """"int""""
          }
        }
      }

      describe("encode") {
        it("should encode as int") {
          val value = 1.toByte
          assertEncodeIs[Byte](
            value,
            Right(AvroValue.Int(1))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not int") {
          assertDecodeError[Byte](
            unsafeEncode(1.toByte),
            unsafeSchema[String],
            "Error decoding Byte: Error decoding Int: Got unexpected schema type string, expected schema type int"
          )
        }

        it("should error if value is not int") {
          assertDecodeError[Byte](
            unsafeEncode("value"),
            unsafeSchema[Byte],
            "Error decoding Byte: Error decoding Int: Invalid string value for int"
          )
        }

        it("should error if int value is not byte") {
          val gen =
            Gen.oneOf(
              Gen.chooseNum(Int.MinValue, Byte.MinValue.toInt - 1),
              Gen.chooseNum(Byte.MaxValue.toInt + 1, Int.MaxValue)
            )

          forAll(gen) { nonByte =>
            assertDecodeError[Byte](
              unsafeEncode(nonByte),
              unsafeSchema[Byte],
              s"Error decoding Byte: Got unexpected Int value $nonByte, expected value in range -128 to 127"
            )
          }
        }

        it("should decode as byte") {
          forAll { (byte: Byte) =>
            assertDecodeIs[Byte](
              unsafeEncode(byte),
              Right(byte)
            )
          }
        }
      }
    }

    describe("bytes") {
      describe("schema") {
        it("should be encoded as bytes") {
          assertSchemaIs[ByteVector] {
            """"bytes""""
          }
        }
      }

      describe("encode") {
        it("should encode as bytes") {
          assertEncodeIs[ByteVector](
            hex"01",
            Right(AvroValue.Bytes(hex"01"))
          )
        }
      }

      describe("decode") {
        it("should error on schema other than bytes or string") {
          assertDecodeError[ByteVector](
            unsafeEncode(hex"01"),
            unsafeSchema[Int],
            "Error decoding ByteVector: Got unexpected schema type int, expected schema type bytes"
          )
        }

        it("should error if value is not bytes") {
          assertDecodeError[ByteVector](
            AvroValue.Int(10),
            unsafeSchema[ByteVector],
            "Error decoding ByteVector: Invalid int value for bytes"
          )
        }

        it("should decode string as bytes") {
          assertDecodeIs[ByteVector](
            unsafeEncode("foo"),
            Right(ByteVector("foo".getBytes(StandardCharsets.UTF_8))),
            Some(unsafeSchema[String])
          )
        }

        it("should decode bytes as bytes") {
          val bytes =
            ByteVector("c6e6b318-303c-4ffa-8c0b-c689ddc49e94".getBytes(StandardCharsets.UTF_8))
          assertDecodeIs[ByteVector](
            AvroValue.Bytes(bytes),
            Right(bytes)
          )
        }
      }
    }

    describe("chain") {
      describe("schema") {
        it("should be encoded as array") {
          assertSchemaIs[Chain[String]] {
            """{"type":"array","items":"string"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as array using encoder for underlying type") {
          assertEncodeIs[Chain[Int]](
            Chain(1, 2, 3),
            Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not array") {
          assertDecodeError[Chain[Int]](
            unsafeEncode(Chain(1, 2, 3)),
            unsafeSchema[Int],
            "Error decoding Chain: Got unexpected schema type int, expected schema type array"
          )
        }

        it("should error if value is not array") {
          assertDecodeError[Chain[Int]](
            unsafeEncode(10),
            unsafeSchema[Chain[Int]],
            "Error decoding Chain: Invalid int value for array"
          )
        }

        it("should decode as Chain") {
          val value = Chain(1, 2, 3)
          assertDecodeIs[Chain[Int]](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("char") {
      describe("schema") {
        it("should be encoded as string") {
          assertSchemaIs[Char] {
            """"string""""
          }
        }
      }

      describe("encode") {
        it("should encode as string") {
          val value = 'a'
          assertEncodeIs[Char](
            value,
            Right(AvroValue.String("a"))
          )
        }
      }

      describe("decode") {
        it("should error if string value is empty") {
          assertDecodeError[Char](
            unsafeEncode(""),
            unsafeSchema[String],
            "Error decoding Char: Got unexpected String with length 0, expected length 1"
          )
        }

        it("should error if string value has more than 1 char") {
          assertDecodeError[Char](
            unsafeEncode("ab"),
            unsafeSchema[String],
            "Error decoding Char: Got unexpected String with length 2, expected length 1"
          )
        }

        it("should decode as char") {
          forAll { (char: Char) =>
            assertDecodeIs[Char](
              unsafeEncode(char),
              Right(char)
            )
          }
        }
      }
    }

    describe("decode") {
      it("should decode using codec for type") {
        forAll { (n: Int) =>
          assert(Codec.decode[Int](AvroValue.Int(n)).value == n)
        }
      }

      it("should decode values of unions using the written type") {
        assertDecodeIs[Long](
          AvroValue.Union(1, AvroValue.Int(1)),
          Right(1L),
          Some(Schema.Union(Vector(Schema.Null, Schema.Int)))
        )
      }

      it("should error if the written type of a union cannot be decoded") {
        assertDecodeError[Long](
          AvroValue.Union(0, AvroValue.Null),
          Schema.Union(Vector(Schema.Null, Schema.Int)),
          "Error decoding Long: Got unexpected schema type null, expected schema type long"
        )
      }

      it("should error if the index is invalid for the union") {
        assertDecodeError[Long](
          AvroValue.Union(2, AvroValue.Int(1)),
          Schema.Union(Vector(Schema.Null, Schema.Int)),
          "Invalid index 2 for union with 2 types"
        )
      }

      it("should error if the value is not a union value") {
        assertDecodeError[Long](
          AvroValue.Int(1),
          Schema.Union(Vector(Schema.Null, Schema.Int)),
          "Invalid int value for union"
        )
      }

      it("should error if the writer schema references an undefined name") {
        assertDecodeError[Int](
          AvroValue.Int(1),
          Schema.Ref(Schema.Name("Undefined", Some("com.example"))),
          "Undefined name com.example.Undefined"
        )
      }
    }

    describe("double") {
      describe("schema") {
        it("should be encoded as double") {
          assertSchemaIs[Double] {
            """"double""""
          }
        }
      }

      describe("encode") {
        it("should encode as double") {
          val value = 123d
          assertEncodeIs[Double](
            value,
            Right(AvroValue.Double(value))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not double or promotable to double") {
          assertDecodeError[Double](
            unsafeEncode(123d),
            unsafeSchema[String],
            "Error decoding Double: Got unexpected schema type string, expected schema type double"
          )
        }

        it("should error if value is not double or promotable to double") {
          assertDecodeError[Double](
            unsafeEncode("foo"),
            unsafeSchema[Double],
            "Error decoding Double: Invalid string value for double"
          )
        }

        it("should decode Float as Double") {
          assertDecodeIs[Double](
            unsafeEncode(123f),
            Right(123d),
            Some(unsafeSchema[Float])
          )
        }

        it("should decode Int as Double") {
          assertDecodeIs[Double](
            unsafeEncode(123),
            Right(123d),
            Some(unsafeSchema[Int])
          )
        }

        it("should decode Long as Double") {
          assertDecodeIs[Double](
            unsafeEncode(123L),
            Right(123d),
            Some(unsafeSchema[Long])
          )
        }

        it("should decode Double as Double") {
          val value = 123d
          assertDecodeIs[Double](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("encode") {
      it("should encode using codec for type") {
        forAll { (n: Int) =>
          assert(Codec.encode(n).value == AvroValue.Int(n))
        }
      }
    }

    describe("float") {
      describe("schema") {
        it("should be encoded as float") {
          assertSchemaIs[Float] {
            """"float""""
          }
        }
      }

      describe("encode") {
        it("should encode as float") {
          val value = 123f
          assertEncodeIs[Float](
            value,
            Right(AvroValue.Float(value))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not float or promotable to float") {
          assertDecodeError[Float](
            unsafeEncode(123f),
            unsafeSchema[String],
            "Error decoding Float: Got unexpected schema type string, expected schema type float"
          )
        }

        it("should error if value is not float or promotable to float") {
          assertDecodeError[Float](
            unsafeEncode("foo"),
            unsafeSchema[Float],
            "Error decoding Float: Invalid string value for float"
          )
        }

        it("should decode Int as Float") {
          assertDecodeIs[Float](
            unsafeEncode(123),
            Right(123f),
            Some(unsafeSchema[Int])
          )
        }

        it("should decode Long as Float") {
          assertDecodeIs[Float](
            unsafeEncode(123L),
            Right(123f),
            Some(unsafeSchema[Long])
          )
        }

        it("should decode Float as Float") {
          val value = 123f
          assertDecodeIs[Float](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("fromBinary") {
      it("should decode from avro binary") {
        assert(Codec.fromBinary[Int](hex"02", unsafeSchema[Int]) == Right(1))
      }

      it("should decode using the writer schema") {
        assert(Codec.fromBinary[Long](hex"02", unsafeSchema[Int]) == Right(1L))
      }

      it("should decode zero bytes as null") {
        assert(Codec.fromBinary[Unit](ByteVector.empty, unsafeSchema[Unit]) == Right(()))
      }

      it("should decode items promoted to types with larger encodings") {
        val bytes = Codec.toBinary(List(0, 1, Int.MaxValue)).value
        assert {
          Codec.fromBinary[List[Float]](bytes, unsafeSchema[List[Int]]) ==
            Right(List(0f, 1f, Int.MaxValue.toFloat))
        }
      }

      it("should error if the binary does not match the writer schema") {
        val result = Codec.fromBinary[Int](ByteVector.empty, unsafeSchema[Int])
        assert(result.swap.value.message == "Unexpected end of input at position 0")
      }

      it("should error if the writer schema does not match the type") {
        val result = Codec.fromBinary[Int](hex"02", unsafeSchema[Long])
        assert {
          result.swap.value.message ==
            "Error decoding Int: Got unexpected schema type long, expected schema type int"
        }
      }
    }

    describe("fromJson") {
      it("should decode from avro json format") {
        assert(Codec.fromJson[Int](Json.fromInt(1), unsafeSchema[Int]) == Right(1))
      }

      it("should error if the json does not match the writer schema") {
        val result = Codec.fromJson[Int](Json.fromString("badValue"), unsafeSchema[Int])
        assert(result.swap.value.message == "Invalid string for int")
      }

      it("should error if the writer schema does not match the type") {
        val result = Codec.fromJson[Int](Json.fromString("1"), unsafeSchema[String])
        assert {
          result.swap.value.message ==
            "Error decoding Int: Got unexpected schema type string, expected schema type int"
        }
      }
    }

    describe("imapError") {
      case class PosInt(value: Int)

      object PosInt {
        def apply(value: Int): Either[AvroError, PosInt] =
          if (value > 0) Right(new PosInt(value) {})
          else Left(AvroError(s"$value is not positive"))

        implicit val posIntCodec: Codec[PosInt] =
          Codec[Int].imapError(apply)(_.value)
      }

      describe("schema") {
        it("should use the schema of the underlying codec") {
          assertSchemaIs[PosInt] {
            """"int""""
          }
        }
      }

      describe("encode") {
        it("should encode using the underlying codec") {
          assertEncodeIs[PosInt](
            PosInt(1).value,
            Right(unsafeEncode(1))
          )
        }
      }

      describe("decode") {
        it("should succeed for valid values") {
          assertDecodeIs[PosInt](
            unsafeEncode(1),
            PosInt(1)
          )
        }

        it("should error for invalid values") {
          assertDecodeError[PosInt](
            unsafeEncode(0),
            unsafeSchema[PosInt],
            "0 is not positive"
          )
        }
      }
    }

    describe("imapTry") {
      case class PosInt(value: Int)

      object PosInt {
        def apply(value: Int): Try[PosInt] =
          if (value > 0) Success(new PosInt(value) {})
          else Failure(new RuntimeException(s"$value is not positive"))

        implicit val posIntCodec: Codec[PosInt] =
          Codec[Int].imapTry(apply)(_.value)
      }

      describe("schema") {
        it("should use the schema of the underlying codec") {
          assertSchemaIs[PosInt] {
            """"int""""
          }
        }
      }

      describe("encode") {
        it("should encode using the underlying codec") {
          assertEncodeIs[PosInt](
            PosInt(1).get,
            Right(unsafeEncode(1))
          )
        }
      }

      describe("decode") {
        it("should succeed for valid values") {
          assertDecodeIs[PosInt](
            unsafeEncode(1),
            PosInt(1).toEither.leftMap(AvroError.fromThrowable)
          )
        }

        it("should error for invalid values") {
          assertDecodeError[PosInt](
            unsafeEncode(0),
            unsafeSchema[PosInt],
            "java.lang.RuntimeException: 0 is not positive"
          )
        }
      }
    }

    describe("instance") {
      val codec: Codec[Int] =
        Codec.instance[Int](
          Right(Schema.Int),
          int => Right(AvroValue.Long(int.toLong)),
          {
            case (AvroValue.Int(int), _: Schema.Int) => Right(int)
            case (value, schema) => Left(AvroError(s"Got value $value with schema $schema"))
          }
        )

      describe("schema") {
        it("should use the schema") {
          assert(codec.schema == Right(Schema.Int))
        }

        it("should error if the schema is invalid") {
          val invalid =
            Codec.instance[Int](
              Right(Schema.Fixed(Schema.Name("1Invalid"), 1)),
              int => Right(AvroValue.Int(int)),
              (_, _) => Left(AvroError("error"))
            )

          assert {
            invalid.schema.swap.value.message ==
              """Invalid name "1Invalid": must start with a letter or underscore"""
          }
        }
      }

      describe("encode") {
        it("should encode using the encode function") {
          assert(codec.encode(1) == Right(AvroValue.Long(1L)))
        }
      }

      describe("decode") {
        it("should decode using the decode function") {
          assert(codec.decode(AvroValue.Int(1), Schema.Int) == Right(1))
        }

        it("should decode values of unions using the written type") {
          assert {
            codec.decode(
              AvroValue.Union(1, AvroValue.Int(1)),
              Schema.Union(Vector(Schema.Null, Schema.Int))
            ) == Right(1)
          }
        }
      }
    }

    describe("int") {
      describe("schema") {
        it("should be encoded as int") {
          assertSchemaIs[Int] {
            """"int""""
          }
        }
      }

      describe("encode") {
        it("should encode as int") {
          val value = 123
          assertEncodeIs[Int](
            value,
            Right(AvroValue.Int(value))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not int") {
          assertDecodeError[Int](
            unsafeEncode(123),
            unsafeSchema[Long],
            "Error decoding Int: Got unexpected schema type long, expected schema type int"
          )
        }

        it("should error if value is not int") {
          assertDecodeError[Int](
            unsafeEncode(10L),
            unsafeSchema[Int],
            "Error decoding Int: Invalid long value for int"
          )
        }

        it("should decode as Int") {
          val value = 123
          assertDecodeIs[Int](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("list") {
      describe("schema") {
        it("should be encoded as array") {
          assertSchemaIs[List[String]] {
            """{"type":"array","items":"string"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as array using encoder for underlying type") {
          assertEncodeIs[List[Int]](
            List(1, 2, 3),
            Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
          )
        }

        it("should error if an item cannot be encoded") {
          implicit val intCodec: Codec[Int] =
            Codec.int.imapErrors(Right(_))(_ => Left(AvroError("error")))

          assertEncodeError[List[Int]](
            List(1, 2, 3),
            "Error encoding List: error"
          )
        }
      }

      describe("decode") {
        it("should error if schema is not array") {
          assertDecodeError[List[Int]](
            unsafeEncode(List(1, 2, 3)),
            unsafeSchema[Int],
            "Error decoding List: Got unexpected schema type int, expected schema type array"
          )
        }

        it("should error if value is not array") {
          assertDecodeError[List[Int]](
            unsafeEncode(10),
            unsafeSchema[List[Int]],
            "Error decoding List: Invalid int value for array"
          )
        }

        it("should error if an item cannot be decoded") {
          assertDecodeError[List[Int]](
            AvroValue.Array(Vector(AvroValue.Long(1L))),
            Schema.Array(Schema.Long),
            "Error decoding List: Error decoding Int: Got unexpected schema type long, expected schema type int"
          )
        }

        it("should decode items using the writer schema") {
          assertDecodeIs[List[Long]](
            AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2))),
            Right(List(1L, 2L)),
            Some(Schema.Array(Schema.Int))
          )
        }

        it("should decode empty arrays even if items cannot be decoded") {
          assertDecodeIs[List[Int]](
            AvroValue.Array(Vector.empty),
            Right(List.empty),
            Some(Schema.Array(Schema.String))
          )
        }

        it("should decode as List") {
          val value = List(1, 2, 3)
          assertDecodeIs[List[Int]](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("long") {
      describe("schema") {
        it("should be encoded as long") {
          assertSchemaIs[Long] {
            """"long""""
          }
        }
      }

      describe("encode") {
        it("should encode as long") {
          val value = 123L
          assertEncodeIs[Long](
            value,
            Right(AvroValue.Long(value))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not int or long") {
          assertDecodeError[Long](
            unsafeEncode(123L),
            unsafeSchema[String],
            "Error decoding Long: Got unexpected schema type string, expected schema type long"
          )
        }

        it("should error if value is not int or long") {
          assertDecodeError[Long](
            unsafeEncode(123.0),
            unsafeSchema[Long],
            "Error decoding Long: Invalid double value for long"
          )
        }

        it("should decode int as long") {
          assertDecodeIs[Long](
            unsafeEncode(123),
            Right(123L),
            Some(unsafeSchema[Int])
          )
        }

        it("should decode long as long") {
          val value = 123L
          assertDecodeIs[Long](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("map") {
      describe("schema") {
        it("should be encoded as map") {
          assertSchemaIs[Map[String, Int]] {
            """{"type":"map","values":"int"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as map using encoder for value") {
          assertEncodeIs[Map[String, Int]](
            Map("key" -> 1),
            Right(AvroValue.Map(Vector("key" -> unsafeEncode(1))))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not map") {
          assertDecodeError[Map[String, Int]](
            unsafeEncode[Map[String, Int]](Map("key" -> 1)),
            unsafeSchema[Int],
            "Error decoding Map: Got unexpected schema type int, expected schema type map"
          )
        }

        it("should error if value is not map") {
          assertDecodeError[Map[String, Int]](
            unsafeEncode(123),
            unsafeSchema[Map[String, Int]],
            "Error decoding Map: Invalid int value for map"
          )
        }

        it("should decode values using the writer schema") {
          assertDecodeIs[Map[String, Long]](
            AvroValue.Map(Vector("key" -> AvroValue.Int(1))),
            Right(Map("key" -> 1L)),
            Some(Schema.Map(Schema.Int))
          )
        }

        it("should decode the last value for duplicate keys") {
          assertDecodeIs[Map[String, Int]](
            AvroValue.Map(Vector("key" -> AvroValue.Int(1), "key" -> AvroValue.Int(2))),
            Right(Map("key" -> 2))
          )
        }

        it("should decode to map using decoder for value") {
          assertDecodeIs[Map[String, Int]](
            unsafeEncode[Map[String, Int]](Map("key" -> 1)),
            Right(Map("key" -> 1))
          )
        }
      }
    }

    describe("nonEmptyChain") {
      describe("schema") {
        it("should be encoded as array") {
          assertSchemaIs[NonEmptyChain[String]] {
            """{"type":"array","items":"string"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as array using encoder for underlying type") {
          assertEncodeIs[NonEmptyChain[Int]](
            NonEmptyChain(1, 2, 3),
            Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not array") {
          assertDecodeError[NonEmptyChain[Int]](
            unsafeEncode(NonEmptyChain(1, 2, 3)),
            unsafeSchema[Int],
            "Error decoding NonEmptyChain: Got unexpected schema type int, expected schema type array"
          )
        }

        it("should error if value is not array") {
          assertDecodeError[NonEmptyChain[Int]](
            unsafeEncode(10),
            unsafeSchema[NonEmptyChain[Int]],
            "Error decoding NonEmptyChain: Invalid int value for array"
          )
        }

        it("should error on empty collection") {
          assertDecodeError[NonEmptyChain[Int]](
            unsafeEncode(Chain.empty[Int]),
            unsafeSchema[NonEmptyChain[Int]],
            "Error decoding NonEmptyChain: Got unexpected empty collection"
          )
        }

        it("should decode as NonEmptyChain") {
          val value = NonEmptyChain(1, 2, 3)
          assertDecodeIs[NonEmptyChain[Int]](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("nonEmptyList") {
      describe("schema") {
        it("should be encoded as array") {
          assertSchemaIs[NonEmptyList[String]] {
            """{"type":"array","items":"string"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as array using encoder for underlying type") {
          assertEncodeIs[NonEmptyList[Int]](
            NonEmptyList.of(1, 2, 3),
            Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not array") {
          assertDecodeError[NonEmptyList[Int]](
            unsafeEncode(NonEmptyList.of(1, 2, 3)),
            unsafeSchema[Int],
            "Error decoding NonEmptyList: Got unexpected schema type int, expected schema type array"
          )
        }

        it("should error if value is not array") {
          assertDecodeError[NonEmptyList[Int]](
            unsafeEncode(10),
            unsafeSchema[NonEmptyList[Int]],
            "Error decoding NonEmptyList: Invalid int value for array"
          )
        }

        it("should error on empty collection") {
          assertDecodeError[NonEmptyList[Int]](
            unsafeEncode(List.empty[Int]),
            unsafeSchema[NonEmptyList[Int]],
            "Error decoding NonEmptyList: Got unexpected empty collection"
          )
        }

        it("should decode as NonEmptyList") {
          val value = NonEmptyList.of(1, 2, 3)
          assertDecodeIs[NonEmptyList[Int]](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("nonEmptyMap") {
      describe("schema") {
        it("should be encoded as map") {
          assertSchemaIs[NonEmptyMap[String, Int]] {
            """{"type":"map","values":"int"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as map using encoder for value") {
          assertEncodeIs[NonEmptyMap[String, Int]](
            NonEmptyMap.of("key1" -> 1, "key2" -> 2, "key3" -> 3),
            Right(
              AvroValue.Map(
                Vector(
                  "key1" -> unsafeEncode(1),
                  "key2" -> unsafeEncode(2),
                  "key3" -> unsafeEncode(3)
                )
              )
            )
          )
        }
      }

      describe("decode") {
        it("should error if schema is not map") {
          assertDecodeError[NonEmptyMap[String, Int]](
            unsafeEncode[NonEmptyMap[String, Int]](NonEmptyMap.one("key", 1)),
            unsafeSchema[Int],
            "Error decoding NonEmptyMap: Got unexpected schema type int, expected schema type map"
          )
        }

        it("should error if value is not map") {
          assertDecodeError[NonEmptyMap[String, Int]](
            unsafeEncode(123),
            unsafeSchema[NonEmptyMap[String, Int]],
            "Error decoding NonEmptyMap: Invalid int value for map"
          )
        }

        it("should error on empty collection") {
          assertDecodeError[NonEmptyMap[String, Int]](
            unsafeEncode(Map.empty[String, Int]),
            unsafeSchema[NonEmptyMap[String, Int]],
            "Error decoding NonEmptyMap: Got unexpected empty collection"
          )
        }

        it("should decode to map using decoder for value") {
          val value = NonEmptyMap.of("key1" -> 1, "key2" -> 2, "key3" -> 3)
          assertDecodeIs[NonEmptyMap[String, Int]](
            unsafeEncode[NonEmptyMap[String, Int]](value),
            Right(value)
          )
        }
      }
    }

    describe("nonEmptySet") {
      describe("schema") {
        it("should be encoded as array") {
          assertSchemaIs[NonEmptySet[String]] {
            """{"type":"array","items":"string"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as array using encoder for underlying type") {
          assertEncodeIs[NonEmptySet[Int]](
            NonEmptySet.of(1, 2, 3),
            Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not array") {
          assertDecodeError[NonEmptySet[Int]](
            unsafeEncode(NonEmptySet.of(1, 2, 3)),
            unsafeSchema[Int],
            "Error decoding NonEmptySet: Got unexpected schema type int, expected schema type array"
          )
        }

        it("should error if value is not array") {
          assertDecodeError[NonEmptySet[Int]](
            unsafeEncode(10),
            unsafeSchema[NonEmptySet[Int]],
            "Error decoding NonEmptySet: Invalid int value for array"
          )
        }

        it("should error on empty collection") {
          assertDecodeError[NonEmptySet[Int]](
            unsafeEncode(Set.empty[Int]),
            unsafeSchema[NonEmptySet[Int]],
            "Error decoding NonEmptySet: Got unexpected empty collection"
          )
        }

        it("should decode as NonEmptySet") {
          val value = NonEmptySet.of(1, 2, 3)
          assertDecodeIs[NonEmptySet[Int]](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("nonEmptyVector") {
      describe("schema") {
        it("should be encoded as array") {
          assertSchemaIs[NonEmptyVector[String]] {
            """{"type":"array","items":"string"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as array using encoder for underlying type") {
          assertEncodeIs[NonEmptyVector[Int]](
            NonEmptyVector.of(1, 2, 3),
            Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not array") {
          assertDecodeError[NonEmptyVector[Int]](
            unsafeEncode(NonEmptyVector.of(1, 2, 3)),
            unsafeSchema[Int],
            "Error decoding NonEmptyVector: Got unexpected schema type int, expected schema type array"
          )
        }

        it("should error if value is not array") {
          assertDecodeError[NonEmptyVector[Int]](
            unsafeEncode(10),
            unsafeSchema[NonEmptyVector[Int]],
            "Error decoding NonEmptyVector: Invalid int value for array"
          )
        }

        it("should error on empty collection") {
          assertDecodeError[NonEmptyVector[Int]](
            unsafeEncode(Vector.empty[Int]),
            unsafeSchema[NonEmptyVector[Int]],
            "Error decoding NonEmptyVector: Got unexpected empty collection"
          )
        }

        it("should decode as NonEmptyVector") {
          val value = NonEmptyVector.of(1, 2, 3)
          assertDecodeIs[NonEmptyVector[Int]](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("seq") {
      describe("schema") {
        it("should be encoded as array") {
          assertSchemaIs[Seq[String]] {
            """{"type":"array","items":"string"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as array using encoder for underlying type") {
          assertEncodeIs[Seq[Int]](
            Seq(1, 2, 3),
            Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not array") {
          assertDecodeError[Seq[Int]](
            unsafeEncode(Seq(1, 2, 3)),
            unsafeSchema[Int],
            "Error decoding Seq: Got unexpected schema type int, expected schema type array"
          )
        }

        it("should error if value is not array") {
          assertDecodeError[Seq[Int]](
            unsafeEncode(10),
            unsafeSchema[Seq[Int]],
            "Error decoding Seq: Invalid int value for array"
          )
        }

        it("should decode as Seq") {
          val value = Seq(1, 2, 3)
          assertDecodeIs[Seq[Int]](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("set") {
      describe("schema") {
        it("should be encoded as array") {
          assertSchemaIs[Set[String]] {
            """{"type":"array","items":"string"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as array using encoder for underlying type") {
          assertEncodeIs[Set[Int]](
            Set(1, 2, 3),
            Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not array") {
          assertDecodeError[Set[Int]](
            unsafeEncode(Set(1, 2, 3)),
            unsafeSchema[Int],
            "Error decoding Set: Got unexpected schema type int, expected schema type array"
          )
        }

        it("should error if value is not array") {
          assertDecodeError[Set[Int]](
            unsafeEncode(10),
            unsafeSchema[Set[Int]],
            "Error decoding Set: Invalid int value for array"
          )
        }

        it("should decode as Set") {
          val value = Set(1, 2, 3)
          assertDecodeIs[Set[Int]](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("short") {
      describe("schema") {
        it("should be encoded as int") {
          assertSchemaIs[Short] {
            """"int""""
          }
        }
      }

      describe("encode") {
        it("should encode as int") {
          val value = 1.toShort
          assertEncodeIs[Short](
            value,
            Right(AvroValue.Int(1))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not int") {
          assertDecodeError[Short](
            unsafeEncode(1.toShort),
            unsafeSchema[String],
            "Error decoding Short: Error decoding Int: Got unexpected schema type string, expected schema type int"
          )
        }

        it("should error if value is not int") {
          assertDecodeError[Short](
            unsafeEncode("value"),
            unsafeSchema[Short],
            "Error decoding Short: Error decoding Int: Invalid string value for int"
          )
        }

        it("should error if int value is not short") {
          val gen =
            Gen.oneOf(
              Gen.chooseNum(Int.MinValue, Short.MinValue.toInt - 1),
              Gen.chooseNum(Short.MaxValue.toInt + 1, Int.MaxValue)
            )

          forAll(gen) { nonShort =>
            assertDecodeError[Short](
              unsafeEncode(nonShort),
              unsafeSchema[Short],
              s"Error decoding Short: Got unexpected Int value $nonShort, expected value in range -32768 to 32767"
            )
          }
        }

        it("should decode as short") {
          forAll { (short: Short) =>
            assertDecodeIs[Short](
              unsafeEncode(short),
              Right(short)
            )
          }
        }
      }
    }

    describe("show") {
      it("should include the schema if available") {
        assert {
          Codec[Int].show == """Codec("int")"""
        }
      }

      it("should show the error if schema is unavailable") {
        val codec =
          Codec.instance[Int](
            Left(AvroError("error")),
            int => Right(AvroValue.Int(int)),
            (_, _) => Left(AvroError("error"))
          )

        assert {
          codec.show == "AvroError(error)"
        }
      }

      it("should have a Show instance consistent with toString") {
        val codec = Codec[Boolean]
        assert {
          codec.show == codec.toString()
        }
      }
    }

    describe("string") {
      describe("schema") {
        it("should be encoded as string") {
          assertSchemaIs[String] {
            """"string""""
          }
        }
      }

      describe("encode") {
        it("should encode as string") {
          val value = "abc"
          assertEncodeIs[String](
            value,
            Right(AvroValue.String(value))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not string or bytes") {
          assertDecodeError[String](
            unsafeEncode("abc"),
            unsafeSchema[Int],
            "Error decoding String: Got unexpected schema type int, expected schema type string"
          )
        }

        it("should error if value is not string or bytes") {
          assertDecodeError[String](
            unsafeEncode(10),
            unsafeSchema[String],
            "Error decoding String: Invalid int value for string"
          )
        }

        it("should decode string as string") {
          val value = "abc"
          assertDecodeIs[String](
            unsafeEncode(value),
            Right(value)
          )
        }

        it("should decode bytes as string") {
          assertDecodeIs[String](
            AvroValue.Bytes(ByteVector("abc".getBytes(StandardCharsets.UTF_8))),
            Right("abc"),
            Some(unsafeSchema[ByteVector])
          )
        }
      }
    }

    describe("toBinary") {
      it("should encode to avro binary") {
        assert(Codec.toBinary[Int](1) == Right(hex"02"))
      }
    }

    describe("toJson") {
      it("should encode to avro json") {
        assert(Codec.toJson[Int](1) == Right(Json.fromInt(1)))
      }
    }

    describe("unit") {
      describe("schema") {
        it("should be encoded as null") {
          assertSchemaIs[Unit] {
            """"null""""
          }
        }
      }

      describe("encode") {
        it("should encode as null") {
          val value = ()
          assertEncodeIs[Unit](
            value,
            Right(AvroValue.Null)
          )
        }
      }

      describe("decode") {
        it("should error if schema is not null") {
          assertDecodeError[Unit](
            unsafeEncode(()),
            unsafeSchema[Int],
            "Error decoding Unit: Got unexpected schema type int, expected schema type null"
          )
        }

        it("should error if value is not null") {
          assertDecodeError[Unit](
            unsafeEncode(10),
            unsafeSchema[Unit],
            "Error decoding Unit: Invalid int value for null"
          )
        }

        it("should decode null as unit") {
          assertDecodeIs[Unit](
            unsafeEncode(()),
            Right(())
          )
        }
      }
    }

    describe("vector") {
      describe("schema") {
        it("should be encoded as array") {
          assertSchemaIs[Vector[String]] {
            """{"type":"array","items":"string"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as array using encoder for underlying type") {
          assertEncodeIs[Vector[Int]](
            Vector(1, 2, 3),
            Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not array") {
          assertDecodeError[Vector[Int]](
            unsafeEncode(Vector(1, 2, 3)),
            unsafeSchema[Int],
            "Error decoding Vector: Got unexpected schema type int, expected schema type array"
          )
        }

        it("should error if value is not array") {
          assertDecodeError[Vector[Int]](
            unsafeEncode(10),
            unsafeSchema[Vector[Int]],
            "Error decoding Vector: Invalid int value for array"
          )
        }

        it("should decode as Vector") {
          val value = Vector(1, 2, 3)
          assertDecodeIs[Vector[Int]](
            unsafeEncode(value),
            Right(value)
          )
        }

        it("should decode as Vector for any number of items") {
          (0 to 64).foreach { size =>
            val value = Vector.range(0, size)
            assertDecodeIs[Vector[Int]](
              unsafeEncode(value),
              Right(value)
            )
          }
        }
      }
    }
  }
}

trait CodecSpecHelpers {
  self: BaseSpec =>

  def unsafeSchema[A](implicit codec: Codec[A]): Schema =
    codec.schema.value

  def unsafeEncode[A](a: A)(implicit codec: Codec[A]): AvroValue =
    codec.encode(a).value

  def unsafeDecode[A](value: AvroValue)(implicit codec: Codec[A]): A =
    codec.schema.flatMap(codec.decode(value, _)).value

  def assertSchemaIs[A](expectedSchema: String)(implicit codec: Codec[A]): Assertion =
    assert(codec.schema.value.toString == expectedSchema)

  def assertEncodeIs[A](
    a: A,
    encoded: Either[AvroError, AvroValue]
  )(implicit codec: Codec[A]): Assertion =
    assert(codec.encode(a).value == encoded.value)

  def assertDecodeIs[A](
    value: AvroValue,
    decoded: Either[AvroError, A],
    schema: Option[Schema] = None
  )(implicit codec: Codec[A]): Assertion = {
    val decode =
      schema
        .map(codec.decode(value, _).value)
        .getOrElse(unsafeDecode(value))
    assert(decode === decoded.value)
  }

  def assertSchemaError[A](
    expectedErrorMessage: String
  )(implicit codec: Codec[A]): Assertion =
    assert(codec.schema.swap.value.message == expectedErrorMessage)

  def assertDecodeError[A](
    value: AvroValue,
    schema: Schema,
    expectedErrorMessage: String
  )(implicit codec: Codec[A]): Assertion =
    assert(codec.decode(value, schema).swap.value.message == expectedErrorMessage)

  def assertEncodeError[A](
    a: A,
    expectedErrorMessage: String
  )(implicit codec: Codec[A]): Assertion =
    assert(codec.encode(a).swap.value.message == expectedErrorMessage)
}
