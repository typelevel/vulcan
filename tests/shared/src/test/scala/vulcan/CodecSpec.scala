/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.data.*
import cats.syntax.all.*
import io.circe.Json
import io.circe.JsonObject
import java.nio.charset.StandardCharsets
import java.time.*
import java.time.temporal.ChronoUnit
import java.util.UUID
import org.scalacheck.Gen
import org.scalatest.Assertion
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scodec.bits.*
import vulcan.Codec.InstantMicros
import vulcan.examples.*

final class CodecSpec extends BaseSpec with CodecSpecHelpers {
  describe("Codec") {
    describe("binaryDecoder") {
      it("should decode values using the writer schema") {
        val decoder = Codec[Long].binaryDecoder(unsafeSchema[Int]).value
        assert(decoder.decode(hex"02") == Right(1L))
        assert(decoder.decode(hex"04") == Right(2L))
      }

      it("should decode values of unions using the written type") {
        val decoder = Codec[Long].binaryDecoder(Schema.Union(Vector(Schema.Int, Schema.Long))).value
        assert(decoder.decode(hex"0002") == Right(1L))
        assert(decoder.decode(hex"0204") == Right(2L))
      }

      it("should ignore remaining bytes") {
        val decoder = Codec[Int].binaryDecoder(unsafeSchema[Int]).value
        assert(decoder.decode(hex"0204") == Right(1))
      }

      it("should decode zero bytes as null") {
        val decoder = Codec[Unit].binaryDecoder(unsafeSchema[Unit]).value
        assert(decoder.decode(ByteVector.empty) == Right(()))
      }

      it("should decode items promoted to types with larger encodings") {
        val bytes = Codec[List[Int]].binaryEncoder.value.encode(List(0, 1, Int.MaxValue)).value
        val decoder = Codec[List[Float]].binaryDecoder(unsafeSchema[List[Int]]).value
        assert(decoder.decode(bytes) == Right(List(0f, 1f, Int.MaxValue.toFloat)))
      }

      it("should error if the writer schema is invalid") {
        val result = Codec[Int].binaryDecoder(Schema.Union(Vector(Schema.Int, Schema.Int)))
        assert(result.swap.value.message == """Duplicate int in union ["int","int"]""")
      }

      it("should error if the schema is invalid") {
        val result = Codec.decimal(precision = -1, scale = -1).binaryDecoder(Schema.Bytes)
        assert(result.swap.value.message == "Invalid decimal precision -1: must be positive")
      }

      it("should error if the binary does not match the writer schema") {
        val decoder = Codec[Int].binaryDecoder(unsafeSchema[Int]).value
        assert {
          decoder.decode(ByteVector.empty).swap.value.message ==
            "Unexpected end of input at position 0"
        }
      }

      it("should error if the writer schema does not match the type") {
        val decoder = Codec[Int].binaryDecoder(unsafeSchema[Long]).value
        assert {
          decoder.decode(hex"02").swap.value.message ==
            "Error decoding Int: Got unexpected schema type long, expected schema type int"
        }
      }
    }

    describe("binaryEncoder") {
      it("should encode to avro binary") {
        val encoder = Codec[Int].binaryEncoder.value
        assert(encoder.encode(1) == Right(hex"02"))
        assert(encoder.encode(2) == Right(hex"04"))
      }
    }

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

    describe("decimal") {
      describe("schema") {
        it("should be encoded as bytes with logical type decimal") {
          implicit val codec: Codec[BigDecimal] =
            Codec.decimal(precision = 10, scale = 5)

          assertSchemaIs[BigDecimal] {
            """{"type":"bytes","logicalType":"decimal","precision":10,"scale":5}"""
          }
        }

        it("should error if precision is not positive") {
          implicit val codec: Codec[BigDecimal] =
            Codec.decimal(precision = -1, scale = -1)

          assertSchemaError[BigDecimal] {
            "Invalid decimal precision -1: must be positive"
          }
        }

        it("should error if scale is negative") {
          implicit val codec: Codec[BigDecimal] =
            Codec.decimal(precision = 5, scale = -1)

          assertSchemaError[BigDecimal] {
            "Invalid decimal scale -1: cannot be negative"
          }
        }

        it("should error if scale exceeds precision") {
          implicit val codec: Codec[BigDecimal] =
            Codec.decimal(precision = 5, scale = 6)

          assertSchemaError[BigDecimal] {
            "Invalid decimal scale 6: cannot exceed precision 5"
          }
        }
      }

      describe("encode") {
        implicit val codec: Codec[BigDecimal] =
          Codec.decimal(precision = 10, scale = 5)

        it("should error if scale is different in schema") {
          assertEncodeError[BigDecimal](
            BigDecimal("123"),
            "Error encoding BigDecimal: Unable to encode decimal with scale 0 as scale 5"
          )
        }

        it("should error if precision exceeds schema precision") {
          assertEncodeError[BigDecimal](
            BigDecimal("123456.45678"),
            "Error encoding BigDecimal: Unable to encode decimal with precision 11 exceeding schema precision 10"
          )
        }

        it("should error if the schema is invalid") {
          assertEncodeError[BigDecimal](
            BigDecimal("1"),
            "Error encoding BigDecimal: Invalid decimal precision -1: must be positive"
          )(Codec.decimal(precision = -1, scale = -1))
        }

        it("should encode as bytes") {
          val value = BigDecimal("123.45678")
          assertEncodeIs[BigDecimal](
            value,
            Right(AvroValue.Bytes(ByteVector(value.underlying.unscaledValue.toByteArray)))
          )
        }
      }

      describe("decode") {
        implicit val codec: Codec[BigDecimal] =
          Codec.decimal(precision = 10, scale = 5)

        it("should error if schema is not bytes") {
          assertDecodeError[BigDecimal](
            unsafeEncode(BigDecimal("123.45678")),
            unsafeSchema[String],
            "Error decoding BigDecimal: Got unexpected schema type string, expected schema type bytes"
          )
        }

        it("should error if value is not bytes") {
          assertDecodeError[BigDecimal](
            unsafeEncode(10),
            unsafeSchema[BigDecimal],
            "Error decoding BigDecimal: Invalid int value for bytes"
          )
        }

        it("should error if logical type is missing") {
          assertDecodeError[BigDecimal](
            unsafeEncode(BigDecimal("123.45678")),
            Schema.Bytes,
            "Error decoding BigDecimal: Got unexpected missing logical type"
          )
        }

        it("should error if logical type is not decimal") {
          assertDecodeError[BigDecimal](
            unsafeEncode(BigDecimal("123.45678")),
            Schema.Bytes.withLogicalType(LogicalType.bigDecimal),
            "Error decoding BigDecimal: Got unexpected logical type big-decimal"
          )
        }

        it("should error if precision exceeds schema precision") {
          assertDecodeError[BigDecimal](
            unsafeEncode(BigDecimal("123.45678")),
            Schema.Bytes.withLogicalType(LogicalType.decimal(6, 5)),
            "Error decoding BigDecimal: Unable to decode decimal with precision 8 exceeding schema precision 6"
          )
        }

        it("should error if bytes are empty") {
          assertDecodeError[BigDecimal](
            AvroValue.Bytes(ByteVector.empty),
            unsafeSchema[BigDecimal],
            "Error decoding BigDecimal: Unable to decode decimal from empty bytes"
          )
        }

        it("should decode using the scale of the writer schema") {
          assertDecodeIs[BigDecimal](
            unsafeEncode(BigDecimal("123.45678")),
            Right(BigDecimal("1234.5678")),
            Some(Schema.Bytes.withLogicalType(LogicalType.decimal(10, 4)))
          )
        }

        it("should decode bytes") {
          val value = BigDecimal("123.45678")
          assertDecodeIs[BigDecimal](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("decoder") {
      it("should decode using codec for type") {
        val decoder = Codec[Int].decoder(Schema.Int).value
        forAll { (n: Int) =>
          assert(decoder.decode(AvroValue.Int(n)).value == n)
        }
      }

      it("should error if the writer schema is invalid") {
        val result = Codec[Int].decoder(Schema.Union(Vector(Schema.Int, Schema.Int)))
        assert(result.swap.value.message == """Duplicate int in union ["int","int"]""")
      }

      it("should error if the schema is invalid") {
        val result = Codec.decimal(precision = -1, scale = -1).decoder(Schema.Bytes)
        assert(result.swap.value.message == "Invalid decimal precision -1: must be positive")
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

    describe("either") {
      describe("schema") {
        it("should be encoded as union") {
          assertSchemaIs[Either[Int, Double]] {
            """["int","double"]"""
          }
        }
      }

      describe("encode") {
        it("should encode left using left schema") {
          assertEncodeIs[Either[Int, Double]](
            Left(1),
            Right(AvroValue.Union(0, AvroValue.Int(1)))
          )
        }

        it("should encode right using right schema") {
          assertEncodeIs[Either[Int, Double]](
            Right(1d),
            Right(AvroValue.Union(1, AvroValue.Double(1d)))
          )
        }
      }

      describe("decode") {
        it("should error if schema type not in union") {
          assertDecodeError[Either[Int, Double]](
            unsafeEncode[Either[Int, Double]](Right(1d)),
            unsafeSchema[String],
            "Error decoding Either: Missing alternative for string in union"
          )
        }

        it("should decode left value as left") {
          assertDecodeIs[Either[Int, Double]](
            unsafeEncode[Either[Int, Double]](Left(1)),
            Right(Left(1))
          )
        }

        it("should decode right value as right") {
          assertDecodeIs[Either[Int, Double]](
            unsafeEncode[Either[Int, Double]](Right(1d)),
            Right(Right(1d))
          )
        }
      }
    }

    describe("encoder") {
      it("should encode using codec for type") {
        val encoder = Codec[Int].encoder.value
        forAll { (n: Int) =>
          assert(encoder.encode(n).value == AvroValue.Int(n))
        }
      }
    }

    describe("enum") {
      def enumCodec(
        default: Option[String] = None,
        props: JsonObject = JsonObject.empty
      ): Codec[String] =
        Codec.enumeration[String](
          name = "Enum",
          namespace = "",
          symbols = List("first", "second"),
          encode = identity,
          decode = Right(_),
          default = default,
          props = props
        )

      describe("schema") {
        it("should have expected schema") {
          assertSchemaIs[SealedTraitEnum] {
            """{"type":"enum","name":"SealedTraitEnum","namespace":"vulcan.examples","doc":"documentation","symbols":["first","second"],"default":"first","custom1":10,"custom2":"value2","aliases":["first","second"]}"""
          }
        }

        it("should capture errors on invalid schema") {
          assertSchemaError[`-SealedTraitEnumInvalidName`] {
            """Invalid name "-SealedTraitEnumInvalidName": must start with a letter or underscore"""
          }
        }

        it("should error if the default is not a symbol") {
          assertSchemaError[String] {
            """Default "third" is not a symbol of enum Enum"""
          }(enumCodec(default = Some("third")))
        }

        it("should error if properties are reserved") {
          assertSchemaError[String] {
            """Invalid property "symbols" of enum Enum: name is reserved"""
          }(enumCodec(props = JsonObject("symbols" -> Json.fromString("first"))))
        }
      }

      describe("encode") {
        it("should error if returned string is not a schema symbol") {
          implicit val codec: Codec[SealedTraitEnum] =
            Codec.enumeration(
              name = "SealedTraitEnum",
              namespace = "vulcan.examples",
              symbols = List("symbol"),
              encode = _ => "not-symbol",
              decode = _ => Left(AvroException("error"))
            )

          assertEncodeError[SealedTraitEnum](
            FirstInSealedTraitEnum,
            "Error encoding vulcan.examples.SealedTraitEnum: Symbol not-symbol is not part of schema symbols [symbol]"
          )
        }

        it("should encode a valid symbol") {
          assertEncodeIs[SealedTraitEnum](
            FirstInSealedTraitEnum,
            Right(AvroValue.Enum("first"))
          )
        }
      }

      describe("decode") {
        it("should error if schema type is not enum") {
          assertDecodeError[SealedTraitEnum](
            unsafeEncode[SealedTraitEnum](FirstInSealedTraitEnum),
            unsafeSchema[String],
            "Error decoding vulcan.examples.SealedTraitEnum: Got unexpected schema type string, expected schema type enum"
          )
        }

        it("should error if value is not enum") {
          assertDecodeError[SealedTraitEnum](
            unsafeEncode(10),
            unsafeSchema[SealedTraitEnum],
            "Error decoding vulcan.examples.SealedTraitEnum: Invalid int value for enum vulcan.examples.SealedTraitEnum"
          )
        }

        it("should return default value if encoded value is not a schema symbol") {
          assertDecodeIs[SealedTraitEnum](
            AvroValue.Enum("symbol"),
            Right(FirstInSealedTraitEnum),
            Some(
              Schema.Enum(Schema.Name("SealedTraitEnum", Some("vulcan.examples")), Vector("symbol"))
            )
          )
        }

        it("should error if encoded value is not a schema symbol and there is no default value") {
          assertDecodeError[SealedTraitEnumNoDefault](
            AvroValue.Enum("symbol"),
            Schema.Enum(
              Schema.Name("SealedTraitEnumNoDefault", Some("vulcan.examples")),
              Vector("symbol")
            ),
            "Error decoding vulcan.examples.SealedTraitEnumNoDefault: symbol is not one of schema symbols [first, second]"
          )
        }

        it("should decode a valid symbol") {
          assertDecodeIs[SealedTraitEnum](
            unsafeEncode[SealedTraitEnum](FirstInSealedTraitEnum),
            Right(FirstInSealedTraitEnum)
          )
        }

        it("should decode a valid symbol with a different namespace") {
          assertDecodeIs[SealedTraitEnum](
            AvroValue.Enum("second"),
            Right(SecondInSealedTraitEnum),
            Some(
              Schema.Enum(
                Schema.Name("SealedTraitEnum", Some("vulcan.examples.alt")),
                Vector("first", "second")
              )
            )
          )
        }

        it("should decode symbols by name, regardless of their order") {
          assertDecodeIs[SealedTraitEnum](
            AvroValue.Enum("first"),
            Right(FirstInSealedTraitEnum),
            Some(
              Schema.Enum(
                Schema.Name("SealedTraitEnum", Some("vulcan.examples")),
                Vector("second", "first")
              )
            )
          )
        }
      }
    }

    describe("fixed") {
      def fixedCodec(size: Int): Codec[ByteVector] =
        Codec.fixed[ByteVector](
          name = "Name",
          namespace = "",
          size = size,
          encode = bytes => bytes,
          decode = bytes => Right(bytes)
        )

      describe("schema") {
        it("should have the expected schema") {
          assertSchemaIs[FixedBoolean] {
            """{"type":"fixed","name":"FixedBoolean","namespace":"vulcan.examples","doc":"A boolean represented as a byte","size":1,"custom":"value","aliases":["SomeOtherBoolean"]}"""
          }
        }

        it("should fail for negative size") {
          assertSchemaError[ByteVector] {
            "Invalid size -1 for fixed Name"
          }(fixedCodec(size = -1))
        }
      }

      describe("encode") {
        it("should error if length exceeds schema size") {
          assertEncodeError[ByteVector](
            hex"0001",
            "Error encoding Name: Got 2 bytes, expected maximum fixed size 1"
          )(fixedCodec(size = 1))
        }

        it("should encode as fixed") {
          assertEncodeIs[FixedBoolean](
            FalseFixedBoolean,
            Right(AvroValue.Fixed(hex"00"))
          )

          assertEncodeIs[FixedBoolean](
            TrueFixedBoolean,
            Right(AvroValue.Fixed(hex"01"))
          )
        }

        it("should pad bytes with zeros to the size") {
          assertEncodeIs[ByteVector](
            hex"01",
            Right(AvroValue.Fixed(hex"010000"))
          )(fixedCodec(size = 3))
        }
      }

      describe("decode") {
        it("should error if schema is not fixed") {
          assertDecodeError[FixedBoolean](
            unsafeEncode[FixedBoolean](FalseFixedBoolean),
            unsafeSchema[String],
            "Error decoding vulcan.examples.FixedBoolean: Got unexpected schema type string, expected schema type fixed"
          )
        }

        it("should error if value is not fixed") {
          assertDecodeError[FixedBoolean](
            AvroValue.Int(123),
            unsafeSchema[FixedBoolean],
            "Error decoding vulcan.examples.FixedBoolean: Invalid int value for fixed vulcan.examples.FixedBoolean"
          )
        }

        it("should error if length does not match schema size") {
          assertDecodeError[FixedBoolean](
            AvroValue.Fixed(hex"0001"),
            unsafeSchema[FixedBoolean],
            "Error decoding vulcan.examples.FixedBoolean: Got 2 bytes, expected fixed size 1"
          )
        }

        it("should error if size of writer schema is different") {
          assertDecodeError[FixedBoolean](
            AvroValue.Fixed(hex"0001"),
            Schema.Fixed(Schema.Name("FixedBoolean", Some("vulcan.examples")), 2),
            "Error decoding vulcan.examples.FixedBoolean: Got unexpected fixed size 2, expected fixed size 1"
          )
        }

        it("should decode as fixed") {
          assertDecodeIs[FixedBoolean](
            unsafeEncode[FixedBoolean](FalseFixedBoolean),
            Right(FalseFixedBoolean)
          )

          assertDecodeIs[FixedBoolean](
            unsafeEncode[FixedBoolean](TrueFixedBoolean),
            Right(TrueFixedBoolean)
          )
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

    describe("imapError") {
      case class PosInt(value: Int)

      object PosInt {
        def apply(value: Int): Either[AvroException, PosInt] =
          if (value > 0) Right(new PosInt(value) {})
          else Left(AvroException(s"$value is not positive"))

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
            PosInt(1).toEither.leftMap(e => AvroException(e.toString, e))
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
            case _: Schema.Int =>
              Right {
                case AvroValue.Int(int) => Right(int)
                case value => Left(AvroException(s"Got value $value"))
              }

            case schema =>
              Left(AvroException(s"Got schema $schema"))
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
              _ => Left(AvroException("error"))
            )

          assert {
            invalid.schema.swap.value.message ==
              """Invalid name "1Invalid": must start with a letter or underscore"""
          }
        }
      }

      describe("encode") {
        it("should encode using the encoder") {
          assert(codec.encode(1) == Right(AvroValue.Long(1L)))
        }
      }

      describe("decode") {
        it("should decode using the decoder function") {
          assert(codec.decoder(Schema.Int).flatMap(_.decode(AvroValue.Int(1))) == Right(1))
        }

        it("should decode values of unions using the written type") {
          assert {
            codec
              .decoder(Schema.Union(Vector(Schema.Null, Schema.Int)))
              .flatMap(_.decode(AvroValue.Union(1, AvroValue.Int(1)))) == Right(1)
          }
        }

        it("should call the decoder function once for each writer schema") {
          var calls = 0
          val counting =
            Codec.instance[Int](
              Right(Schema.Int),
              int => Right(AvroValue.Int(int)),
              _ => {
                calls += 1
                Right(_ => Right(1))
              }
            )

          val decoder = counting.decoder(Schema.Int).value
          assert(decoder.decode(AvroValue.Int(1)) == Right(1))
          assert(decoder.decode(AvroValue.Int(2)) == Right(1))
          assert(calls == 1)
        }

        it("should include the definitions of referenced names in writer schemas") {
          val inner = Schema.Fixed(Schema.Name("Inner"), 1)

          def outer(innerSchema: Schema): Schema =
            Schema.Record(Schema.Name("Outer"), Vector(Schema.Field("inner", innerSchema)))

          val delegating =
            Codec.instance[Int](Right(Schema.Int), int => Right(AvroValue.Int(int)), codec.decoder)

          val decoder = delegating.decoder(Schema.Union(Vector(inner, outer(Schema.Ref(inner.name))))).value
          val value = AvroValue.Union(1, AvroValue.Record(Vector(AvroValue.Fixed(hex"00"))))
          assert(decoder.decode(value).swap.value.message == s"Got schema ${outer(inner)}")
        }

        it("should error if the decoder function errors") {
          assert {
            codec.decoder(Schema.String).flatMap(_.decode(AvroValue.String("a"))).swap.value.message ==
              s"Got schema ${Schema.String}"
          }
        }
      }
    }

    describe("instant") {
      describe("schema") {
        it("should be encoded as long with logical type timestamp-millis") {
          assertSchemaIs[Instant] {
            """{"type":"long","logicalType":"timestamp-millis"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as long") {
          val value = Instant.now()
          assertEncodeIs[Instant](
            value,
            Right(AvroValue.Long(value.toEpochMilli))
          )
        }

        it("should error if epoch milliseconds exceed the maximum long size") {
          assertEncodeError[Instant](
            Instant.MAX,
            s"Error encoding Instant: Unable to encode ${Instant.MAX} as epoch milliseconds exceeding the maximum long size"
          )
        }
      }

      describe("decode") {
        it("should error if schema is not long") {
          assertDecodeError[Instant](
            unsafeEncode("value"),
            unsafeSchema[String],
            "Error decoding Instant: Error decoding Long: Got unexpected schema type string, expected schema type long"
          )
        }

        it("should error if logical type is missing") {
          assertDecodeError[Instant](
            unsafeEncode(Instant.now()),
            unsafeSchema[Long],
            "Error decoding Instant: Got unexpected missing logical type"
          )
        }

        it("should error if logical type is not timestamp-millis") {
          assertDecodeError[Instant](
            unsafeEncode(Instant.now()),
            Schema.Long.withLogicalType(LogicalType.timestampMicros),
            "Error decoding Instant: Got unexpected logical type timestamp-micros"
          )
        }

        it("should error if logical type is missing in nested writer schemas") {
          assertDecodeError[List[Instant]](
            unsafeEncode(List(Instant.now())),
            Schema.Array(Schema.Long),
            "Error decoding List: Error decoding Instant: Got unexpected missing logical type"
          )
        }

        it("should check the logical type of the written type of unions") {
          val value = Instant.ofEpochMilli(1767225600000L)

          assertDecodeIs[Instant](
            AvroValue.Union(1, unsafeEncode(value)),
            Right(value),
            Some(Schema.Union(Vector(Schema.Null, unsafeSchema[Instant])))
          )

          assertDecodeError[Instant](
            AvroValue.Union(1, unsafeEncode(value)),
            Schema.Union(Vector(Schema.Null, Schema.Long)),
            "Error decoding Instant: Got unexpected missing logical type"
          )
        }

        it("should decode as Instant") {
          val value = Instant.ofEpochMilli(1767225600000L)
          assertDecodeIs[Instant](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("instant micros") {
      describe("schema") {
        it("should be encoded as long with logical type timestamp-micros") {
          assertSchemaIs[InstantMicros] {
            """{"type":"long","logicalType":"timestamp-micros"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as long") {
          val instantMicros = InstantMicros(Instant.now())
          assertEncodeIs[InstantMicros](
            instantMicros,
            Right(AvroValue.Long(instantMicros.toEpochMicro))
          )
        }

        it("should encode mid-term future Instant") {
          // Epoch 10_000_000_000, or 2286-11-20T17:46:40Z, exceeds Long.MaxValue in nanos
          val futureEpoch = 10L * 1000 * 1000 * 1000
          assertEncodeIs[InstantMicros](
            InstantMicros(Instant.ofEpochSecond(futureEpoch, 0L)),
            Right(AvroValue.Long(futureEpoch * 1000 * 1000))
          )
        }

        it("should error if epoch microseconds exceed the maximum long size") {
          assertEncodeError[InstantMicros](
            InstantMicros(Instant.MAX),
            s"Error encoding InstantMicros: Unable to encode ${Instant.MAX} as epoch microseconds exceeding the maximum long size"
          )
        }
      }

      describe("decode") {
        it("should error if logical type is missing") {
          assertDecodeError[InstantMicros](
            unsafeEncode(InstantMicros(Instant.now())),
            unsafeSchema[Long],
            "Error decoding InstantMicros: Got unexpected missing logical type"
          )
        }

        it("should error if logical type is not timestamp-micros") {
          assertDecodeError[InstantMicros](
            unsafeEncode(InstantMicros(Instant.now())),
            Schema.Long.withLogicalType(LogicalType.timestampMillis),
            "Error decoding InstantMicros: Got unexpected logical type timestamp-millis"
          )
        }

        it("should truncate Instant to micros") {
          val instant = Instant.now()
          assertDecodeIs[InstantMicros](
            unsafeEncode(InstantMicros(instant)),
            Right(InstantMicros(instant.truncatedTo(ChronoUnit.MICROS)))
          )
        }

        it("should roundtrip negative Instant with microsecond offsets") {
          // 1969-12-31T23:59:59.999999Z is one microsecond before the epoch
          val instantMicros = InstantMicros(Instant.ofEpochSecond(-1L, 999999L * 1000))
          assertDecodeIs[InstantMicros](
            unsafeEncode(instantMicros),
            Right(instantMicros)
          )
        }

        it("should roundtrip the minimum and maximum epoch microseconds") {
          List(Long.MinValue, Long.MaxValue).foreach { micros =>
            val decoded = unsafeDecode[InstantMicros](AvroValue.Long(micros))
            assert(unsafeEncode(decoded) == AvroValue.Long(micros))
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

    describe("jsonDecoder") {
      it("should decode values using the writer schema") {
        val decoder = Codec[Long].jsonDecoder(unsafeSchema[Int]).value
        assert(decoder.decode(Json.fromInt(1)) == Right(1L))
        assert(decoder.decode(Json.fromInt(2)) == Right(2L))
      }

      it("should error if the writer schema is invalid") {
        val result = Codec[Int].jsonDecoder(Schema.Union(Vector(Schema.Int, Schema.Int)))
        assert(result.swap.value.message == """Duplicate int in union ["int","int"]""")
      }

      it("should error if the schema is invalid") {
        val result = Codec.decimal(precision = -1, scale = -1).jsonDecoder(Schema.Bytes)
        assert(result.swap.value.message == "Invalid decimal precision -1: must be positive")
      }

      it("should error if the json does not match the writer schema") {
        val decoder = Codec[Int].jsonDecoder(unsafeSchema[Int]).value
        assert {
          decoder.decode(Json.fromString("badValue")).swap.value.message == "Invalid string for int"
        }
      }

      it("should error if the writer schema does not match the type") {
        val decoder = Codec[Int].jsonDecoder(unsafeSchema[String]).value
        assert {
          decoder.decode(Json.fromString("1")).swap.value.message ==
            "Error decoding Int: Got unexpected schema type string, expected schema type int"
        }
      }
    }

    describe("jsonEncoder") {
      it("should encode to avro json") {
        val encoder = Codec[Int].jsonEncoder.value
        assert(encoder.encode(1) == Right(Json.fromInt(1)))
        assert(encoder.encode(2) == Right(Json.fromInt(2)))
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
            Codec.int.imapErrors(Right(_))(_ => Left(AvroException("error")))

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

    describe("localDate") {
      describe("schema") {
        it("should be encoded as int with logical type date") {
          assertSchemaIs[LocalDate] {
            """{"type":"int","logicalType":"date"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as int") {
          val value = LocalDate.now(ZoneOffset.UTC)
          assertEncodeIs[LocalDate](
            value,
            Right(AvroValue.Int(value.toEpochDay.toInt))
          )
        }

        it("should error if epoch days exceed the maximum integer size") {
          val value = LocalDate.MAX
          assertEncodeError[LocalDate](
            value,
            s"Error encoding LocalDate: Unable to encode date as epoch days of ${value.toEpochDay} exceeds the maximum integer size"
          )
        }
      }

      describe("decode") {
        it("should error if logical type is not date") {
          assertDecodeError[LocalDate](
            unsafeEncode(LocalDate.now(ZoneOffset.UTC)),
            unsafeSchema[Int],
            "Error decoding LocalDate: Got unexpected missing logical type"
          )
        }

        it("should decode int as local date") {
          val value = LocalDate.now(ZoneOffset.UTC)
          assertDecodeIs[LocalDate](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("localDateTime") {
      describe("schema") {
        it("should be encoded as long with logical type local-timestamp-millis") {
          assertSchemaIs[LocalDateTime] {
            """{"type":"long","logicalType":"local-timestamp-millis"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as long") {
          val value = LocalDateTime.ofInstant(Instant.ofEpochMilli(1767225600000L), ZoneOffset.UTC)
          assertEncodeIs[LocalDateTime](
            value,
            Right(AvroValue.Long(1767225600000L))
          )
        }

        it("should error if epoch milliseconds exceed the maximum long size") {
          assertEncodeError[LocalDateTime](
            LocalDateTime.MAX,
            s"Error encoding LocalDateTime: Unable to encode ${LocalDateTime.MAX} as epoch milliseconds exceeding the maximum long size"
          )
        }
      }

      describe("decode") {
        it("should error if logical type is missing") {
          assertDecodeError[LocalDateTime](
            unsafeEncode(LocalDateTime.now(ZoneOffset.UTC)),
            unsafeSchema[Long],
            "Error decoding LocalDateTime: Got unexpected missing logical type"
          )
        }

        it("should error if logical type is not local-timestamp-millis") {
          assertDecodeError[LocalDateTime](
            unsafeEncode(LocalDateTime.now(ZoneOffset.UTC)),
            Schema.Long.withLogicalType(LogicalType.timestampMillis),
            "Error decoding LocalDateTime: Got unexpected logical type timestamp-millis"
          )
        }

        it("should decode as LocalDateTime") {
          val value = LocalDateTime.ofInstant(Instant.ofEpochMilli(1767225600000L), ZoneOffset.UTC)
          assertDecodeIs[LocalDateTime](
            unsafeEncode(value),
            Right(value)
          )
        }
      }
    }

    describe("localTimeMicros") {
      implicit val codec: Codec[LocalTime] = Codec.localTimeMicros

      describe("schema") {
        it("should be encoded as long with logical type time-micros") {
          assertSchemaIs[LocalTime] {
            """{"type":"long","logicalType":"time-micros"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as long") {
          val value = LocalTime.now(ZoneOffset.UTC)
          assertEncodeIs[LocalTime](
            value,
            Right(AvroValue.Long(value.toNanoOfDay / 1000L))
          )
        }
      }

      describe("decode") {
        it("should error if logical type is not time-micros") {
          assertDecodeError[LocalTime](
            unsafeEncode(LocalTime.now(ZoneOffset.UTC)),
            unsafeSchema[Long],
            "Error decoding LocalTime: Got unexpected missing logical type"
          )
        }

        it("should error if value is outside of the day") {
          List(-1L, 86400000000L).foreach { micros =>
            assertDecodeError[LocalTime](
              AvroValue.Long(micros),
              unsafeSchema[LocalTime],
              s"Error decoding LocalTime: Got unexpected Long value $micros, expected value in range 0 to 86399999999"
            )
          }
        }

        it("should decode long as local time-micros") {
          val value = LocalTime.now(ZoneOffset.UTC)
          assertDecodeIs[LocalTime](
            unsafeEncode(value),
            Right(value.truncatedTo(ChronoUnit.MICROS))
          )
        }
      }
    }

    describe("localTimeMillis") {
      implicit val codec: Codec[LocalTime] = Codec.localTimeMillis

      describe("schema") {
        it("should be encoded as int with logical type time-millis") {
          assertSchemaIs[LocalTime] {
            """{"type":"int","logicalType":"time-millis"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as int") {
          val value = LocalTime.now(ZoneOffset.UTC)
          assertEncodeIs[LocalTime](
            value,
            Right(AvroValue.Int((value.toNanoOfDay / 1000000L).toInt))
          )
        }
      }

      describe("decode") {
        it("should error if logical type is not time-millis") {
          assertDecodeError[LocalTime](
            unsafeEncode(LocalTime.now(ZoneOffset.UTC)),
            unsafeSchema[Int],
            "Error decoding LocalTime: Got unexpected missing logical type"
          )
        }

        it("should error if value is outside of the day") {
          List(-1, 86400000).foreach { millis =>
            assertDecodeError[LocalTime](
              AvroValue.Int(millis),
              unsafeSchema[LocalTime],
              s"Error decoding LocalTime: Got unexpected Int value $millis, expected value in range 0 to 86399999"
            )
          }
        }

        it("should decode int as local time-millis") {
          val value = LocalTime.now(ZoneOffset.UTC)
          assertDecodeIs[LocalTime](
            unsafeEncode(value),
            Right(value.truncatedTo(ChronoUnit.MILLIS))
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

    describe("none") {
      describe("schema") {
        it("should be encoded as null") {
          assertSchemaIs[None.type] {
            """"null""""
          }
        }
      }

      describe("encode") {
        it("should encode as null") {
          assertEncodeIs[None.type](
            None,
            Right(AvroValue.Null)
          )
        }
      }

      describe("decode") {
        it("should error if schema is not null") {
          assertDecodeError[None.type](
            unsafeEncode(None),
            unsafeSchema[Int],
            "Error decoding None: Got unexpected schema type int, expected schema type null"
          )
        }

        it("should error if value is not null") {
          assertDecodeError[None.type](
            unsafeEncode(10),
            unsafeSchema[None.type],
            "Error decoding None: Invalid int value for null"
          )
        }

        it("should decode null as None") {
          assertDecodeIs[None.type](
            unsafeEncode(()),
            Right(None)
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

    describe("option") {
      describe("schema") {
        it("should be encoded as union") {
          assertSchemaIs[Option[Double]] {
            """["null","double"]"""
          }
        }

        it("should include the types of unions") {
          assertSchemaIs[Option[Either[Int, String]]] {
            """["null","int","string"]"""
          }
        }

        it("should capture errors on nested unions") {
          assertSchemaError[Option[Option[Int]]] {
            """Duplicate null in union ["null","null","int"]"""
          }
        }
      }

      describe("encode") {
        it("should encode Some") {
          assertEncodeIs[Option[Int]](
            Some(1),
            Right(AvroValue.Union(1, AvroValue.Int(1)))
          )
        }

        it("should encode None as null") {
          assertEncodeIs[Option[Int]](
            None,
            Right(AvroValue.Union(0, AvroValue.Null))
          )
        }

        it("should encode Some using the types of unions") {
          assertEncodeIs[Option[Either[Int, String]]](
            Some(Right("a")),
            Right(AvroValue.Union(2, AvroValue.String("a")))
          )
        }
      }

      describe("decode") {
        it("should error if schema is not in union") {
          assertDecodeError[Option[Int]](
            unsafeEncode(Option(1)),
            unsafeSchema[String],
            "Error decoding Option: Error decoding Int: Got unexpected schema type string, expected schema type int"
          )
        }

        it("should decode if schema is part of union") {
          assertDecodeIs[Option[Int]](
            AvroValue.Int(1),
            Right(Some(1)),
            Some(unsafeSchema[Int])
          )
        }

        it("should decode even if there is one schema in union") {
          assertDecodeIs[Option[Int]](
            AvroValue.Union(0, AvroValue.Int(1)),
            Right(Some(1)),
            Some(Schema.Union(Vector(Schema.Int)))
          )
        }

        it("should decode even if there is not null in union") {
          assertDecodeIs[Option[Int]](
            AvroValue.Union(0, AvroValue.Int(1)),
            Right(Some(1)),
            Some(Schema.Union(Vector(Schema.Int, Schema.String)))
          )
        }

        it("should decode if there are more than two schemas in union") {
          assertDecodeIs[Option[Int]](
            AvroValue.Union(0, AvroValue.Int(1)),
            Right(Some(1)),
            Some(Schema.Union(Vector(Schema.Int, Schema.String, Schema.Null)))
          )
        }

        it("should support null as first schema type in union") {
          assertDecodeIs[Option[Int]](
            AvroValue.Union(1, AvroValue.Int(1)),
            Right(Some(1)),
            Some(Schema.Union(Vector(Schema.Null, Schema.Int)))
          )
        }

        it("should support null as second schema type in union") {
          assertDecodeIs[Option[Int]](
            AvroValue.Union(0, AvroValue.Int(1)),
            Right(Some(1)),
            Some(Schema.Union(Vector(Schema.Int, Schema.Null)))
          )
        }

        it("should decode the written type using its schema") {
          assertDecodeIs[Option[Long]](
            AvroValue.Union(1, AvroValue.Int(1)),
            Right(Some(1L)),
            Some(Schema.Union(Vector(Schema.Null, Schema.Int)))
          )
        }

        it("should decode using the types of unions") {
          assertDecodeIs[Option[Either[Int, String]]](
            AvroValue.Union(2, AvroValue.String("a")),
            Right(Some(Right("a")))
          )
        }

        it("should decode null as None") {
          assertDecodeIs[Option[Int]](
            unsafeEncode(Option.empty[Int]),
            Right(None)
          )
        }

        it("should decode null as None if the writer schema is not a union") {
          assertDecodeIs[Option[Int]](
            AvroValue.Null,
            Right(None),
            Some(Schema.Null)
          )
        }
      }
    }

    describe("option.union") {
      sealed trait FirstOrSecond

      final case class First(value: Int) extends FirstOrSecond
      object First {
        implicit val firstCodec: Codec[First] =
          Codec[Int].imap(apply)(_.value)
      }

      final case class Second(value: Double) extends FirstOrSecond
      object Second {
        implicit val secondCodec: Codec[Second] =
          Codec[Double].imap(apply)(_.value)
      }

      implicit val codec: Codec[Option[FirstOrSecond]] =
        Codec.union(alt => alt[None.type] ++ alt[Some[First]] ++ alt[Some[Second]])

      describe("schema") {
        it("should be encoded as union") {
          assertSchemaIs[Option[FirstOrSecond]] {
            """["null","int","double"]"""
          }
        }
      }

      describe("encode") {
        it("should encode none as null") {
          assert(codec.encode(None) == Right(AvroValue.Union(0, AvroValue.Null)))
        }

        it("should encode first as int") {
          forAll { (n: Int) =>
            assert(codec.encode(Some(First(n))) == Right(AvroValue.Union(1, AvroValue.Int(n))))
          }
        }

        it("should encode second as double") {
          forAll { (n: Double) =>
            assert(codec.encode(Some(Second(n))) == Right(AvroValue.Union(2, AvroValue.Double(n))))
          }
        }
      }

      describe("decode") {
        it("should decode null as none") {
          assert {
            codec
              .decoder(unsafeSchema[Option[FirstOrSecond]])
              .flatMap(_.decode(AvroValue.Union(0, AvroValue.Null))) == Right(None)
          }
        }

        it("should decode int as first") {
          val decoder = codec.decoder(unsafeSchema[Option[FirstOrSecond]]).value
          forAll { (n: Int) =>
            assert(decoder.decode(AvroValue.Union(1, AvroValue.Int(n))) == Right(Some(First(n))))
          }
        }

        it("should decode double as second") {
          val decoder = codec.decoder(unsafeSchema[Option[FirstOrSecond]]).value
          forAll { (n: Double) =>
            assert(decoder.decode(AvroValue.Union(2, AvroValue.Double(n))) == Right(Some(Second(n))))
          }
        }
      }
    }

    describe("record") {
      sealed trait CustomEnum
      case object First extends CustomEnum
      case object Second extends CustomEnum

      implicit val customEnumCodec: Codec[CustomEnum] =
        Codec.enumeration(
          name = "CustomEnum",
          symbols = List("first", "second"),
          encode = {
            case First => "first"
            case Second => "second"
          },
          decode = {
            case "first" => Right(First)
            case "second" => Right(Second)
            case other => Left(AvroException(other))
          },
          namespace = ""
        )

      describe("schema") {
        it("should have the expected schema") {
          assert {
            Codec[CaseClassTwoFields].schema.value.toString() ==
              """{"type":"record","name":"CaseClassTwoFields","namespace":"vulcan.examples","doc":"some documentation for example","fields":[{"name":"name","type":"string","doc":"some doc","default":"default name","order":"descending","aliases":["TheAlias"],"custom":"value"},{"name":"age","type":"int"}],"custom":[1,2,3],"aliases":["FirstAlias","SecondAlias"]}"""
          }
        }

        it("should error if default value can't be encoded") {
          implicit val intCodec: Codec[Int] =
            Codec.int.imapErrors(_ => Left(AvroException("error")))(_ => Left(AvroException("error")))

          implicit val caseClassFieldCodec: Codec[CaseClassField] =
            Codec.record[CaseClassField]("CaseClassField", "") { field =>
              field("value", _.value, default = Some(10)).map(CaseClassField(_))
            }

          assert {
            caseClassFieldCodec.schema.swap.value.message ==
              "Error encoding default for field value: error"
          }
        }

        it("should error if field names are not unique") {
          case class Test(first: Int, second: Int)

          implicit val testCodec: Codec[Test] =
            Codec.record("Test", "") { field =>
              (field("value", _.first), field("value", _.second)).mapN(Test(_, _))
            }

          assertSchemaError[Test] {
            """Duplicate field "value" in record Test"""
          }
        }

        it("should support None as default value") {
          case class Test(value: Option[Int])

          implicit val testCodec: Codec[Test] =
            Codec.record("Test", "") { field =>
              field("value", _.value, default = Some(None)).map(Test(_))
            }

          assertSchemaIs[Test] {
            """{"type":"record","name":"Test","fields":[{"name":"value","type":["null","int"],"default":null}]}"""
          }
        }

        it("should support Some as default value") {
          case class Test(value: Option[Int])

          implicit val testCodec: Codec[Test] =
            Codec.record("Test", "") { field =>
              field("value", _.value, default = Some(Some(0)))(
                Codec.union(alt => alt[Some[Int]] ++ alt[None.type])
              ).map(Test(_))
            }

          assertSchemaIs[Test] {
            """{"type":"record","name":"Test","fields":[{"name":"value","type":["int","null"],"default":0}]}"""
          }
        }

        describe("default") {
          it("should support null default value") {
            case class Test(value: Unit)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(())).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":"null","default":null}]}"""
            }
          }

          it("should support boolean default value") {
            case class Test(value: Boolean)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(false)).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":"boolean","default":false}]}"""
            }
          }

          it("should support int default value") {
            case class Test(value: Int)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(0)).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":"int","default":0}]}"""
            }
          }

          it("should support long default value") {
            case class Test(value: Long)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(0L)).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":"long","default":0}]}"""
            }
          }

          it("should support float default value") {
            case class Test(value: Float)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(1.5f)).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":"float","default":1.5}]}"""
            }
          }

          it("should support double default value") {
            case class Test(value: Double)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(1.5d)).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":"double","default":1.5}]}"""
            }
          }

          it("should support bytes default value") {
            case class Test(value: ByteVector)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(ByteVector(Byte.MinValue, Byte.MaxValue)))
                  .map(Test(_))
              }

            val expectedDefault = "\u0080\u007f"

            assertSchemaIs[Test] {
              s"""{"type":"record","name":"Test","fields":[{"name":"value","type":"bytes","default":"$expectedDefault"}]}"""
            }
          }

          it("should support string default value") {
            case class Test(value: String)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some("default-value")).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":"string","default":"default-value"}]}"""
            }
          }

          it("should support record default value") {
            case class Inner(value: Int, customEnum: CustomEnum)

            implicit val innerCodec: Codec[Inner] =
              Codec.record("Inner", "") { field =>
                (
                  field("value", _.value),
                  field("customEnum", _.customEnum, default = Some(First))
                ).mapN(Inner(_, _))
              }

            case class Test(value: Inner)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(Inner(0, Second))).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"record","name":"Inner","fields":[{"name":"value","type":"int"},{"name":"customEnum","type":{"type":"enum","name":"CustomEnum","symbols":["first","second"]},"default":"first"}]},"default":{"value":0,"customEnum":"second"}}]}"""
            }
          }

          it("should support enum default value") {
            case class Test(value: CustomEnum)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(First)).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"enum","name":"CustomEnum","symbols":["first","second"]},"default":"first"}]}"""
            }
          }

          it("should support array default value") {
            case class Test(value: List[Int])

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(List(123, 456))).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"array","items":"int"},"default":[123,456]}]}"""
            }
          }

          it("should support record array default value") {
            case class Test(value: List[Element])
            case class Element(value: Int)

            implicit val elementCodec: Codec[Element] =
              Codec.record("Element", "") { field =>
                field("value", _.value).map(Element(_))
              }

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(List(Element(123), Element(456))))
                  .map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"array","items":{"type":"record","name":"Element","fields":[{"name":"value","type":"int"}]}},"default":[{"value":123},{"value":456}]}]}"""
            }
          }

          it("should support map default value") {
            case class Test(value: Map[String, Int])

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(Map("key" -> 0))).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"map","values":"int"},"default":{"key":0}}]}"""
            }
          }

          it("should support record map default value") {
            case class Test(value: Map[String, Value])
            case class Value(value: Int)

            implicit val valueCodec: Codec[Value] =
              Codec.record("Value", "") { field =>
                field("value", _.value).map(Value(_))
              }

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(Map("key" -> Value(0)))).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"map","values":{"type":"record","name":"Value","fields":[{"name":"value","type":"int"}]}},"default":{"key":{"value":0}}}]}"""
            }
          }

          it("should support fixed default value") {
            case class Inner(value: ByteVector)

            implicit val innerCodec: Codec[Inner] =
              Codec.fixed(
                name = "Inner",
                size = 1,
                encode = _.value,
                decode = bytes => Right(Inner(bytes)),
                namespace = ""
              )

            case class Test(value: Inner)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, default = Some(Inner(ByteVector(Byte.MaxValue))))
                  .map(Test(_))
              }

            val expectedDefault = "\u007f"

            assertSchemaIs[Test] {
              s"""{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"fixed","name":"Inner","size":1},"default":"$expectedDefault"}]}"""
            }
          }
        }

        describe("props") {
          it("should support custom properties") {
            case class Test(value: Int)

            implicit val testCodec: Codec[Test] =
              Codec.record(
                "Test",
                "",
                props = JsonObject("custom" -> Json.obj("key" -> Json.fromString("value")))
              ) { field =>
                field(
                  "value",
                  _.value,
                  props = JsonObject(
                    "boolean" -> Json.True,
                    "number" -> Json.fromBigDecimal(BigDecimal("123.0")),
                    "array" -> Json.arr(Json.fromInt(123), Json.fromInt(456))
                  )
                ).map(Test(_))
              }

            assertSchemaIs[Test] {
              """{"type":"record","name":"Test","fields":[{"name":"value","type":"int","boolean":true,"number":123.0,"array":[123,456]}],"custom":{"key":"value"}}"""
            }
          }

          it("should error if properties are reserved") {
            case class Test(value: Int)

            implicit val testCodec: Codec[Test] =
              Codec.record("Test", "") { field =>
                field("value", _.value, props = JsonObject("default" -> Json.fromInt(1)))
                  .map(Test(_))
              }

            assertSchemaError[Test] {
              """Invalid property "default" of field "value" in record Test: name is reserved"""
            }
          }
        }
      }

      describe("encode") {
        it("should encode as record") {
          assertEncodeIs[CaseClassTwoFields](
            CaseClassTwoFields("name", 0),
            Right(AvroValue.Record(Vector(AvroValue.String("name"), AvroValue.Int(0))))
          )
        }

        it("should error if a field cannot be encoded") {
          case class Test(value: Int)

          implicit val testCodec: Codec[Test] =
            Codec.record("Test", "") { field =>
              field("value", _.value)(Codec.int.imapErrors(Right(_))(_ => Left(AvroException("error"))))
                .map(Test(_))
            }

          assertEncodeError[Test](
            Test(0),
            "Error encoding Test: Error encoding field value: error"
          )
        }

        it("should support None as default value") {
          case class Test(value: Option[Int])

          implicit val testCodec: Codec[Test] =
            Codec.record("Test", "") { field =>
              field("value", _.value, default = Some(None)).map(Test(_))
            }

          assertEncodeIs[Test](
            Test(None),
            Right(AvroValue.Record(Vector(AvroValue.Union(0, AvroValue.Null))))
          )
        }

        it("should support Some as default value") {
          case class Test(value: Option[Int])

          implicit val testCodec: Codec[Test] =
            Codec.record("Test", "") { field =>
              field("value", _.value, default = Some(Some(0)))(
                Codec.union(alt => alt[Some[Int]] ++ alt[None.type])
              ).map(Test(_))
            }

          assertEncodeIs[Test](
            Test(None),
            Right(AvroValue.Record(Vector(AvroValue.Union(1, AvroValue.Null))))
          )
        }
      }

      describe("decode") {
        val nameField = Schema.Field("name", Schema.String)
        val ageField = Schema.Field("age", Schema.Int)

        def writerSchema(fields: Schema.Field*): Schema =
          Schema.Record(Schema.Name("CaseClassTwoFields", Some("vulcan.examples")), fields.toVector)

        it("should error if schema is not record") {
          assertDecodeError[CaseClassTwoFields](
            unsafeEncode(CaseClassTwoFields("name", 123)),
            unsafeSchema[String],
            "Error decoding vulcan.examples.CaseClassTwoFields: Got unexpected schema type string, expected schema type record"
          )
        }

        it("should error if value is not record") {
          assertDecodeError[CaseClassTwoFields](
            unsafeEncode(123),
            unsafeSchema[CaseClassTwoFields],
            "Error decoding vulcan.examples.CaseClassTwoFields: Invalid int value for record vulcan.examples.CaseClassTwoFields"
          )
        }

        it("should error if the record does not have a value for every field") {
          assertDecodeError[CaseClassTwoFields](
            AvroValue.Record(Vector(AvroValue.String("name"))),
            unsafeSchema[CaseClassTwoFields],
            "Error decoding vulcan.examples.CaseClassTwoFields: Invalid record with 1 values for record vulcan.examples.CaseClassTwoFields with 2 fields"
          )
        }

        it("should error if any field without default value is missing") {
          assertDecodeError[CaseClassTwoFields](
            AvroValue.Record(Vector(AvroValue.String("name"))),
            writerSchema(nameField),
            "Error decoding vulcan.examples.CaseClassTwoFields: Record writer schema is missing field 'age'"
          )
        }

        it("should error if a field cannot be decoded") {
          assertDecodeError[CaseClassTwoFields](
            AvroValue.Record(Vector(AvroValue.String("name"), AvroValue.String("123"))),
            writerSchema(nameField, Schema.Field("age", Schema.String)),
            "Error decoding vulcan.examples.CaseClassTwoFields: Error decoding field age: Error decoding Int: Got unexpected schema type string, expected schema type int"
          )
        }

        it("should decode if field with default value is missing") {
          assertDecodeIs[CaseClassTwoFields](
            AvroValue.Record(Vector(AvroValue.Int(123))),
            Right(CaseClassTwoFields("default name", 123)),
            Some(writerSchema(ageField))
          )
        }

        it("should decode fields in a different order") {
          assertDecodeIs[CaseClassTwoFields](
            AvroValue.Record(Vector(AvroValue.Int(123), AvroValue.String("name"))),
            Right(CaseClassTwoFields("name", 123)),
            Some(writerSchema(ageField, nameField))
          )
        }

        it("should ignore fields which are not part of the record") {
          assertDecodeIs[CaseClassTwoFields](
            AvroValue.Record(
              Vector(AvroValue.String("name"), AvroValue.Double(1.0), AvroValue.Int(123))
            ),
            Right(CaseClassTwoFields("name", 123)),
            Some(writerSchema(nameField, Schema.Field("other", Schema.Double), ageField))
          )
        }

        it("should decode fields using the writer schema") {
          case class Test(value: Long)

          implicit val testCodec: Codec[Test] =
            Codec.record("Test", "") { field =>
              field("value", _.value).map(Test(_))
            }

          assertDecodeIs[Test](
            AvroValue.Record(Vector(AvroValue.Int(1))),
            Right(Test(1L)),
            Some(Schema.Record(Schema.Name("Test"), Vector(Schema.Field("value", Schema.Int))))
          )
        }

        it("should decode as case class") {
          assertDecodeIs[CaseClassTwoFields](
            unsafeEncode(CaseClassTwoFields("name", 123)),
            Right(CaseClassTwoFields("name", 123))
          )
        }

        it("should support None as default value") {
          case class Test(value: Option[Int])

          implicit val testCodec: Codec[Test] =
            Codec.record("Test", "") { field =>
              field("value", _.value, default = Some(None)).map(Test(_))
            }

          assertDecodeIs[Test](
            unsafeEncode(Test(None)),
            Right(Test(None))
          )
        }

        it("should support Some as default value") {
          case class Test(value: Option[Int])

          implicit val testCodec: Codec[Test] =
            Codec.record("Test", "") { field =>
              field("value", _.value, default = Some(Some(0)))(
                Codec.union(alt => alt[Some[Int]] ++ alt[None.type])
              ).map(Test(_))
            }

          assertDecodeIs[Test](
            unsafeEncode(Test(None)),
            Right(Test(None))
          )
        }

        it("should decode field with aliased name") {
          case class Aliased(aliasedField: Int)

          implicit val codec: Codec[Aliased] =
            Codec.record("CaseClassField", "") { field =>
              field("aliasedField", _.aliasedField, aliases = Seq("value")).map(Aliased(_))
            }

          assertDecodeIs[Aliased](
            unsafeEncode(CaseClassField(3)),
            Right(Aliased(3)),
            Some(unsafeSchema[CaseClassField])
          )
        }

        it("should decode references to named schemas in the writer schema") {
          case class Leaf(value: Int)

          implicit val leafCodec: Codec[Leaf] =
            Codec.record("Leaf", "") { field =>
              field("value", _.value).map(Leaf(_))
            }

          case class Node(value: Int, next: Option[Leaf])

          implicit val nodeCodec: Codec[Node] =
            Codec.record("Node", "") { field =>
              (field("value", _.value), field("next", _.next)).mapN(Node(_, _))
            }

          val recursive =
            Schema.Record(
              Schema.Name("Node"),
              Vector(
                Schema.Field("value", Schema.Int),
                Schema.Field(
                  "next",
                  Schema.Union(Vector(Schema.Null, Schema.Ref(Schema.Name("Node"))))
                )
              )
            )

          assertDecodeIs[Node](
            AvroValue.Record(
              Vector(
                AvroValue.Int(1),
                AvroValue.Union(
                  1,
                  AvroValue.Record(Vector(AvroValue.Int(2), AvroValue.Union(0, AvroValue.Null)))
                )
              )
            ),
            Right(Node(1, Some(Leaf(2)))),
            Some(recursive)
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
            Left(AvroException("error")),
            int => Right(AvroValue.Int(int)),
            _ => Left(AvroException("error"))
          )

        assert {
          codec.show == "vulcan.AvroException: error"
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

        it("should decode a slice of bytes as string") {
          assertDecodeIs[String](
            AvroValue.Bytes(ByteVector("xabcx".getBytes(StandardCharsets.UTF_8)).slice(1, 4)),
            Right("abc"),
            Some(unsafeSchema[ByteVector])
          )
        }
      }
    }

    describe("union") {
      def enumCodec(
        name: String,
        namespace: String,
        aliases: Seq[String] = Seq.empty
      ): Codec[String] =
        Codec.enumeration[String](
          name = name,
          namespace = namespace,
          symbols = List("A", "B"),
          encode = identity,
          decode = Right(_),
          aliases = aliases
        )

      def unionOf(first: Codec[String], second: Codec[String]): Codec[Either[String, String]] =
        Codec.union[Either[String, String]] { alt =>
          alt[Left[String, String]](Codec.left(first), Prism.left) ++
            alt[Right[String, String]](Codec.right(second), Prism.right)
        }

      def enumSchema(name: String, namespace: String): Schema =
        Schema.Enum(Schema.Name(name, Some(namespace)), Vector("A", "B"))

      describe("schema") {
        it("should encode records as union") {
          assertSchemaIs[SealedTraitCaseClass] {
            """[{"type":"record","name":"FirstInSealedTraitCaseClass","namespace":"com.example","fields":[{"name":"value","type":"int"}]},{"type":"record","name":"SecondInSealedTraitCaseClass","namespace":"com.example","fields":[{"name":"value","type":"string"}]},{"type":"array","items":"int"}]"""
          }
        }

        it("should encode as union") {
          assertSchemaIs[Either[String, String]] {
            """[{"type":"enum","name":"Suit","namespace":"a","symbols":["A","B"]},{"type":"enum","name":"Suit","namespace":"b","symbols":["A","B"]}]"""
          }(unionOf(enumCodec("Suit", "a"), enumCodec("Suit", "b")))
        }

        it("should capture errors on nested unions") {
          assertSchemaError[SealedTraitCaseClassNestedUnion] {
            """Unions cannot directly contain other unions: [["null","int"]]"""
          }
        }

        it("should capture errors on duplicate types") {
          assertSchemaError[Either[Int, Int]] {
            """Duplicate int in union ["int","int"]"""
          }
        }
      }

      describe("encode") {
        it("should error if subtype is not an alternative") {
          assertEncodeError[SealedTraitCaseClassIncomplete](
            SecondInSealedTraitCaseClassIncomplete(0d),
            "Exhausted alternatives for union"
          )
        }

        it("should error if subtype is not an alternative and value null") {
          assertEncodeError[SealedTraitCaseClassIncomplete](
            null,
            "Exhausted alternatives for union"
          )
        }

        it("should encode with encoder for alternative") {
          assertEncodeIs[SealedTraitCaseClassIncomplete](
            FirstInSealedTraitCaseClassIncomplete(0),
            Right(AvroValue.Union(0, AvroValue.Int(0)))
          )
        }
      }

      describe("decode") {
        it("should error if there is no alternative for the written type") {
          assertDecodeError[Either[Int, String]](
            AvroValue.Boolean(true),
            Schema.Boolean,
            "Error decoding Either: Missing alternative for boolean in union"
          )
        }

        it("should decode using the alternative for the written type") {
          assertDecodeIs[Either[Int, String]](
            AvroValue.Union(1, AvroValue.Int(1)),
            Right(Left(1)),
            Some(Schema.Union(Vector(Schema.String, Schema.Int)))
          )
        }

        it("should decode written types which are not unions") {
          assertDecodeIs[Either[Int, String]](
            AvroValue.String("a"),
            Right(Right("a")),
            Some(Schema.String)
          )
        }

        it("should decode using the alternative of the same type before promotion") {
          assertDecodeIs[Either[Double, Long]](
            AvroValue.Long(1L),
            Right(Right(1L)),
            Some(Schema.Long)
          )
        }

        it("should decode using promotion if there is no alternative of the same type") {
          assertDecodeIs[Either[Long, String]](
            AvroValue.Int(1),
            Right(Left(1L)),
            Some(Schema.Int)
          )
        }

        it("should return the error of the alternative for the written type") {
          assertDecodeError[Either[Byte, String]](
            AvroValue.Int(1000),
            Schema.Int,
            "Error decoding Either: Error decoding Byte: Got unexpected Int value 1000, expected value in range -128 to 127"
          )
        }

        it("should decode named types using the full name") {
          assertDecodeIs[Either[String, String]](
            AvroValue.Enum("A"),
            Right(Right("A")),
            Some(enumSchema("Suit", "b"))
          )(unionOf(enumCodec("Suit", "a"), enumCodec("Suit", "b")))
        }

        it("should decode named types using an alias") {
          assertDecodeIs[Either[String, String]](
            AvroValue.Enum("A"),
            Right(Right("A")),
            Some(enumSchema("Old", "a"))
          )(unionOf(enumCodec("Card", "a"), enumCodec("Suit", "b", aliases = List("a.Old"))))
        }

        it("should decode named types using the name if there is no full name") {
          assertDecodeIs[Either[String, String]](
            AvroValue.Enum("A"),
            Right(Right("A")),
            Some(enumSchema("Suit", "c"))
          )(unionOf(enumCodec("Card", "a"), enumCodec("Suit", "b")))
        }

        it("should error if there is no named type of the same kind") {
          assertDecodeError[Either[String, String]](
            AvroValue.Fixed(hex"00"),
            Schema.Fixed(Schema.Name("Suit", Some("b")), 1),
            "Missing alternative for fixed b.Suit in union"
          )(unionOf(enumCodec("Card", "a"), enumCodec("Suit", "b")))
        }

        it("should error if schema is not in union") {
          assertDecodeError[SealedTraitCaseClass](
            AvroValue.String("value"),
            unsafeSchema[String],
            "Missing alternative for string in union"
          )
        }

        it("should decode if schema is part of union") {
          assertDecodeIs[SealedTraitCaseClass](
            unsafeEncode(FirstInSealedTraitCaseClass(0)),
            Right(FirstInSealedTraitCaseClass(0)),
            Some(unsafeSchema[FirstInSealedTraitCaseClass])
          )
        }

        it("should decode if schema is union with records of the same name") {
          assertDecodeIs[SealedTraitCaseClassSharedName](
            unsafeEncode(Second.SharedNameSealedTraitCaseClass("hello")),
            Right(Second.SharedNameSealedTraitCaseClass("hello")),
            Some(unsafeSchema[Second.SharedNameSealedTraitCaseClass])
          )
        }

        it("should error if value is not a union value") {
          assertDecodeError[SealedTraitCaseClass](
            AvroValue.Double(123d),
            unsafeSchema[SealedTraitCaseClass],
            "Invalid double value for union"
          )
        }

        it("should error if value is null and not an alternative") {
          assertDecodeError[SealedTraitCaseClass](
            AvroValue.Null,
            unsafeSchema[SealedTraitCaseClass],
            "Invalid null value for union"
          )
        }

        it("should error if no alternative with the name of the written type") {
          assertDecodeError[SealedTraitCaseClass](
            unsafeEncode(CaseClassInSealedTraitCaseClassSingle(0)),
            unsafeSchema[CaseClassInSealedTraitCaseClassSingle],
            "Missing alternative for record vulcan.examples.CaseClassInSealedTraitCaseClassSingle in union"
          )
        }

        it("should decode using schema and decoder for alternative") {
          assertDecodeIs[SealedTraitCaseClass](
            unsafeEncode[SealedTraitCaseClass](FirstInSealedTraitCaseClass(0)),
            Right(FirstInSealedTraitCaseClass(0))
          )
        }

        it("should decode using schema with aliased name") {
          implicit val secondCodec: Codec[SecondInSealedTraitCaseClass] =
            Codec.record(
              name = "AliasedInSealedTraitCaseClass",
              namespace = "com.example",
              aliases = Seq("SecondInSealedTraitCaseClass")
            ) { field =>
              field("value", _.value).map(SecondInSealedTraitCaseClass(_))
            }

          implicit val codec: Codec[SealedTraitCaseClass] =
            Codec.union { alt =>
              alt[FirstInSealedTraitCaseClass] ++
                alt[SecondInSealedTraitCaseClass] ++
                alt[ThirdInSealedTraitCaseClass]
            }

          assertDecodeIs[SealedTraitCaseClass](
            unsafeEncode[SealedTraitCaseClass](SecondInSealedTraitCaseClass("foo"))(
              SealedTraitCaseClass.sealedTraitCaseClassCodec
            ),
            Right(SecondInSealedTraitCaseClass("foo")),
            Some(SealedTraitCaseClass.sealedTraitCaseClassCodec.schema.value)
          )
        }
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

    describe("uuid") {
      val uuid = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")

      describe("schema") {
        it("should be encoded as string with logical type uuid") {
          assertSchemaIs[UUID] {
            """{"type":"string","logicalType":"uuid"}"""
          }
        }
      }

      describe("encode") {
        it("should encode as string") {
          val value = uuid
          assertEncodeIs[UUID](
            value,
            Right(AvroValue.String(value.toString))
          )
        }
      }

      describe("decode") {
        it("should error if logical type is not uuid") {
          assertDecodeError[UUID](
            unsafeEncode(uuid),
            unsafeSchema[String],
            "Error decoding UUID: Got unexpected missing logical type"
          )
        }

        it("should error if value is not uuid") {
          assertDecodeError[UUID](
            AvroValue.String("not-uuid"),
            unsafeSchema[UUID],
            "Error decoding UUID: Invalid UUID string: not-uuid"
          )
        }

        it("should decode uuid") {
          val value = uuid
          assertDecodeIs[UUID](
            unsafeEncode(value),
            Right(value)
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
    codec.schema.flatMap(codec.decoder).flatMap(_.decode(value)).value

  def assertSchemaIs[A](expectedSchema: String)(implicit codec: Codec[A]): Assertion =
    assert(codec.schema.value.toString == expectedSchema)

  def assertEncodeIs[A](
    a: A,
    encoded: Either[AvroException, AvroValue]
  )(implicit codec: Codec[A]): Assertion =
    assert(codec.encode(a).value == encoded.value)

  def assertDecodeIs[A](
    value: AvroValue,
    decoded: Either[AvroException, A],
    schema: Option[Schema] = None
  )(implicit codec: Codec[A]): Assertion = {
    val decode =
      schema
        .map(codec.decoder(_).flatMap(_.decode(value)).value)
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
    assert(codec.decoder(schema).flatMap(_.decode(value)).swap.value.message == expectedErrorMessage)

  def assertEncodeError[A](
    a: A,
    expectedErrorMessage: String
  )(implicit codec: Codec[A]): Assertion =
    assert(codec.encode(a).swap.value.message == expectedErrorMessage)
}
