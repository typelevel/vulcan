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
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scodec.bits.*
import vulcan.Codec.InstantMicros
import vulcan.examples.*

object CodecSpec extends BaseSuite {
  pureTest("Codec binaryDecoder should decode values using the writer schema") {
    val decoder = Codec[Long].binaryDecoder(Schema.Int)
    expect.same(Right(1L), decoder.flatMap(_.decode(hex"02"))) &&
    expect.same(Right(2L), decoder.flatMap(_.decode(hex"04")))
  }

  pureTest("Codec binaryDecoder should decode values of unions using the written type") {
    val decoder = Codec[Long].binaryDecoder(Schema.Union(Vector(Schema.Int, Schema.Long)))
    expect.same(Right(1L), decoder.flatMap(_.decode(hex"0002"))) &&
    expect.same(Right(2L), decoder.flatMap(_.decode(hex"0204")))
  }

  pureTest("Codec binaryDecoder should ignore remaining bytes") {
    val decoder = Codec[Int].binaryDecoder(Schema.Int)
    expect.same(Right(1), decoder.flatMap(_.decode(hex"0204")))
  }

  pureTest("Codec binaryDecoder should decode zero bytes as null") {
    val decoder = Codec[Unit].binaryDecoder(Schema.Null)
    expect.same(Right(()), decoder.flatMap(_.decode(ByteVector.empty)))
  }

  pureTest("Codec binaryDecoder should decode items promoted to types with larger encodings") {
    val bytes = Codec[List[Int]].binaryEncoder.flatMap(_.encode(List(0, 1, Int.MaxValue)))
    val decoder = Codec[List[Float]].binaryDecoder(Schema.Array(Schema.Int))
    expect.same(Right(List(0f, 1f, Int.MaxValue.toFloat)), (decoder, bytes).flatMapN(_.decode(_)))
  }

  pureTest("Codec binaryDecoder should error if the writer schema is invalid") {
    val result = Codec[Int].binaryDecoder(Schema.Union(Vector(Schema.Int, Schema.Int)))
    expect.same(Left("""Duplicate int in union ["int","int"]"""), result.left.map(_.message))
  }

  pureTest("Codec binaryDecoder should error if the schema is invalid") {
    val result = Codec.decimal(precision = -1, scale = -1).binaryDecoder(Schema.Bytes)
    expect.same(Left("Invalid decimal precision -1: must be positive"), result.left.map(_.message))
  }

  pureTest("Codec binaryDecoder should error if the binary does not match the writer schema") {
    val decoder = Codec[Int].binaryDecoder(Schema.Int)
    expect.same(
      Left("Unexpected end of input at position 0"),
      decoder.flatMap(_.decode(ByteVector.empty)).left.map(_.message)
    )
  }

  pureTest("Codec binaryDecoder should error if the writer schema does not match the type") {
    val decoder = Codec[Int].binaryDecoder(Schema.Long)
    expect.same(
      Left("Error decoding Int: Got unexpected schema type long, expected schema type int"),
      decoder.flatMap(_.decode(hex"02")).left.map(_.message)
    )
  }

  pureTest("Codec binaryEncoder should encode to avro binary") {
    val encoder = Codec[Int].binaryEncoder
    expect.same(Right(hex"02"), encoder.flatMap(_.encode(1))) &&
    expect.same(Right(hex"04"), encoder.flatMap(_.encode(2)))
  }

  pureTest("Codec boolean schema should be encoded as boolean") {
    expectSchema[Boolean] {
      """"boolean""""
    }
  }

  pureTest("Codec boolean encode should encode as boolean") {
    expectEncode[Boolean](
      true,
      AvroValue.Boolean(true)
    )
  }

  pureTest("Codec boolean decode should error if schema is not boolean") {
    expectDecodeError[Boolean](
      Codec[Boolean].encode(false),
      Codec[Long].schema,
      "Error decoding Boolean: Got unexpected schema type long, expected schema type boolean"
    )
  }

  pureTest("Codec boolean decode should error if value is not boolean") {
    expectDecodeError[Boolean](
      Codec[Long].encode(10L),
      "Error decoding Boolean: Invalid long value for boolean"
    )
  }

  pureTest("Codec boolean decode should error if value is null") {
    expectDecodeError[Boolean](
      Right(AvroValue.Null),
      "Error decoding Boolean: Invalid null value for boolean"
    )
  }

  pureTest("Codec boolean decode should decode as Boolean") {
    val value = false
    expectDecode[Boolean](
      Codec[Boolean].encode(value),
      value
    )
  }

  pureTest("Codec byte schema should be encoded as int") {
    expectSchema[Byte] {
      """"int""""
    }
  }

  pureTest("Codec byte encode should encode as int") {
    val value = 1.toByte
    expectEncode[Byte](
      value,
      AvroValue.Int(1)
    )
  }

  pureTest("Codec byte decode should error if schema is not int") {
    expectDecodeError[Byte](
      Codec[Byte].encode(1.toByte),
      Codec[String].schema,
      "Error decoding Byte: Error decoding Int: Got unexpected schema type string, expected schema type int"
    )
  }

  pureTest("Codec byte decode should error if value is not int") {
    expectDecodeError[Byte](
      Codec[String].encode("value"),
      "Error decoding Byte: Error decoding Int: Invalid string value for int"
    )
  }

  test("Codec byte decode should error if int value is not byte") {
    val gen =
      Gen.oneOf(
        Gen.chooseNum(Int.MinValue, Byte.MinValue.toInt - 1),
        Gen.chooseNum(Byte.MaxValue.toInt + 1, Int.MaxValue)
      )

    forall(gen) { nonByte =>
      expectDecodeError[Byte](
        Codec[Int].encode(nonByte),
        s"Error decoding Byte: Got unexpected Int value $nonByte, expected value in range -128 to 127"
      )
    }
  }

  test("Codec byte decode should decode as byte") {
    forall { (byte: Byte) =>
      expectDecode[Byte](
        Codec[Byte].encode(byte),
        byte
      )
    }
  }

  pureTest("Codec bytes schema should be encoded as bytes") {
    expectSchema[ByteVector] {
      """"bytes""""
    }
  }

  pureTest("Codec bytes encode should encode as bytes") {
    expectEncode[ByteVector](
      hex"01",
      AvroValue.Bytes(hex"01")
    )
  }

  pureTest("Codec bytes decode should error on schema other than bytes or string") {
    expectDecodeError[ByteVector](
      Codec[ByteVector].encode(hex"01"),
      Codec[Int].schema,
      "Error decoding ByteVector: Got unexpected schema type int, expected schema type bytes"
    )
  }

  pureTest("Codec bytes decode should error if value is not bytes") {
    expectDecodeError[ByteVector](
      Right(AvroValue.Int(10)),
      "Error decoding ByteVector: Invalid int value for bytes"
    )
  }

  pureTest("Codec bytes decode should decode string as bytes") {
    expectDecode[ByteVector](
      Codec[String].encode("foo"),
      Codec[String].schema,
      ByteVector("foo".getBytes(StandardCharsets.UTF_8))
    )
  }

  pureTest("Codec bytes decode should decode bytes as bytes") {
    val bytes =
      ByteVector("c6e6b318-303c-4ffa-8c0b-c689ddc49e94".getBytes(StandardCharsets.UTF_8))
    expectDecode[ByteVector](
      Right(AvroValue.Bytes(bytes)),
      bytes
    )
  }

  pureTest("Codec chain schema should be encoded as array") {
    expectSchema[Chain[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec chain encode should encode as array using encoder for underlying type") {
    expectEncode[Chain[Int]](
      Chain(1, 2, 3),
      AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2), AvroValue.Int(3)))
    )
  }

  pureTest("Codec chain decode should error if schema is not array") {
    expectDecodeError[Chain[Int]](
      Codec[Chain[Int]].encode(Chain(1, 2, 3)),
      Codec[Int].schema,
      "Error decoding Chain: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec chain decode should error if value is not array") {
    expectDecodeError[Chain[Int]](
      Codec[Int].encode(10),
      "Error decoding Chain: Invalid int value for array"
    )
  }

  pureTest("Codec chain decode should decode as Chain") {
    val value = Chain(1, 2, 3)
    expectDecode[Chain[Int]](
      Codec[Chain[Int]].encode(value),
      value
    )
  }

  pureTest("Codec char schema should be encoded as string") {
    expectSchema[Char] {
      """"string""""
    }
  }

  pureTest("Codec char encode should encode as string") {
    val value = 'a'
    expectEncode[Char](
      value,
      AvroValue.String("a")
    )
  }

  pureTest("Codec char decode should error if string value is empty") {
    expectDecodeError[Char](
      Codec[String].encode(""),
      Codec[String].schema,
      "Error decoding Char: Got unexpected String with length 0, expected length 1"
    )
  }

  pureTest("Codec char decode should error if string value has more than 1 char") {
    expectDecodeError[Char](
      Codec[String].encode("ab"),
      Codec[String].schema,
      "Error decoding Char: Got unexpected String with length 2, expected length 1"
    )
  }

  test("Codec char decode should decode as char") {
    forall { (char: Char) =>
      expectDecode[Char](
        Codec[Char].encode(char),
        char
      )
    }
  }

  pureTest("Codec decimal schema should be encoded as bytes with logical type decimal") {
    implicit val codec: Codec[BigDecimal] =
      Codec.decimal(precision = 10, scale = 5)

    expectSchema[BigDecimal] {
      """{"type":"bytes","logicalType":"decimal","precision":10,"scale":5}"""
    }
  }

  pureTest("Codec decimal schema should error if precision is not positive") {
    implicit val codec: Codec[BigDecimal] =
      Codec.decimal(precision = -1, scale = -1)

    expectSchemaError[BigDecimal] {
      "Invalid decimal precision -1: must be positive"
    }
  }

  pureTest("Codec decimal schema should error if scale is negative") {
    implicit val codec: Codec[BigDecimal] =
      Codec.decimal(precision = 5, scale = -1)

    expectSchemaError[BigDecimal] {
      "Invalid decimal scale -1: cannot be negative"
    }
  }

  pureTest("Codec decimal schema should error if scale exceeds precision") {
    implicit val codec: Codec[BigDecimal] =
      Codec.decimal(precision = 5, scale = 6)

    expectSchemaError[BigDecimal] {
      "Invalid decimal scale 6: cannot exceed precision 5"
    }
  }

  locally {
    implicit val codec: Codec[BigDecimal] =
      Codec.decimal(precision = 10, scale = 5)

    pureTest("Codec decimal encode should error if scale is different in schema") {
      expectEncodeError[BigDecimal](
        BigDecimal("123"),
        "Error encoding BigDecimal: Unable to encode decimal with scale 0 as scale 5"
      )
    }

    pureTest("Codec decimal encode should error if precision exceeds schema precision") {
      expectEncodeError[BigDecimal](
        BigDecimal("123456.45678"),
        "Error encoding BigDecimal: Unable to encode decimal with precision 11 exceeding schema precision 10"
      )
    }

    pureTest("Codec decimal encode should error if the schema is invalid") {
      implicit val codec: Codec[BigDecimal] =
        Codec.decimal(precision = -1, scale = -1)

      expectEncodeError[BigDecimal](
        BigDecimal("1"),
        "Error encoding BigDecimal: Invalid decimal precision -1: must be positive"
      )
    }

    pureTest("Codec decimal encode should encode as bytes") {
      val value = BigDecimal("123.45678")
      expectEncode[BigDecimal](
        value,
        AvroValue.Bytes(ByteVector(value.underlying.unscaledValue.toByteArray))
      )
    }
  }

  locally {
    implicit val codec: Codec[BigDecimal] =
      Codec.decimal(precision = 10, scale = 5)

    pureTest("Codec decimal decode should error if schema is not bytes") {
      expectDecodeError[BigDecimal](
        Codec[BigDecimal].encode(BigDecimal("123.45678")),
        Codec[String].schema,
        "Error decoding BigDecimal: Got unexpected schema type string, expected schema type bytes"
      )
    }

    pureTest("Codec decimal decode should error if value is not bytes") {
      expectDecodeError[BigDecimal](
        Codec[Int].encode(10),
        "Error decoding BigDecimal: Invalid int value for bytes"
      )
    }

    pureTest("Codec decimal decode should error if logical type is missing") {
      expectDecodeError[BigDecimal](
        Codec[BigDecimal].encode(BigDecimal("123.45678")),
        Right(Schema.Bytes),
        "Error decoding BigDecimal: Got unexpected missing logical type"
      )
    }

    pureTest("Codec decimal decode should error if logical type is not decimal") {
      expectDecodeError[BigDecimal](
        Codec[BigDecimal].encode(BigDecimal("123.45678")),
        Right(Schema.Bytes.withLogicalType(LogicalType.bigDecimal)),
        "Error decoding BigDecimal: Got unexpected logical type big-decimal"
      )
    }

    pureTest("Codec decimal decode should error if logical type is invalid") {
      val params = JsonObject("precision" -> Json.fromInt(2), "scale" -> Json.fromInt(3))

      expectDecodeError[BigDecimal](
        Codec[BigDecimal].encode(BigDecimal("123.45678")),
        Right(Schema.Bytes.withLogicalType(LogicalType("decimal", params))),
        "Error decoding BigDecimal: Got invalid logical type decimal for bytes"
      )
    }

    pureTest("Codec decimal decode should error if precision exceeds schema precision") {
      expectDecodeError[BigDecimal](
        Codec[BigDecimal].encode(BigDecimal("123.45678")),
        Right(Schema.Bytes.withLogicalType(LogicalType.decimal(6, 5))),
        "Error decoding BigDecimal: Unable to decode decimal with precision 8 exceeding schema precision 6"
      )
    }

    pureTest("Codec decimal decode should error if bytes are empty") {
      expectDecodeError[BigDecimal](
        Right(AvroValue.Bytes(ByteVector.empty)),
        "Error decoding BigDecimal: Unable to decode decimal from empty bytes"
      )
    }

    pureTest("Codec decimal decode should decode using the scale of the writer schema") {
      expectDecode[BigDecimal](
        Codec[BigDecimal].encode(BigDecimal("123.45678")),
        Right(Schema.Bytes.withLogicalType(LogicalType.decimal(10, 4))),
        BigDecimal("1234.5678")
      )
    }

    pureTest("Codec decimal decode should decode bytes") {
      val value = BigDecimal("123.45678")
      expectDecode[BigDecimal](
        Codec[BigDecimal].encode(value),
        value
      )
    }
  }

  test("Codec decoder should decode using codec for type") {
    val decoder = Codec[Int].decoder(Schema.Int)
    forall { (n: Int) =>
      expect.same(Right(n), decoder.flatMap(_.decode(AvroValue.Int(n))))
    }
  }

  pureTest("Codec decoder should error if the writer schema is invalid") {
    val result = Codec[Int].decoder(Schema.Union(Vector(Schema.Int, Schema.Int)))
    expect.same(Left("""Duplicate int in union ["int","int"]"""), result.left.map(_.message))
  }

  pureTest("Codec decoder should error if the schema is invalid") {
    val result = Codec.decimal(precision = -1, scale = -1).decoder(Schema.Bytes)
    expect.same(Left("Invalid decimal precision -1: must be positive"), result.left.map(_.message))
  }

  pureTest("Codec decoder should decode values of unions using the written type") {
    expectDecode[Long](
      Right(AvroValue.Union(1, AvroValue.Int(1))),
      Right(Schema.Union(Vector(Schema.Null, Schema.Int))),
      1L
    )
  }

  pureTest("Codec decoder should error if the written type of a union cannot be decoded") {
    expectDecodeError[Long](
      Right(AvroValue.Union(0, AvroValue.Null)),
      Right(Schema.Union(Vector(Schema.Null, Schema.Int))),
      "Error decoding Long: Got unexpected schema type null, expected schema type long"
    )
  }

  pureTest("Codec decoder should error if the index is invalid for the union") {
    expectDecodeError[Long](
      Right(AvroValue.Union(2, AvroValue.Int(1))),
      Right(Schema.Union(Vector(Schema.Null, Schema.Int))),
      "Invalid index 2 for union with 2 types"
    )
  }

  pureTest("Codec decoder should error if the value is not a union value") {
    expectDecodeError[Long](
      Right(AvroValue.Int(1)),
      Right(Schema.Union(Vector(Schema.Null, Schema.Int))),
      "Invalid int value for union"
    )
  }

  pureTest("Codec decoder should error if the writer schema references an undefined name") {
    expectDecodeError[Int](
      Right(AvroValue.Int(1)),
      Right(Schema.Ref(Schema.Name("Undefined", Some("com.example")))),
      "Undefined name com.example.Undefined"
    )
  }

  pureTest("Codec double schema should be encoded as double") {
    expectSchema[Double] {
      """"double""""
    }
  }

  pureTest("Codec double encode should encode as double") {
    val value = 123d
    expectEncode[Double](
      value,
      AvroValue.Double(value)
    )
  }

  pureTest("Codec double decode should error if schema is not double or promotable to double") {
    expectDecodeError[Double](
      Codec[Double].encode(123d),
      Codec[String].schema,
      "Error decoding Double: Got unexpected schema type string, expected schema type double"
    )
  }

  pureTest("Codec double decode should error if value is not double or promotable to double") {
    expectDecodeError[Double](
      Codec[String].encode("foo"),
      "Error decoding Double: Invalid string value for double"
    )
  }

  pureTest("Codec double decode should decode Float as Double") {
    expectDecode[Double](
      Codec[Float].encode(123f),
      Codec[Float].schema,
      123d
    )
  }

  pureTest("Codec double decode should decode Int as Double") {
    expectDecode[Double](
      Codec[Int].encode(123),
      Codec[Int].schema,
      123d
    )
  }

  pureTest("Codec double decode should decode Long as Double") {
    expectDecode[Double](
      Codec[Long].encode(123L),
      Codec[Long].schema,
      123d
    )
  }

  pureTest("Codec double decode should decode Double as Double") {
    val value = 123d
    expectDecode[Double](
      Codec[Double].encode(value),
      value
    )
  }

  pureTest("Codec either schema should be encoded as union") {
    expectSchema[Either[Int, Double]] {
      """["int","double"]"""
    }
  }

  pureTest("Codec either encode should encode left using left schema") {
    expectEncode[Either[Int, Double]](
      Left(1),
      AvroValue.Union(0, AvroValue.Int(1))
    )
  }

  pureTest("Codec either encode should encode right using right schema") {
    expectEncode[Either[Int, Double]](
      Right(1d),
      AvroValue.Union(1, AvroValue.Double(1d))
    )
  }

  pureTest("Codec either decode should error if schema type not in union") {
    expectDecodeError[Either[Int, Double]](
      Codec[Either[Int, Double]].encode(Right(1d)),
      Codec[String].schema,
      "Error decoding Either: Missing alternative for string in union"
    )
  }

  pureTest("Codec either decode should decode left value as left") {
    expectDecode[Either[Int, Double]](
      Codec[Either[Int, Double]].encode(Left(1)),
      Left(1)
    )
  }

  pureTest("Codec either decode should decode right value as right") {
    expectDecode[Either[Int, Double]](
      Codec[Either[Int, Double]].encode(Right(1d)),
      Right(1d)
    )
  }

  test("Codec encoder should encode using codec for type") {
    val encoder = Codec[Int].encoder
    forall { (n: Int) =>
      expect.same(Right(AvroValue.Int(n)), encoder.flatMap(_.encode(n)))
    }
  }

  locally {
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

    pureTest("Codec enum schema should have expected schema") {
      expectSchema[SealedTraitEnum] {
        """{"type":"enum","name":"SealedTraitEnum","namespace":"vulcan.examples","doc":"documentation","symbols":["first","second"],"default":"first","custom1":10,"custom2":"value2","aliases":["first","second"]}"""
      }
    }

    pureTest("Codec enum schema should capture errors on invalid schema") {
      expectSchemaError[`-SealedTraitEnumInvalidName`] {
        """Invalid name "-SealedTraitEnumInvalidName": must start with a letter or underscore"""
      }
    }

    pureTest("Codec enum schema should error if the default is not a symbol") {
      implicit val codec: Codec[String] =
        enumCodec(default = Some("third"))

      expectSchemaError[String] {
        """Default "third" is not a symbol of enum Enum"""
      }
    }

    pureTest("Codec enum schema should error if properties are reserved") {
      implicit val codec: Codec[String] =
        enumCodec(props = JsonObject("symbols" -> Json.fromString("first")))

      expectSchemaError[String] {
        """Invalid property "symbols" of enum Enum: name is reserved"""
      }
    }

    pureTest("Codec enum encode should error if returned string is not a schema symbol") {
      implicit val codec: Codec[SealedTraitEnum] =
        Codec.enumeration(
          name = "SealedTraitEnum",
          namespace = "vulcan.examples",
          symbols = List("symbol"),
          encode = _ => "not-symbol",
          decode = _ => Left(AvroException("error"))
        )

      expectEncodeError[SealedTraitEnum](
        FirstInSealedTraitEnum,
        "Error encoding vulcan.examples.SealedTraitEnum: Symbol not-symbol is not part of schema symbols [symbol]"
      )
    }

    pureTest("Codec enum encode should encode a valid symbol") {
      expectEncode[SealedTraitEnum](
        FirstInSealedTraitEnum,
        AvroValue.Enum("first")
      )
    }

    pureTest("Codec enum decode should error if schema type is not enum") {
      expectDecodeError[SealedTraitEnum](
        Codec[SealedTraitEnum].encode(FirstInSealedTraitEnum),
        Codec[String].schema,
        "Error decoding vulcan.examples.SealedTraitEnum: Got unexpected schema type string, expected schema type enum"
      )
    }

    pureTest("Codec enum decode should error if value is not enum") {
      expectDecodeError[SealedTraitEnum](
        Codec[Int].encode(10),
        "Error decoding vulcan.examples.SealedTraitEnum: Invalid int value for enum vulcan.examples.SealedTraitEnum"
      )
    }

    pureTest("Codec enum decode should return default value if encoded value is not a schema symbol") {
      expectDecode[SealedTraitEnum](
        Right(AvroValue.Enum("symbol")),
        Right(
          Schema.Enum(Schema.Name("SealedTraitEnum", Some("vulcan.examples")), Vector("symbol"))
        ),
        FirstInSealedTraitEnum
      )
    }

    pureTest(
      "Codec enum decode should error if encoded value is not a schema symbol and there is no default value"
    ) {
      expectDecodeError[SealedTraitEnumNoDefault](
        Right(AvroValue.Enum("symbol")),
        Right(
          Schema.Enum(
            Schema.Name("SealedTraitEnumNoDefault", Some("vulcan.examples")),
            Vector("symbol")
          )
        ),
        "Error decoding vulcan.examples.SealedTraitEnumNoDefault: symbol is not one of schema symbols [first, second]"
      )
    }

    pureTest("Codec enum decode should decode a valid symbol") {
      expectDecode[SealedTraitEnum](
        Codec[SealedTraitEnum].encode(FirstInSealedTraitEnum),
        FirstInSealedTraitEnum
      )
    }

    pureTest("Codec enum decode should decode a valid symbol with a different namespace") {
      expectDecode[SealedTraitEnum](
        Right(AvroValue.Enum("second")),
        Right(
          Schema.Enum(
            Schema.Name("SealedTraitEnum", Some("vulcan.examples.alt")),
            Vector("first", "second")
          )
        ),
        SecondInSealedTraitEnum
      )
    }

    pureTest("Codec enum decode should decode symbols by name, regardless of their order") {
      expectDecode[SealedTraitEnum](
        Right(AvroValue.Enum("first")),
        Right(
          Schema.Enum(
            Schema.Name("SealedTraitEnum", Some("vulcan.examples")),
            Vector("second", "first")
          )
        ),
        FirstInSealedTraitEnum
      )
    }
  }

  locally {
    def fixedCodec(size: Int): Codec[ByteVector] =
      Codec.fixed[ByteVector](
        name = "Name",
        namespace = "",
        size = size,
        encode = bytes => bytes,
        decode = bytes => Right(bytes)
      )

    pureTest("Codec fixed schema should have the expected schema") {
      expectSchema[FixedBoolean] {
        """{"type":"fixed","name":"FixedBoolean","namespace":"vulcan.examples","doc":"A boolean represented as a byte","size":1,"custom":"value","aliases":["SomeOtherBoolean"]}"""
      }
    }

    pureTest("Codec fixed schema should fail for negative size") {
      implicit val codec: Codec[ByteVector] =
        fixedCodec(size = -1)

      expectSchemaError[ByteVector] {
        "Invalid size -1 for fixed Name"
      }
    }

    pureTest("Codec fixed encode should error if length exceeds schema size") {
      implicit val codec: Codec[ByteVector] =
        fixedCodec(size = 1)

      expectEncodeError[ByteVector](
        hex"0001",
        "Error encoding Name: Got 2 bytes, expected maximum fixed size 1"
      )
    }

    pureTest("Codec fixed encode should encode as fixed") {
      expectEncode[FixedBoolean](
        FalseFixedBoolean,
        AvroValue.Fixed(hex"00")
      ) &&
      expectEncode[FixedBoolean](
        TrueFixedBoolean,
        AvroValue.Fixed(hex"01")
      )
    }

    pureTest("Codec fixed encode should pad bytes with zeros to the size") {
      implicit val codec: Codec[ByteVector] =
        fixedCodec(size = 3)

      expectEncode[ByteVector](
        hex"01",
        AvroValue.Fixed(hex"010000")
      )
    }

    pureTest("Codec fixed decode should error if schema is not fixed") {
      expectDecodeError[FixedBoolean](
        Codec[FixedBoolean].encode(FalseFixedBoolean),
        Codec[String].schema,
        "Error decoding vulcan.examples.FixedBoolean: Got unexpected schema type string, expected schema type fixed"
      )
    }

    pureTest("Codec fixed decode should error if value is not fixed") {
      expectDecodeError[FixedBoolean](
        Right(AvroValue.Int(123)),
        "Error decoding vulcan.examples.FixedBoolean: Invalid int value for fixed vulcan.examples.FixedBoolean"
      )
    }

    pureTest("Codec fixed decode should error if length does not match schema size") {
      expectDecodeError[FixedBoolean](
        Right(AvroValue.Fixed(hex"0001")),
        "Error decoding vulcan.examples.FixedBoolean: Got 2 bytes, expected fixed size 1"
      )
    }

    pureTest("Codec fixed decode should error if size of writer schema is different") {
      expectDecodeError[FixedBoolean](
        Right(AvroValue.Fixed(hex"0001")),
        Right(Schema.Fixed(Schema.Name("FixedBoolean", Some("vulcan.examples")), 2)),
        "Error decoding vulcan.examples.FixedBoolean: Got unexpected fixed size 2, expected fixed size 1"
      )
    }

    pureTest("Codec fixed decode should decode as fixed") {
      expectDecode[FixedBoolean](
        Codec[FixedBoolean].encode(FalseFixedBoolean),
        FalseFixedBoolean
      ) &&
      expectDecode[FixedBoolean](
        Codec[FixedBoolean].encode(TrueFixedBoolean),
        TrueFixedBoolean
      )
    }
  }

  pureTest("Codec float schema should be encoded as float") {
    expectSchema[Float] {
      """"float""""
    }
  }

  pureTest("Codec float encode should encode as float") {
    val value = 123f
    expectEncode[Float](
      value,
      AvroValue.Float(value)
    )
  }

  pureTest("Codec float decode should error if schema is not float or promotable to float") {
    expectDecodeError[Float](
      Codec[Float].encode(123f),
      Codec[String].schema,
      "Error decoding Float: Got unexpected schema type string, expected schema type float"
    )
  }

  pureTest("Codec float decode should error if value is not float or promotable to float") {
    expectDecodeError[Float](
      Codec[String].encode("foo"),
      "Error decoding Float: Invalid string value for float"
    )
  }

  pureTest("Codec float decode should decode Int as Float") {
    expectDecode[Float](
      Codec[Int].encode(123),
      Codec[Int].schema,
      123f
    )
  }

  pureTest("Codec float decode should decode Long as Float") {
    expectDecode[Float](
      Codec[Long].encode(123L),
      Codec[Long].schema,
      123f
    )
  }

  pureTest("Codec float decode should decode Float as Float") {
    val value = 123f
    expectDecode[Float](
      Codec[Float].encode(value),
      value
    )
  }

  locally {
    case class PosInt(value: Int)

    object PosInt {
      def apply(value: Int): Either[AvroException, PosInt] =
        if (value > 0) Right(new PosInt(value) {})
        else Left(AvroException(s"$value is not positive"))

      implicit val posIntCodec: Codec[PosInt] =
        Codec[Int].imapError(apply)(_.value)
    }

    pureTest("Codec imapError schema should use the schema of the underlying codec") {
      expectSchema[PosInt] {
        """"int""""
      }
    }

    pureTest("Codec imapError encode should encode using the underlying codec") {
      whenSuccess(PosInt(1)) { posInt =>
        expectEncode[PosInt](
          posInt,
          AvroValue.Int(1)
        )
      }
    }

    pureTest("Codec imapError decode should succeed for valid values") {
      whenSuccess(PosInt(1)) { posInt =>
        expectDecode[PosInt](
          Codec[Int].encode(1),
          posInt
        )
      }
    }

    pureTest("Codec imapError decode should error for invalid values") {
      expectDecodeError[PosInt](
        Codec[Int].encode(0),
        "0 is not positive"
      )
    }
  }

  locally {
    case class PosInt(value: Int)

    object PosInt {
      def apply(value: Int): Try[PosInt] =
        if (value > 0) Success(new PosInt(value) {})
        else Failure(new RuntimeException(s"$value is not positive"))

      implicit val posIntCodec: Codec[PosInt] =
        Codec[Int].imapTry(apply)(_.value)
    }

    pureTest("Codec imapTry schema should use the schema of the underlying codec") {
      expectSchema[PosInt] {
        """"int""""
      }
    }

    pureTest("Codec imapTry encode should encode using the underlying codec") {
      whenSuccess(PosInt(1)) { posInt =>
        expectEncode[PosInt](
          posInt,
          AvroValue.Int(1)
        )
      }
    }

    pureTest("Codec imapTry decode should succeed for valid values") {
      whenSuccess(PosInt(1)) { posInt =>
        expectDecode[PosInt](
          Codec[Int].encode(1),
          posInt
        )
      }
    }

    pureTest("Codec imapTry decode should error for invalid values") {
      expectDecodeError[PosInt](
        Codec[Int].encode(0),
        "java.lang.RuntimeException: 0 is not positive"
      )
    }
  }

  locally {
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

    pureTest("Codec instance schema should use the schema") {
      expect.same(Right(Schema.Int), codec.schema)
    }

    pureTest("Codec instance schema should error if the schema is invalid") {
      val invalid =
        Codec.instance[Int](
          Right(Schema.Fixed(Schema.Name("1Invalid"), 1)),
          int => Right(AvroValue.Int(int)),
          _ => Left(AvroException("error"))
        )

      expect.eql(
        Left("""Invalid name "1Invalid": must start with a letter or underscore"""),
        invalid.schema.left.map(_.message)
      )
    }

    pureTest("Codec instance encode should encode using the encoder") {
      expect.same(Right(AvroValue.Long(1L)), codec.encode(1))
    }

    pureTest("Codec instance decode should decode using the decoder function") {
      expect.same(Right(1), codec.decoder(Schema.Int).flatMap(_.decode(AvroValue.Int(1))))
    }

    pureTest("Codec instance decode should decode values of unions using the written type") {
      expect.same(
        Right(1),
        codec
          .decoder(Schema.Union(Vector(Schema.Null, Schema.Int)))
          .flatMap(_.decode(AvroValue.Union(1, AvroValue.Int(1))))
      )
    }

    pureTest("Codec instance decode should call the decoder function once for each writer schema") {
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

      val decoder = counting.decoder(Schema.Int)
      expect.same(Right(1), decoder.flatMap(_.decode(AvroValue.Int(1)))) &&
      expect.same(Right(1), decoder.flatMap(_.decode(AvroValue.Int(2)))) &&
      expect.eql(1, calls)
    }

    pureTest("Codec instance decode should include the definitions of referenced names in writer schemas") {
      val inner = Schema.Fixed(Schema.Name("Inner"), 1)

      def outer(innerSchema: Schema): Schema =
        Schema.Record(Schema.Name("Outer"), Vector(Schema.Field("inner", innerSchema)))

      val delegating =
        Codec.instance[Int](Right(Schema.Int), int => Right(AvroValue.Int(int)), codec.decoder)

      val decoder = delegating.decoder(Schema.Union(Vector(inner, outer(Schema.Ref(inner.name)))))
      val value = AvroValue.Union(1, AvroValue.Record(Vector(AvroValue.Fixed(hex"00"))))
      expect.same(
        Left(s"Got schema ${outer(inner)}"),
        decoder.flatMap(_.decode(value)).left.map(_.message)
      )
    }

    pureTest("Codec instance decode should error if the decoder function errors") {
      expect.same(
        Left(s"Got schema ${Schema.String}"),
        codec.decoder(Schema.String).flatMap(_.decode(AvroValue.String("a"))).left.map(_.message)
      )
    }
  }

  pureTest("Codec instant schema should be encoded as long with logical type timestamp-millis") {
    expectSchema[Instant] {
      """{"type":"long","logicalType":"timestamp-millis"}"""
    }
  }

  pureTest("Codec instant encode should encode as long") {
    val value = Instant.now()
    expectEncode[Instant](
      value,
      AvroValue.Long(value.toEpochMilli)
    )
  }

  pureTest("Codec instant encode should error if epoch milliseconds exceed the maximum long size") {
    expectEncodeError[Instant](
      Instant.MAX,
      s"Error encoding Instant: Unable to encode ${Instant.MAX} as epoch milliseconds exceeding the maximum long size"
    )
  }

  pureTest("Codec instant decode should error if schema is not long") {
    expectDecodeError[Instant](
      Codec[String].encode("value"),
      Codec[String].schema,
      "Error decoding Instant: Error decoding Long: Got unexpected schema type string, expected schema type long"
    )
  }

  pureTest("Codec instant decode should error if logical type is missing") {
    expectDecodeError[Instant](
      Codec[Instant].encode(Instant.now()),
      Codec[Long].schema,
      "Error decoding Instant: Got unexpected missing logical type"
    )
  }

  pureTest("Codec instant decode should error if logical type is not timestamp-millis") {
    expectDecodeError[Instant](
      Codec[Instant].encode(Instant.now()),
      Right(Schema.Long.withLogicalType(LogicalType.timestampMicros)),
      "Error decoding Instant: Got unexpected logical type timestamp-micros"
    )
  }

  pureTest("Codec instant decode should error if logical type is invalid") {
    expectDecodeError[Instant](
      Right(AvroValue.Int(1)),
      Right(Schema.Int.withLogicalType(LogicalType.timestampMillis)),
      "Error decoding Instant: Got invalid logical type timestamp-millis for int"
    )
  }

  pureTest("Codec instant decode should error if logical type is missing in nested writer schemas") {
    expectDecodeError[List[Instant]](
      Codec[List[Instant]].encode(List(Instant.now())),
      Right(Schema.Array(Schema.Long)),
      "Error decoding List: Error decoding Instant: Got unexpected missing logical type"
    )
  }

  pureTest("Codec instant decode should check the logical type of the written type of unions") {
    val value = Instant.ofEpochMilli(1767225600000L)

    expectDecode[Instant](
      Right(AvroValue.Union(1, AvroValue.Long(value.toEpochMilli))),
      Right(Schema.Union(Vector(Schema.Null, Schema.Long.withLogicalType(LogicalType.timestampMillis)))),
      value
    ) &&
    expectDecodeError[Instant](
      Right(AvroValue.Union(1, AvroValue.Long(value.toEpochMilli))),
      Right(Schema.Union(Vector(Schema.Null, Schema.Long))),
      "Error decoding Instant: Got unexpected missing logical type"
    )
  }

  pureTest("Codec instant decode should decode as Instant") {
    val value = Instant.ofEpochMilli(1767225600000L)
    expectDecode[Instant](
      Codec[Instant].encode(value),
      value
    )
  }

  pureTest("Codec instant micros schema should be encoded as long with logical type timestamp-micros") {
    expectSchema[InstantMicros] {
      """{"type":"long","logicalType":"timestamp-micros"}"""
    }
  }

  pureTest("Codec instant micros encode should encode as long") {
    val instantMicros = InstantMicros(Instant.now())
    expectEncode[InstantMicros](
      instantMicros,
      AvroValue.Long(instantMicros.toEpochMicro)
    )
  }

  pureTest("Codec instant micros encode should encode mid-term future Instant") {
    // Epoch 10_000_000_000, or 2286-11-20T17:46:40Z, exceeds Long.MaxValue in nanos
    val futureEpoch = 10L * 1000 * 1000 * 1000
    expectEncode[InstantMicros](
      InstantMicros(Instant.ofEpochSecond(futureEpoch, 0L)),
      AvroValue.Long(futureEpoch * 1000 * 1000)
    )
  }

  pureTest("Codec instant micros encode should error if epoch microseconds exceed the maximum long size") {
    expectEncodeError[InstantMicros](
      InstantMicros(Instant.MAX),
      s"Error encoding InstantMicros: Unable to encode ${Instant.MAX} as epoch microseconds exceeding the maximum long size"
    )
  }

  pureTest("Codec instant micros decode should error if logical type is missing") {
    expectDecodeError[InstantMicros](
      Codec[InstantMicros].encode(InstantMicros(Instant.now())),
      Codec[Long].schema,
      "Error decoding InstantMicros: Got unexpected missing logical type"
    )
  }

  pureTest("Codec instant micros decode should error if logical type is not timestamp-micros") {
    expectDecodeError[InstantMicros](
      Codec[InstantMicros].encode(InstantMicros(Instant.now())),
      Right(Schema.Long.withLogicalType(LogicalType.timestampMillis)),
      "Error decoding InstantMicros: Got unexpected logical type timestamp-millis"
    )
  }

  pureTest("Codec instant micros decode should truncate Instant to micros") {
    val instant = Instant.now()
    expectDecode[InstantMicros](
      Codec[InstantMicros].encode(InstantMicros(instant)),
      InstantMicros(instant.truncatedTo(ChronoUnit.MICROS))
    )
  }

  pureTest("Codec instant micros decode should roundtrip negative Instant with microsecond offsets") {
    // 1969-12-31T23:59:59.999999Z is one microsecond before the epoch
    val instantMicros = InstantMicros(Instant.ofEpochSecond(-1L, 999999L * 1000))
    expectDecode[InstantMicros](
      Codec[InstantMicros].encode(instantMicros),
      instantMicros
    )
  }

  pureTest("Codec instant micros decode should roundtrip the minimum and maximum epoch microseconds") {
    forEach(List(Long.MinValue, Long.MaxValue)) { micros =>
      val decoded =
        Codec[InstantMicros].schema
          .flatMap(Codec[InstantMicros].decoder)
          .flatMap(_.decode(AvroValue.Long(micros)))
      expect.same(Right(AvroValue.Long(micros)), decoded.flatMap(Codec[InstantMicros].encode))
    }
  }

  pureTest("Codec int schema should be encoded as int") {
    expectSchema[Int] {
      """"int""""
    }
  }

  pureTest("Codec int encode should encode as int") {
    val value = 123
    expectEncode[Int](
      value,
      AvroValue.Int(value)
    )
  }

  pureTest("Codec int decode should error if schema is not int") {
    expectDecodeError[Int](
      Codec[Int].encode(123),
      Codec[Long].schema,
      "Error decoding Int: Got unexpected schema type long, expected schema type int"
    )
  }

  pureTest("Codec int decode should error if value is not int") {
    expectDecodeError[Int](
      Codec[Long].encode(10L),
      "Error decoding Int: Invalid long value for int"
    )
  }

  pureTest("Codec int decode should decode as Int") {
    val value = 123
    expectDecode[Int](
      Codec[Int].encode(value),
      value
    )
  }

  pureTest("Codec jsonDecoder should decode values using the writer schema") {
    val decoder = Codec[Long].jsonDecoder(Schema.Int)
    expect.same(Right(1L), decoder.flatMap(_.decode(Json.fromInt(1)))) &&
    expect.same(Right(2L), decoder.flatMap(_.decode(Json.fromInt(2))))
  }

  pureTest("Codec jsonDecoder should error if the writer schema is invalid") {
    val result = Codec[Int].jsonDecoder(Schema.Union(Vector(Schema.Int, Schema.Int)))
    expect.same(Left("""Duplicate int in union ["int","int"]"""), result.left.map(_.message))
  }

  pureTest("Codec jsonDecoder should error if the schema is invalid") {
    val result = Codec.decimal(precision = -1, scale = -1).jsonDecoder(Schema.Bytes)
    expect.same(Left("Invalid decimal precision -1: must be positive"), result.left.map(_.message))
  }

  pureTest("Codec jsonDecoder should error if the json does not match the writer schema") {
    val decoder = Codec[Int].jsonDecoder(Schema.Int)
    expect.same(
      Left("Invalid string for int"),
      decoder.flatMap(_.decode(Json.fromString("badValue"))).left.map(_.message)
    )
  }

  pureTest("Codec jsonDecoder should error if the writer schema does not match the type") {
    val decoder = Codec[Int].jsonDecoder(Schema.String)
    expect.same(
      Left("Error decoding Int: Got unexpected schema type string, expected schema type int"),
      decoder.flatMap(_.decode(Json.fromString("1"))).left.map(_.message)
    )
  }

  pureTest("Codec jsonEncoder should encode to avro json") {
    val encoder = Codec[Int].jsonEncoder
    expect.same(Right(Json.fromInt(1)), encoder.flatMap(_.encode(1))) &&
    expect.same(Right(Json.fromInt(2)), encoder.flatMap(_.encode(2)))
  }

  pureTest("Codec list schema should be encoded as array") {
    expectSchema[List[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec list encode should encode as array using encoder for underlying type") {
    expectEncode[List[Int]](
      List(1, 2, 3),
      AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2), AvroValue.Int(3)))
    )
  }

  pureTest("Codec list encode should error if an item cannot be encoded") {
    implicit val intCodec: Codec[Int] =
      Codec.int.imapErrors(Right(_))(_ => Left(AvroException("error")))

    expectEncodeError[List[Int]](
      List(1, 2, 3),
      "Error encoding List: error"
    )
  }

  pureTest("Codec list decode should error if schema is not array") {
    expectDecodeError[List[Int]](
      Codec[List[Int]].encode(List(1, 2, 3)),
      Codec[Int].schema,
      "Error decoding List: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec list decode should error if value is not array") {
    expectDecodeError[List[Int]](
      Codec[Int].encode(10),
      "Error decoding List: Invalid int value for array"
    )
  }

  pureTest("Codec list decode should error if an item cannot be decoded") {
    expectDecodeError[List[Int]](
      Right(AvroValue.Array(Vector(AvroValue.Long(1L)))),
      Right(Schema.Array(Schema.Long)),
      "Error decoding List: Error decoding Int: Got unexpected schema type long, expected schema type int"
    )
  }

  pureTest("Codec list decode should decode items using the writer schema") {
    expectDecode[List[Long]](
      Right(AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2)))),
      Right(Schema.Array(Schema.Int)),
      List(1L, 2L)
    )
  }

  pureTest("Codec list decode should decode empty arrays even if items cannot be decoded") {
    expectDecode[List[Int]](
      Right(AvroValue.Array(Vector.empty)),
      Right(Schema.Array(Schema.String)),
      List.empty
    )
  }

  pureTest("Codec list decode should decode as List") {
    val value = List(1, 2, 3)
    expectDecode[List[Int]](
      Codec[List[Int]].encode(value),
      value
    )
  }

  pureTest("Codec localDate schema should be encoded as int with logical type date") {
    expectSchema[LocalDate] {
      """{"type":"int","logicalType":"date"}"""
    }
  }

  pureTest("Codec localDate encode should encode as int") {
    val value = LocalDate.now(ZoneOffset.UTC)
    expectEncode[LocalDate](
      value,
      AvroValue.Int(value.toEpochDay.toInt)
    )
  }

  pureTest("Codec localDate encode should error if epoch days exceed the maximum integer size") {
    val value = LocalDate.MAX
    expectEncodeError[LocalDate](
      value,
      s"Error encoding LocalDate: Unable to encode date as epoch days of ${value.toEpochDay} exceeds the maximum integer size"
    )
  }

  pureTest("Codec localDate decode should error if logical type is not date") {
    expectDecodeError[LocalDate](
      Codec[LocalDate].encode(LocalDate.now(ZoneOffset.UTC)),
      Codec[Int].schema,
      "Error decoding LocalDate: Got unexpected missing logical type"
    )
  }

  pureTest("Codec localDate decode should decode int as local date") {
    val value = LocalDate.now(ZoneOffset.UTC)
    expectDecode[LocalDate](
      Codec[LocalDate].encode(value),
      value
    )
  }

  pureTest("Codec localDateTime schema should be encoded as long with logical type local-timestamp-millis") {
    expectSchema[LocalDateTime] {
      """{"type":"long","logicalType":"local-timestamp-millis"}"""
    }
  }

  pureTest("Codec localDateTime encode should encode as long") {
    val value = LocalDateTime.ofInstant(Instant.ofEpochMilli(1767225600000L), ZoneOffset.UTC)
    expectEncode[LocalDateTime](
      value,
      AvroValue.Long(1767225600000L)
    )
  }

  pureTest("Codec localDateTime encode should error if epoch milliseconds exceed the maximum long size") {
    expectEncodeError[LocalDateTime](
      LocalDateTime.MAX,
      s"Error encoding LocalDateTime: Unable to encode ${LocalDateTime.MAX} as epoch milliseconds exceeding the maximum long size"
    )
  }

  pureTest("Codec localDateTime decode should error if logical type is missing") {
    expectDecodeError[LocalDateTime](
      Codec[LocalDateTime].encode(LocalDateTime.now(ZoneOffset.UTC)),
      Codec[Long].schema,
      "Error decoding LocalDateTime: Got unexpected missing logical type"
    )
  }

  pureTest("Codec localDateTime decode should error if logical type is not local-timestamp-millis") {
    expectDecodeError[LocalDateTime](
      Codec[LocalDateTime].encode(LocalDateTime.now(ZoneOffset.UTC)),
      Right(Schema.Long.withLogicalType(LogicalType.timestampMillis)),
      "Error decoding LocalDateTime: Got unexpected logical type timestamp-millis"
    )
  }

  pureTest("Codec localDateTime decode should decode as LocalDateTime") {
    val value = LocalDateTime.ofInstant(Instant.ofEpochMilli(1767225600000L), ZoneOffset.UTC)
    expectDecode[LocalDateTime](
      Codec[LocalDateTime].encode(value),
      value
    )
  }

  locally {
    implicit val codec: Codec[LocalTime] = Codec.localTimeMicros

    pureTest("Codec localTimeMicros schema should be encoded as long with logical type time-micros") {
      expectSchema[LocalTime] {
        """{"type":"long","logicalType":"time-micros"}"""
      }
    }

    pureTest("Codec localTimeMicros encode should encode as long") {
      val value = LocalTime.now(ZoneOffset.UTC)
      expectEncode[LocalTime](
        value,
        AvroValue.Long(value.toNanoOfDay / 1000L)
      )
    }

    pureTest("Codec localTimeMicros decode should error if logical type is not time-micros") {
      expectDecodeError[LocalTime](
        Codec[LocalTime].encode(LocalTime.now(ZoneOffset.UTC)),
        Codec[Long].schema,
        "Error decoding LocalTime: Got unexpected missing logical type"
      )
    }

    pureTest("Codec localTimeMicros decode should error if value is outside of the day") {
      forEach(List(-1L, 86400000000L)) { micros =>
        expectDecodeError[LocalTime](
          Right(AvroValue.Long(micros)),
          s"Error decoding LocalTime: Got unexpected Long value $micros, expected value in range 0 to 86399999999"
        )
      }
    }

    pureTest("Codec localTimeMicros decode should decode long as local time-micros") {
      val value = LocalTime.now(ZoneOffset.UTC)
      expectDecode[LocalTime](
        Codec[LocalTime].encode(value),
        value.truncatedTo(ChronoUnit.MICROS)
      )
    }
  }

  locally {
    implicit val codec: Codec[LocalTime] = Codec.localTimeMillis

    pureTest("Codec localTimeMillis schema should be encoded as int with logical type time-millis") {
      expectSchema[LocalTime] {
        """{"type":"int","logicalType":"time-millis"}"""
      }
    }

    pureTest("Codec localTimeMillis encode should encode as int") {
      val value = LocalTime.now(ZoneOffset.UTC)
      expectEncode[LocalTime](
        value,
        AvroValue.Int((value.toNanoOfDay / 1000000L).toInt)
      )
    }

    pureTest("Codec localTimeMillis decode should error if logical type is not time-millis") {
      expectDecodeError[LocalTime](
        Codec[LocalTime].encode(LocalTime.now(ZoneOffset.UTC)),
        Codec[Int].schema,
        "Error decoding LocalTime: Got unexpected missing logical type"
      )
    }

    pureTest("Codec localTimeMillis decode should error if value is outside of the day") {
      forEach(List(-1, 86400000)) { millis =>
        expectDecodeError[LocalTime](
          Right(AvroValue.Int(millis)),
          s"Error decoding LocalTime: Got unexpected Int value $millis, expected value in range 0 to 86399999"
        )
      }
    }

    pureTest("Codec localTimeMillis decode should decode int as local time-millis") {
      val value = LocalTime.now(ZoneOffset.UTC)
      expectDecode[LocalTime](
        Codec[LocalTime].encode(value),
        value.truncatedTo(ChronoUnit.MILLIS)
      )
    }
  }

  pureTest("Codec long schema should be encoded as long") {
    expectSchema[Long] {
      """"long""""
    }
  }

  pureTest("Codec long encode should encode as long") {
    val value = 123L
    expectEncode[Long](
      value,
      AvroValue.Long(value)
    )
  }

  pureTest("Codec long decode should error if schema is not int or long") {
    expectDecodeError[Long](
      Codec[Long].encode(123L),
      Codec[String].schema,
      "Error decoding Long: Got unexpected schema type string, expected schema type long"
    )
  }

  pureTest("Codec long decode should error if value is not int or long") {
    expectDecodeError[Long](
      Codec[Double].encode(123.0),
      "Error decoding Long: Invalid double value for long"
    )
  }

  pureTest("Codec long decode should decode int as long") {
    expectDecode[Long](
      Codec[Int].encode(123),
      Codec[Int].schema,
      123L
    )
  }

  pureTest("Codec long decode should decode long as long") {
    val value = 123L
    expectDecode[Long](
      Codec[Long].encode(value),
      value
    )
  }

  pureTest("Codec map schema should be encoded as map") {
    expectSchema[Map[String, Int]] {
      """{"type":"map","values":"int"}"""
    }
  }

  pureTest("Codec map encode should encode as map using encoder for value") {
    expectEncode[Map[String, Int]](
      Map("key" -> 1),
      AvroValue.Map(Vector("key" -> AvroValue.Int(1)))
    )
  }

  pureTest("Codec map decode should error if schema is not map") {
    expectDecodeError[Map[String, Int]](
      Codec[Map[String, Int]].encode(Map("key" -> 1)),
      Codec[Int].schema,
      "Error decoding Map: Got unexpected schema type int, expected schema type map"
    )
  }

  pureTest("Codec map decode should error if value is not map") {
    expectDecodeError[Map[String, Int]](
      Codec[Int].encode(123),
      "Error decoding Map: Invalid int value for map"
    )
  }

  pureTest("Codec map decode should decode values using the writer schema") {
    expectDecode[Map[String, Long]](
      Right(AvroValue.Map(Vector("key" -> AvroValue.Int(1)))),
      Right(Schema.Map(Schema.Int)),
      Map("key" -> 1L)
    )
  }

  pureTest("Codec map decode should decode the last value for duplicate keys") {
    expectDecode[Map[String, Int]](
      Right(AvroValue.Map(Vector("key" -> AvroValue.Int(1), "key" -> AvroValue.Int(2)))),
      Map("key" -> 2)
    )
  }

  pureTest("Codec map decode should decode to map using decoder for value") {
    expectDecode[Map[String, Int]](
      Codec[Map[String, Int]].encode(Map("key" -> 1)),
      Map("key" -> 1)
    )
  }

  pureTest("Codec none schema should be encoded as null") {
    expectSchema[None.type] {
      """"null""""
    }
  }

  pureTest("Codec none encode should encode as null") {
    expectEncode[None.type](
      None,
      AvroValue.Null
    )
  }

  pureTest("Codec none decode should error if schema is not null") {
    expectDecodeError[None.type](
      Codec[None.type].encode(None),
      Codec[Int].schema,
      "Error decoding None: Got unexpected schema type int, expected schema type null"
    )
  }

  pureTest("Codec none decode should error if value is not null") {
    expectDecodeError[None.type](
      Codec[Int].encode(10),
      "Error decoding None: Invalid int value for null"
    )
  }

  pureTest("Codec none decode should decode null as None") {
    expectDecode[None.type](
      Codec[Unit].encode(()),
      None
    )
  }

  pureTest("Codec nonEmptyChain schema should be encoded as array") {
    expectSchema[NonEmptyChain[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec nonEmptyChain encode should encode as array using encoder for underlying type") {
    expectEncode[NonEmptyChain[Int]](
      NonEmptyChain(1, 2, 3),
      AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2), AvroValue.Int(3)))
    )
  }

  pureTest("Codec nonEmptyChain decode should error if schema is not array") {
    expectDecodeError[NonEmptyChain[Int]](
      Codec[NonEmptyChain[Int]].encode(NonEmptyChain(1, 2, 3)),
      Codec[Int].schema,
      "Error decoding NonEmptyChain: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec nonEmptyChain decode should error if value is not array") {
    expectDecodeError[NonEmptyChain[Int]](
      Codec[Int].encode(10),
      "Error decoding NonEmptyChain: Invalid int value for array"
    )
  }

  pureTest("Codec nonEmptyChain decode should error on empty collection") {
    expectDecodeError[NonEmptyChain[Int]](
      Codec[Chain[Int]].encode(Chain.empty[Int]),
      "Error decoding NonEmptyChain: Got unexpected empty collection"
    )
  }

  pureTest("Codec nonEmptyChain decode should decode as NonEmptyChain") {
    val value = NonEmptyChain(1, 2, 3)
    expectDecode[NonEmptyChain[Int]](
      Codec[NonEmptyChain[Int]].encode(value),
      value
    )
  }

  pureTest("Codec nonEmptyList schema should be encoded as array") {
    expectSchema[NonEmptyList[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec nonEmptyList encode should encode as array using encoder for underlying type") {
    expectEncode[NonEmptyList[Int]](
      NonEmptyList.of(1, 2, 3),
      AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2), AvroValue.Int(3)))
    )
  }

  pureTest("Codec nonEmptyList decode should error if schema is not array") {
    expectDecodeError[NonEmptyList[Int]](
      Codec[NonEmptyList[Int]].encode(NonEmptyList.of(1, 2, 3)),
      Codec[Int].schema,
      "Error decoding NonEmptyList: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec nonEmptyList decode should error if value is not array") {
    expectDecodeError[NonEmptyList[Int]](
      Codec[Int].encode(10),
      "Error decoding NonEmptyList: Invalid int value for array"
    )
  }

  pureTest("Codec nonEmptyList decode should error on empty collection") {
    expectDecodeError[NonEmptyList[Int]](
      Codec[List[Int]].encode(List.empty[Int]),
      "Error decoding NonEmptyList: Got unexpected empty collection"
    )
  }

  pureTest("Codec nonEmptyList decode should decode as NonEmptyList") {
    val value = NonEmptyList.of(1, 2, 3)
    expectDecode[NonEmptyList[Int]](
      Codec[NonEmptyList[Int]].encode(value),
      value
    )
  }

  pureTest("Codec nonEmptyMap schema should be encoded as map") {
    expectSchema[NonEmptyMap[String, Int]] {
      """{"type":"map","values":"int"}"""
    }
  }

  pureTest("Codec nonEmptyMap encode should encode as map using encoder for value") {
    expectEncode[NonEmptyMap[String, Int]](
      NonEmptyMap.of("key1" -> 1, "key2" -> 2, "key3" -> 3),
      AvroValue.Map(
        Vector(
          "key1" -> AvroValue.Int(1),
          "key2" -> AvroValue.Int(2),
          "key3" -> AvroValue.Int(3)
        )
      )
    )
  }

  pureTest("Codec nonEmptyMap decode should error if schema is not map") {
    expectDecodeError[NonEmptyMap[String, Int]](
      Codec[NonEmptyMap[String, Int]].encode(NonEmptyMap.one("key", 1)),
      Codec[Int].schema,
      "Error decoding NonEmptyMap: Got unexpected schema type int, expected schema type map"
    )
  }

  pureTest("Codec nonEmptyMap decode should error if value is not map") {
    expectDecodeError[NonEmptyMap[String, Int]](
      Codec[Int].encode(123),
      "Error decoding NonEmptyMap: Invalid int value for map"
    )
  }

  pureTest("Codec nonEmptyMap decode should error on empty collection") {
    expectDecodeError[NonEmptyMap[String, Int]](
      Codec[Map[String, Int]].encode(Map.empty[String, Int]),
      "Error decoding NonEmptyMap: Got unexpected empty collection"
    )
  }

  pureTest("Codec nonEmptyMap decode should decode to map using decoder for value") {
    val value = NonEmptyMap.of("key1" -> 1, "key2" -> 2, "key3" -> 3)
    expectDecode[NonEmptyMap[String, Int]](
      Codec[NonEmptyMap[String, Int]].encode(value),
      value
    )
  }

  pureTest("Codec nonEmptySet schema should be encoded as array") {
    expectSchema[NonEmptySet[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec nonEmptySet encode should encode as array using encoder for underlying type") {
    expectEncode[NonEmptySet[Int]](
      NonEmptySet.of(1, 2, 3),
      AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2), AvroValue.Int(3)))
    )
  }

  pureTest("Codec nonEmptySet decode should error if schema is not array") {
    expectDecodeError[NonEmptySet[Int]](
      Codec[NonEmptySet[Int]].encode(NonEmptySet.of(1, 2, 3)),
      Codec[Int].schema,
      "Error decoding NonEmptySet: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec nonEmptySet decode should error if value is not array") {
    expectDecodeError[NonEmptySet[Int]](
      Codec[Int].encode(10),
      "Error decoding NonEmptySet: Invalid int value for array"
    )
  }

  pureTest("Codec nonEmptySet decode should error on empty collection") {
    expectDecodeError[NonEmptySet[Int]](
      Codec[Set[Int]].encode(Set.empty[Int]),
      "Error decoding NonEmptySet: Got unexpected empty collection"
    )
  }

  pureTest("Codec nonEmptySet decode should decode as NonEmptySet") {
    val value = NonEmptySet.of(1, 2, 3)
    expectDecode[NonEmptySet[Int]](
      Codec[NonEmptySet[Int]].encode(value),
      value
    )
  }

  pureTest("Codec nonEmptyVector schema should be encoded as array") {
    expectSchema[NonEmptyVector[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec nonEmptyVector encode should encode as array using encoder for underlying type") {
    expectEncode[NonEmptyVector[Int]](
      NonEmptyVector.of(1, 2, 3),
      AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2), AvroValue.Int(3)))
    )
  }

  pureTest("Codec nonEmptyVector decode should error if schema is not array") {
    expectDecodeError[NonEmptyVector[Int]](
      Codec[NonEmptyVector[Int]].encode(NonEmptyVector.of(1, 2, 3)),
      Codec[Int].schema,
      "Error decoding NonEmptyVector: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec nonEmptyVector decode should error if value is not array") {
    expectDecodeError[NonEmptyVector[Int]](
      Codec[Int].encode(10),
      "Error decoding NonEmptyVector: Invalid int value for array"
    )
  }

  pureTest("Codec nonEmptyVector decode should error on empty collection") {
    expectDecodeError[NonEmptyVector[Int]](
      Codec[Vector[Int]].encode(Vector.empty[Int]),
      "Error decoding NonEmptyVector: Got unexpected empty collection"
    )
  }

  pureTest("Codec nonEmptyVector decode should decode as NonEmptyVector") {
    val value = NonEmptyVector.of(1, 2, 3)
    expectDecode[NonEmptyVector[Int]](
      Codec[NonEmptyVector[Int]].encode(value),
      value
    )
  }

  pureTest("Codec option schema should be encoded as union") {
    expectSchema[Option[Double]] {
      """["null","double"]"""
    }
  }

  pureTest("Codec option schema should include the types of unions") {
    expectSchema[Option[Either[Int, String]]] {
      """["null","int","string"]"""
    }
  }

  pureTest("Codec option schema should capture errors on nested unions") {
    expectSchemaError[Option[Option[Int]]] {
      """Duplicate null in union ["null","null","int"]"""
    }
  }

  pureTest("Codec option encode should encode Some") {
    expectEncode[Option[Int]](
      Some(1),
      AvroValue.Union(1, AvroValue.Int(1))
    )
  }

  pureTest("Codec option encode should encode None as null") {
    expectEncode[Option[Int]](
      None,
      AvroValue.Union(0, AvroValue.Null)
    )
  }

  pureTest("Codec option encode should encode Some using the types of unions") {
    expectEncode[Option[Either[Int, String]]](
      Some(Right("a")),
      AvroValue.Union(2, AvroValue.String("a"))
    )
  }

  pureTest("Codec option decode should error if schema is not in union") {
    expectDecodeError[Option[Int]](
      Codec[Option[Int]].encode(Option(1)),
      Codec[String].schema,
      "Error decoding Option: Error decoding Int: Got unexpected schema type string, expected schema type int"
    )
  }

  pureTest("Codec option decode should decode if schema is part of union") {
    expectDecode[Option[Int]](
      Right(AvroValue.Int(1)),
      Codec[Int].schema,
      Some(1)
    )
  }

  pureTest("Codec option decode should decode even if there is one schema in union") {
    expectDecode[Option[Int]](
      Right(AvroValue.Union(0, AvroValue.Int(1))),
      Right(Schema.Union(Vector(Schema.Int))),
      Some(1)
    )
  }

  pureTest("Codec option decode should decode even if there is not null in union") {
    expectDecode[Option[Int]](
      Right(AvroValue.Union(0, AvroValue.Int(1))),
      Right(Schema.Union(Vector(Schema.Int, Schema.String))),
      Some(1)
    )
  }

  pureTest("Codec option decode should decode if there are more than two schemas in union") {
    expectDecode[Option[Int]](
      Right(AvroValue.Union(0, AvroValue.Int(1))),
      Right(Schema.Union(Vector(Schema.Int, Schema.String, Schema.Null))),
      Some(1)
    )
  }

  pureTest("Codec option decode should support null as first schema type in union") {
    expectDecode[Option[Int]](
      Right(AvroValue.Union(1, AvroValue.Int(1))),
      Right(Schema.Union(Vector(Schema.Null, Schema.Int))),
      Some(1)
    )
  }

  pureTest("Codec option decode should support null as second schema type in union") {
    expectDecode[Option[Int]](
      Right(AvroValue.Union(0, AvroValue.Int(1))),
      Right(Schema.Union(Vector(Schema.Int, Schema.Null))),
      Some(1)
    )
  }

  pureTest("Codec option decode should decode the written type using its schema") {
    expectDecode[Option[Long]](
      Right(AvroValue.Union(1, AvroValue.Int(1))),
      Right(Schema.Union(Vector(Schema.Null, Schema.Int))),
      Some(1L)
    )
  }

  pureTest("Codec option decode should decode using the types of unions") {
    expectDecode[Option[Either[Int, String]]](
      Right(AvroValue.Union(2, AvroValue.String("a"))),
      Some(Right("a"))
    )
  }

  pureTest("Codec option decode should decode null as None") {
    expectDecode[Option[Int]](
      Codec[Option[Int]].encode(Option.empty[Int]),
      None
    )
  }

  pureTest("Codec option decode should decode null as None if the writer schema is not a union") {
    expectDecode[Option[Int]](
      Right(AvroValue.Null),
      Right(Schema.Null),
      None
    )
  }

  locally {
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

    pureTest("Codec option.union schema should be encoded as union") {
      expectSchema[Option[FirstOrSecond]] {
        """["null","int","double"]"""
      }
    }

    pureTest("Codec option.union encode should encode none as null") {
      expectEncode[Option[FirstOrSecond]](
        None,
        AvroValue.Union(0, AvroValue.Null)
      )
    }

    test("Codec option.union encode should encode first as int") {
      forall { (n: Int) =>
        expectEncode[Option[FirstOrSecond]](
          Some(First(n)),
          AvroValue.Union(1, AvroValue.Int(n))
        )
      }
    }

    test("Codec option.union encode should encode second as double") {
      forall { (n: Double) =>
        expectEncode[Option[FirstOrSecond]](
          Some(Second(n)),
          AvroValue.Union(2, AvroValue.Double(n))
        )
      }
    }

    pureTest("Codec option.union decode should decode null as none") {
      expectDecode[Option[FirstOrSecond]](
        Right(AvroValue.Union(0, AvroValue.Null)),
        None
      )
    }

    test("Codec option.union decode should decode int as first") {
      forall { (n: Int) =>
        expectDecode[Option[FirstOrSecond]](
          Right(AvroValue.Union(1, AvroValue.Int(n))),
          Some(First(n))
        )
      }
    }

    test("Codec option.union decode should decode double as second") {
      forall { (n: Double) =>
        expectDecode[Option[FirstOrSecond]](
          Right(AvroValue.Union(2, AvroValue.Double(n))),
          Some(Second(n))
        )
      }
    }
  }

  pureTest("Codec record field builder should have FieldBuilder as toString") {
    expect.eql("FieldBuilder", Codec.FieldBuilder.instance[Int].toString)
  }

  locally {
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

    pureTest("Codec record schema should have the expected schema") {
      expectSchema[CaseClassTwoFields] {
        """{"type":"record","name":"CaseClassTwoFields","namespace":"vulcan.examples","doc":"some documentation for example","fields":[{"name":"name","type":"string","doc":"some doc","default":"default name","order":"descending","aliases":["TheAlias"],"custom":"value"},{"name":"age","type":"int"}],"custom":[1,2,3],"aliases":["FirstAlias","SecondAlias"]}"""
      }
    }

    pureTest("Codec record schema should error if default value can't be encoded") {
      implicit val intCodec: Codec[Int] =
        Codec.int.imapErrors(_ => Left(AvroException("error")))(_ => Left(AvroException("error")))

      implicit val caseClassFieldCodec: Codec[CaseClassField] =
        Codec.record[CaseClassField]("CaseClassField", "") { field =>
          field("value", _.value, default = Some(10)).map(CaseClassField(_))
        }

      expect.eql(
        Left("Error encoding default for field value: error"),
        caseClassFieldCodec.schema.left.map(_.message)
      )
    }

    pureTest("Codec record schema should error if field names are not unique") {
      case class Test(first: Int, second: Int)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          (field("value", _.first), field("value", _.second)).mapN(Test(_, _))
        }

      expectSchemaError[Test] {
        """Duplicate field "value" in record Test"""
      }
    }

    pureTest("Codec record schema should support None as default value") {
      case class Test(value: Option[Int])

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(None)).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":["null","int"],"default":null}]}"""
      }
    }

    pureTest("Codec record schema should support Some as default value") {
      case class Test(value: Option[Int])

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(Some(0)))(
            Codec.union(alt => alt[Some[Int]] ++ alt[None.type])
          ).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":["int","null"],"default":0}]}"""
      }
    }

    pureTest("Codec record schema default should support null default value") {
      case class Test(value: Unit)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(())).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"null","default":null}]}"""
      }
    }

    pureTest("Codec record schema default should support boolean default value") {
      case class Test(value: Boolean)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(false)).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"boolean","default":false}]}"""
      }
    }

    pureTest("Codec record schema default should support int default value") {
      case class Test(value: Int)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(0)).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"int","default":0}]}"""
      }
    }

    pureTest("Codec record schema default should support long default value") {
      case class Test(value: Long)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(0L)).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"long","default":0}]}"""
      }
    }

    pureTest("Codec record schema default should support float default value") {
      case class Test(value: Float)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(1.5f)).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"float","default":1.5}]}"""
      }
    }

    pureTest("Codec record schema default should support double default value") {
      case class Test(value: Double)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(1.5d)).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"double","default":1.5}]}"""
      }
    }

    pureTest("Codec record schema default should support bytes default value") {
      case class Test(value: ByteVector)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(ByteVector(Byte.MinValue, Byte.MaxValue)))
            .map(Test(_))
        }

      val expectedDefault = "\u0080\u007f"

      expectSchema[Test] {
        s"""{"type":"record","name":"Test","fields":[{"name":"value","type":"bytes","default":"$expectedDefault"}]}"""
      }
    }

    pureTest("Codec record schema default should support string default value") {
      case class Test(value: String)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some("default-value")).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"string","default":"default-value"}]}"""
      }
    }

    pureTest("Codec record schema default should support record default value") {
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

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"record","name":"Inner","fields":[{"name":"value","type":"int"},{"name":"customEnum","type":{"type":"enum","name":"CustomEnum","symbols":["first","second"]},"default":"first"}]},"default":{"value":0,"customEnum":"second"}}]}"""
      }
    }

    pureTest("Codec record schema default should support enum default value") {
      case class Test(value: CustomEnum)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(First)).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"enum","name":"CustomEnum","symbols":["first","second"]},"default":"first"}]}"""
      }
    }

    pureTest("Codec record schema default should support array default value") {
      case class Test(value: List[Int])

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(List(123, 456))).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"array","items":"int"},"default":[123,456]}]}"""
      }
    }

    pureTest("Codec record schema default should support record array default value") {
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

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"array","items":{"type":"record","name":"Element","fields":[{"name":"value","type":"int"}]}},"default":[{"value":123},{"value":456}]}]}"""
      }
    }

    pureTest("Codec record schema default should support map default value") {
      case class Test(value: Map[String, Int])

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(Map("key" -> 0))).map(Test(_))
        }

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"map","values":"int"},"default":{"key":0}}]}"""
      }
    }

    pureTest("Codec record schema default should support record map default value") {
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

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"map","values":{"type":"record","name":"Value","fields":[{"name":"value","type":"int"}]}},"default":{"key":{"value":0}}}]}"""
      }
    }

    pureTest("Codec record schema default should support fixed default value") {
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

      expectSchema[Test] {
        s"""{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"fixed","name":"Inner","size":1},"default":"$expectedDefault"}]}"""
      }
    }

    pureTest("Codec record schema props should support custom properties") {
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

      expectSchema[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"int","boolean":true,"number":123.0,"array":[123,456]}],"custom":{"key":"value"}}"""
      }
    }

    pureTest("Codec record schema props should error if properties are reserved") {
      case class Test(value: Int)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, props = JsonObject("default" -> Json.fromInt(1)))
            .map(Test(_))
        }

      expectSchemaError[Test] {
        """Invalid property "default" of field "value" in record Test: name is reserved"""
      }
    }

    pureTest("Codec record encode should encode as record") {
      expectEncode[CaseClassTwoFields](
        CaseClassTwoFields("name", 0),
        AvroValue.Record(Vector(AvroValue.String("name"), AvroValue.Int(0)))
      )
    }

    pureTest("Codec record encode should error if a field cannot be encoded") {
      case class Test(value: Int)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value)(Codec.int.imapErrors(Right(_))(_ => Left(AvroException("error"))))
            .map(Test(_))
        }

      expectEncodeError[Test](
        Test(0),
        "Error encoding Test: Error encoding field value: error"
      )
    }

    pureTest("Codec record encode should support None as default value") {
      case class Test(value: Option[Int])

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(None)).map(Test(_))
        }

      expectEncode[Test](
        Test(None),
        AvroValue.Record(Vector(AvroValue.Union(0, AvroValue.Null)))
      )
    }

    pureTest("Codec record encode should support Some as default value") {
      case class Test(value: Option[Int])

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(Some(0)))(
            Codec.union(alt => alt[Some[Int]] ++ alt[None.type])
          ).map(Test(_))
        }

      expectEncode[Test](
        Test(None),
        AvroValue.Record(Vector(AvroValue.Union(1, AvroValue.Null)))
      )
    }

    locally {
      val nameField = Schema.Field("name", Schema.String)
      val ageField = Schema.Field("age", Schema.Int)

      def writerSchema(fields: Schema.Field*): Schema =
        Schema.Record(Schema.Name("CaseClassTwoFields", Some("vulcan.examples")), fields.toVector)

      pureTest("Codec record decode should error if schema is not record") {
        expectDecodeError[CaseClassTwoFields](
          Codec[CaseClassTwoFields].encode(CaseClassTwoFields("name", 123)),
          Codec[String].schema,
          "Error decoding vulcan.examples.CaseClassTwoFields: Got unexpected schema type string, expected schema type record"
        )
      }

      pureTest("Codec record decode should error if value is not record") {
        expectDecodeError[CaseClassTwoFields](
          Codec[Int].encode(123),
          "Error decoding vulcan.examples.CaseClassTwoFields: Invalid int value for record vulcan.examples.CaseClassTwoFields"
        )
      }

      pureTest("Codec record decode should error if the record does not have a value for every field") {
        expectDecodeError[CaseClassTwoFields](
          Right(AvroValue.Record(Vector(AvroValue.String("name")))),
          "Error decoding vulcan.examples.CaseClassTwoFields: Invalid record with 1 values for record vulcan.examples.CaseClassTwoFields with 2 fields"
        )
      }

      pureTest("Codec record decode should error if any field without default value is missing") {
        expectDecodeError[CaseClassTwoFields](
          Right(AvroValue.Record(Vector(AvroValue.String("name")))),
          Right(writerSchema(nameField)),
          "Error decoding vulcan.examples.CaseClassTwoFields: Record writer schema is missing field 'age'"
        )
      }

      pureTest("Codec record decode should error if a field cannot be decoded") {
        expectDecodeError[CaseClassTwoFields](
          Right(AvroValue.Record(Vector(AvroValue.String("name"), AvroValue.String("123")))),
          Right(writerSchema(nameField, Schema.Field("age", Schema.String))),
          "Error decoding vulcan.examples.CaseClassTwoFields: Error decoding field age: Error decoding Int: Got unexpected schema type string, expected schema type int"
        )
      }

      pureTest("Codec record decode should decode if field with default value is missing") {
        expectDecode[CaseClassTwoFields](
          Right(AvroValue.Record(Vector(AvroValue.Int(123)))),
          Right(writerSchema(ageField)),
          CaseClassTwoFields("default name", 123)
        )
      }

      pureTest("Codec record decode should decode fields in a different order") {
        expectDecode[CaseClassTwoFields](
          Right(AvroValue.Record(Vector(AvroValue.Int(123), AvroValue.String("name")))),
          Right(writerSchema(ageField, nameField)),
          CaseClassTwoFields("name", 123)
        )
      }

      pureTest("Codec record decode should ignore fields which are not part of the record") {
        expectDecode[CaseClassTwoFields](
          Right(
            AvroValue.Record(
              Vector(AvroValue.String("name"), AvroValue.Double(1.0), AvroValue.Int(123))
            )
          ),
          Right(writerSchema(nameField, Schema.Field("other", Schema.Double), ageField)),
          CaseClassTwoFields("name", 123)
        )
      }

      pureTest("Codec record decode should decode fields using the writer schema") {
        case class Test(value: Long)

        implicit val testCodec: Codec[Test] =
          Codec.record("Test", "") { field =>
            field("value", _.value).map(Test(_))
          }

        expectDecode[Test](
          Right(AvroValue.Record(Vector(AvroValue.Int(1)))),
          Right(Schema.Record(Schema.Name("Test"), Vector(Schema.Field("value", Schema.Int)))),
          Test(1L)
        )
      }

      pureTest("Codec record decode should decode as case class") {
        expectDecode[CaseClassTwoFields](
          Codec[CaseClassTwoFields].encode(CaseClassTwoFields("name", 123)),
          CaseClassTwoFields("name", 123)
        )
      }

      pureTest("Codec record decode should support None as default value") {
        case class Test(value: Option[Int])

        implicit val testCodec: Codec[Test] =
          Codec.record("Test", "") { field =>
            field("value", _.value, default = Some(None)).map(Test(_))
          }

        expectDecode[Test](
          Codec[Test].encode(Test(None)),
          Test(None)
        )
      }

      pureTest("Codec record decode should support Some as default value") {
        case class Test(value: Option[Int])

        implicit val testCodec: Codec[Test] =
          Codec.record("Test", "") { field =>
            field("value", _.value, default = Some(Some(0)))(
              Codec.union(alt => alt[Some[Int]] ++ alt[None.type])
            ).map(Test(_))
          }

        expectDecode[Test](
          Codec[Test].encode(Test(None)),
          Test(None)
        )
      }

      pureTest("Codec record decode should decode field with aliased name") {
        case class Aliased(aliasedField: Int)

        implicit val codec: Codec[Aliased] =
          Codec.record("CaseClassField", "") { field =>
            field("aliasedField", _.aliasedField, aliases = Seq("value")).map(Aliased(_))
          }

        expectDecode[Aliased](
          Codec[CaseClassField].encode(CaseClassField(3)),
          Codec[CaseClassField].schema,
          Aliased(3)
        )
      }

      pureTest("Codec record decode should decode references to named schemas in the writer schema") {
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

        expectDecode[Node](
          Right(
            AvroValue.Record(
              Vector(
                AvroValue.Int(1),
                AvroValue.Union(
                  1,
                  AvroValue.Record(Vector(AvroValue.Int(2), AvroValue.Union(0, AvroValue.Null)))
                )
              )
            )
          ),
          Right(recursive),
          Node(1, Some(Leaf(2)))
        )
      }
    }
  }

  pureTest("Codec seq schema should be encoded as array") {
    expectSchema[Seq[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec seq encode should encode as array using encoder for underlying type") {
    expectEncode[Seq[Int]](
      Seq(1, 2, 3),
      AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2), AvroValue.Int(3)))
    )
  }

  pureTest("Codec seq decode should error if schema is not array") {
    expectDecodeError[Seq[Int]](
      Codec[Seq[Int]].encode(Seq(1, 2, 3)),
      Codec[Int].schema,
      "Error decoding Seq: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec seq decode should error if value is not array") {
    expectDecodeError[Seq[Int]](
      Codec[Int].encode(10),
      "Error decoding Seq: Invalid int value for array"
    )
  }

  pureTest("Codec seq decode should decode as Seq") {
    val value = Seq(1, 2, 3)
    expectDecode[Seq[Int]](
      Codec[Seq[Int]].encode(value),
      value
    )
  }

  pureTest("Codec set schema should be encoded as array") {
    expectSchema[Set[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec set encode should encode as array using encoder for underlying type") {
    expectEncode[Set[Int]](
      Set(1, 2, 3),
      AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2), AvroValue.Int(3)))
    )
  }

  pureTest("Codec set decode should error if schema is not array") {
    expectDecodeError[Set[Int]](
      Codec[Set[Int]].encode(Set(1, 2, 3)),
      Codec[Int].schema,
      "Error decoding Set: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec set decode should error if value is not array") {
    expectDecodeError[Set[Int]](
      Codec[Int].encode(10),
      "Error decoding Set: Invalid int value for array"
    )
  }

  pureTest("Codec set decode should decode as Set") {
    val value = Set(1, 2, 3)
    expectDecode[Set[Int]](
      Codec[Set[Int]].encode(value),
      value
    )
  }

  pureTest("Codec short schema should be encoded as int") {
    expectSchema[Short] {
      """"int""""
    }
  }

  pureTest("Codec short encode should encode as int") {
    val value = 1.toShort
    expectEncode[Short](
      value,
      AvroValue.Int(1)
    )
  }

  pureTest("Codec short decode should error if schema is not int") {
    expectDecodeError[Short](
      Codec[Short].encode(1.toShort),
      Codec[String].schema,
      "Error decoding Short: Error decoding Int: Got unexpected schema type string, expected schema type int"
    )
  }

  pureTest("Codec short decode should error if value is not int") {
    expectDecodeError[Short](
      Codec[String].encode("value"),
      "Error decoding Short: Error decoding Int: Invalid string value for int"
    )
  }

  test("Codec short decode should error if int value is not short") {
    val gen =
      Gen.oneOf(
        Gen.chooseNum(Int.MinValue, Short.MinValue.toInt - 1),
        Gen.chooseNum(Short.MaxValue.toInt + 1, Int.MaxValue)
      )

    forall(gen) { nonShort =>
      expectDecodeError[Short](
        Codec[Int].encode(nonShort),
        s"Error decoding Short: Got unexpected Int value $nonShort, expected value in range -32768 to 32767"
      )
    }
  }

  test("Codec short decode should decode as short") {
    forall { (short: Short) =>
      expectDecode[Short](
        Codec[Short].encode(short),
        short
      )
    }
  }

  pureTest("Codec show should include the schema if available") {
    expect.eql("""Codec("int")""", Codec[Int].show)
  }

  pureTest("Codec show should show the error if schema is unavailable") {
    val codec =
      Codec.instance[Int](
        Left(AvroException("error")),
        int => Right(AvroValue.Int(int)),
        _ => Left(AvroException("error"))
      )

    expect.eql("vulcan.AvroException: error", codec.show)
  }

  pureTest("Codec show should have a Show instance consistent with toString") {
    val codec = Codec[Boolean]
    expect.eql(codec.toString(), codec.show)
  }

  pureTest("Codec string schema should be encoded as string") {
    expectSchema[String] {
      """"string""""
    }
  }

  pureTest("Codec string encode should encode as string") {
    val value = "abc"
    expectEncode[String](
      value,
      AvroValue.String(value)
    )
  }

  pureTest("Codec string decode should error if schema is not string or bytes") {
    expectDecodeError[String](
      Codec[String].encode("abc"),
      Codec[Int].schema,
      "Error decoding String: Got unexpected schema type int, expected schema type string"
    )
  }

  pureTest("Codec string decode should error if value is not string or bytes") {
    expectDecodeError[String](
      Codec[Int].encode(10),
      "Error decoding String: Invalid int value for string"
    )
  }

  pureTest("Codec string decode should decode string as string") {
    val value = "abc"
    expectDecode[String](
      Codec[String].encode(value),
      value
    )
  }

  pureTest("Codec string decode should decode bytes as string") {
    expectDecode[String](
      Right(AvroValue.Bytes(ByteVector("abc".getBytes(StandardCharsets.UTF_8)))),
      Codec[ByteVector].schema,
      "abc"
    )
  }

  pureTest("Codec string decode should decode a slice of bytes as string") {
    expectDecode[String](
      Right(AvroValue.Bytes(ByteVector("xabcx".getBytes(StandardCharsets.UTF_8)).slice(1, 4))),
      Codec[ByteVector].schema,
      "abc"
    )
  }

  pureTest("Codec union alt builder should have AltBuilder as toString") {
    expect.eql("AltBuilder", Codec.AltBuilder.instance[Int].toString)
  }

  locally {
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

    pureTest("Codec union schema should encode records as union") {
      expectSchema[SealedTraitCaseClass] {
        """[{"type":"record","name":"FirstInSealedTraitCaseClass","namespace":"com.example","fields":[{"name":"value","type":"int"}]},{"type":"record","name":"SecondInSealedTraitCaseClass","namespace":"com.example","fields":[{"name":"value","type":"string"}]},{"type":"array","items":"int"}]"""
      }
    }

    pureTest("Codec union schema should encode as union") {
      implicit val codec: Codec[Either[String, String]] =
        unionOf(enumCodec("Suit", "a"), enumCodec("Suit", "b"))

      expectSchema[Either[String, String]] {
        """[{"type":"enum","name":"Suit","namespace":"a","symbols":["A","B"]},{"type":"enum","name":"Suit","namespace":"b","symbols":["A","B"]}]"""
      }
    }

    pureTest("Codec union schema should capture errors on nested unions") {
      expectSchemaError[SealedTraitCaseClassNestedUnion] {
        """Unions cannot directly contain other unions: [["null","int"]]"""
      }
    }

    pureTest("Codec union schema should capture errors on duplicate types") {
      expectSchemaError[Either[Int, Int]] {
        """Duplicate int in union ["int","int"]"""
      }
    }

    pureTest("Codec union encode should error if subtype is not an alternative") {
      expectEncodeError[SealedTraitCaseClassIncomplete](
        SecondInSealedTraitCaseClassIncomplete(0d),
        "Exhausted alternatives for union"
      )
    }

    pureTest("Codec union encode should error if subtype is not an alternative and value null") {
      expectEncodeError[SealedTraitCaseClassIncomplete](
        null,
        "Exhausted alternatives for union"
      )
    }

    pureTest("Codec union encode should encode with encoder for alternative") {
      expectEncode[SealedTraitCaseClassIncomplete](
        FirstInSealedTraitCaseClassIncomplete(0),
        AvroValue.Union(0, AvroValue.Int(0))
      )
    }

    pureTest("Codec union decode should error if there is no alternative for the written type") {
      expectDecodeError[Either[Int, String]](
        Right(AvroValue.Boolean(true)),
        Right(Schema.Boolean),
        "Error decoding Either: Missing alternative for boolean in union"
      )
    }

    pureTest("Codec union decode should decode using the alternative for the written type") {
      expectDecode[Either[Int, String]](
        Right(AvroValue.Union(1, AvroValue.Int(1))),
        Right(Schema.Union(Vector(Schema.String, Schema.Int))),
        Left(1)
      )
    }

    pureTest("Codec union decode should decode written types which are not unions") {
      expectDecode[Either[Int, String]](
        Right(AvroValue.String("a")),
        Right(Schema.String),
        Right("a")
      )
    }

    pureTest("Codec union decode should decode using the alternative of the same type before promotion") {
      expectDecode[Either[Double, Long]](
        Right(AvroValue.Long(1L)),
        Right(Schema.Long),
        Right(1L)
      )
    }

    pureTest("Codec union decode should decode using promotion if there is no alternative of the same type") {
      expectDecode[Either[Long, String]](
        Right(AvroValue.Int(1)),
        Right(Schema.Int),
        Left(1L)
      )
    }

    pureTest("Codec union decode should return the error of the alternative for the written type") {
      expectDecodeError[Either[Byte, String]](
        Right(AvroValue.Int(1000)),
        Right(Schema.Int),
        "Error decoding Either: Error decoding Byte: Got unexpected Int value 1000, expected value in range -128 to 127"
      )
    }

    pureTest("Codec union decode should decode named types using the full name") {
      implicit val codec: Codec[Either[String, String]] =
        unionOf(enumCodec("Suit", "a"), enumCodec("Suit", "b"))

      expectDecode[Either[String, String]](
        Right(AvroValue.Enum("A")),
        Right(enumSchema("Suit", "b")),
        Right("A")
      )
    }

    pureTest("Codec union decode should decode named types using an alias") {
      implicit val codec: Codec[Either[String, String]] =
        unionOf(enumCodec("Card", "a"), enumCodec("Suit", "b", aliases = List("a.Old")))

      expectDecode[Either[String, String]](
        Right(AvroValue.Enum("A")),
        Right(enumSchema("Old", "a")),
        Right("A")
      )
    }

    pureTest("Codec union decode should decode named types using the name if there is no full name") {
      implicit val codec: Codec[Either[String, String]] =
        unionOf(enumCodec("Card", "a"), enumCodec("Suit", "b"))

      expectDecode[Either[String, String]](
        Right(AvroValue.Enum("A")),
        Right(enumSchema("Suit", "c")),
        Right("A")
      )
    }

    pureTest("Codec union decode should error if there is no named type of the same kind") {
      implicit val codec: Codec[Either[String, String]] =
        unionOf(enumCodec("Card", "a"), enumCodec("Suit", "b"))

      expectDecodeError[Either[String, String]](
        Right(AvroValue.Fixed(hex"00")),
        Right(Schema.Fixed(Schema.Name("Suit", Some("b")), 1)),
        "Missing alternative for fixed b.Suit in union"
      )
    }

    pureTest("Codec union decode should error if schema is not in union") {
      expectDecodeError[SealedTraitCaseClass](
        Right(AvroValue.String("value")),
        Codec[String].schema,
        "Missing alternative for string in union"
      )
    }

    pureTest("Codec union decode should decode if schema is part of union") {
      expectDecode[SealedTraitCaseClass](
        Codec[FirstInSealedTraitCaseClass].encode(FirstInSealedTraitCaseClass(0)),
        Codec[FirstInSealedTraitCaseClass].schema,
        FirstInSealedTraitCaseClass(0)
      )
    }

    pureTest("Codec union decode should decode if schema is union with records of the same name") {
      expectDecode[SealedTraitCaseClassSharedName](
        Codec[Second.SharedNameSealedTraitCaseClass].encode(Second.SharedNameSealedTraitCaseClass("hello")),
        Codec[Second.SharedNameSealedTraitCaseClass].schema,
        Second.SharedNameSealedTraitCaseClass("hello")
      )
    }

    pureTest("Codec union decode should error if value is not a union value") {
      expectDecodeError[SealedTraitCaseClass](
        Right(AvroValue.Double(123d)),
        "Invalid double value for union"
      )
    }

    pureTest("Codec union decode should error if value is null and not an alternative") {
      expectDecodeError[SealedTraitCaseClass](
        Right(AvroValue.Null),
        "Invalid null value for union"
      )
    }

    pureTest("Codec union decode should error if no alternative with the name of the written type") {
      expectDecodeError[SealedTraitCaseClass](
        Codec[CaseClassInSealedTraitCaseClassSingle].encode(CaseClassInSealedTraitCaseClassSingle(0)),
        Codec[CaseClassInSealedTraitCaseClassSingle].schema,
        "Missing alternative for record vulcan.examples.CaseClassInSealedTraitCaseClassSingle in union"
      )
    }

    pureTest("Codec union decode should decode using schema and decoder for alternative") {
      expectDecode[SealedTraitCaseClass](
        Codec[SealedTraitCaseClass].encode(FirstInSealedTraitCaseClass(0)),
        FirstInSealedTraitCaseClass(0)
      )
    }

    pureTest("Codec union decode should decode using schema with aliased name") {
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

      expectDecode[SealedTraitCaseClass](
        SealedTraitCaseClass.sealedTraitCaseClassCodec.encode(SecondInSealedTraitCaseClass("foo")),
        SealedTraitCaseClass.sealedTraitCaseClassCodec.schema,
        SecondInSealedTraitCaseClass("foo")
      )
    }
  }

  pureTest("Codec unit schema should be encoded as null") {
    expectSchema[Unit] {
      """"null""""
    }
  }

  pureTest("Codec unit encode should encode as null") {
    val value = ()
    expectEncode[Unit](
      value,
      AvroValue.Null
    )
  }

  pureTest("Codec unit decode should error if schema is not null") {
    expectDecodeError[Unit](
      Codec[Unit].encode(()),
      Codec[Int].schema,
      "Error decoding Unit: Got unexpected schema type int, expected schema type null"
    )
  }

  pureTest("Codec unit decode should error if value is not null") {
    expectDecodeError[Unit](
      Codec[Int].encode(10),
      "Error decoding Unit: Invalid int value for null"
    )
  }

  pureTest("Codec unit decode should decode null as unit") {
    expectDecode[Unit](
      Codec[Unit].encode(()),
      ()
    )
  }

  locally {
    val uuid = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")

    pureTest("Codec uuid schema should be encoded as string with logical type uuid") {
      expectSchema[UUID] {
        """{"type":"string","logicalType":"uuid"}"""
      }
    }

    pureTest("Codec uuid encode should encode as string") {
      val value = uuid
      expectEncode[UUID](
        value,
        AvroValue.String(value.toString)
      )
    }

    pureTest("Codec uuid decode should error if logical type is not uuid") {
      expectDecodeError[UUID](
        Codec[UUID].encode(uuid),
        Codec[String].schema,
        "Error decoding UUID: Got unexpected missing logical type"
      )
    }

    pureTest("Codec uuid decode should error if value is not uuid") {
      expectDecodeError[UUID](
        Right(AvroValue.String("not-uuid")),
        "Error decoding UUID: Invalid UUID string: not-uuid"
      )
    }

    pureTest("Codec uuid decode should decode uuid") {
      val value = uuid
      expectDecode[UUID](
        Codec[UUID].encode(value),
        value
      )
    }
  }

  pureTest("Codec vector schema should be encoded as array") {
    expectSchema[Vector[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec vector encode should encode as array using encoder for underlying type") {
    expectEncode[Vector[Int]](
      Vector(1, 2, 3),
      AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2), AvroValue.Int(3)))
    )
  }

  pureTest("Codec vector decode should error if schema is not array") {
    expectDecodeError[Vector[Int]](
      Codec[Vector[Int]].encode(Vector(1, 2, 3)),
      Codec[Int].schema,
      "Error decoding Vector: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec vector decode should error if value is not array") {
    expectDecodeError[Vector[Int]](
      Codec[Int].encode(10),
      "Error decoding Vector: Invalid int value for array"
    )
  }

  pureTest("Codec vector decode should decode as Vector") {
    val value = Vector(1, 2, 3)
    expectDecode[Vector[Int]](
      Codec[Vector[Int]].encode(value),
      value
    )
  }

  pureTest("Codec vector decode should decode as Vector for any number of items") {
    forEach((0 to 64).toList) { size =>
      val value = Vector.range(0, size)
      expectDecode[Vector[Int]](
        Codec[Vector[Int]].encode(value),
        value
      )
    }
  }
}
