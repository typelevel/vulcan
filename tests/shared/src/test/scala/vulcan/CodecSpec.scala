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
import weaver.Expectations
import weaver.SourceLocation

object CodecSpec extends BaseSuite with CodecSpecHelpers {
  pureTest("Codec binaryDecoder should decode values using the writer schema") {
    val decoder = Codec[Long].binaryDecoder(unsafeSchema[Int]).value
    expect.same(Right(1L), decoder.decode(hex"02")) &&
    expect.same(Right(2L), decoder.decode(hex"04"))
  }

  pureTest("Codec binaryDecoder should decode values of unions using the written type") {
    val decoder = Codec[Long].binaryDecoder(Schema.Union(Vector(Schema.Int, Schema.Long))).value
    expect.same(Right(1L), decoder.decode(hex"0002")) &&
    expect.same(Right(2L), decoder.decode(hex"0204"))
  }

  pureTest("Codec binaryDecoder should ignore remaining bytes") {
    val decoder = Codec[Int].binaryDecoder(unsafeSchema[Int]).value
    expect.same(Right(1), decoder.decode(hex"0204"))
  }

  pureTest("Codec binaryDecoder should decode zero bytes as null") {
    val decoder = Codec[Unit].binaryDecoder(unsafeSchema[Unit]).value
    expect.same(Right(()), decoder.decode(ByteVector.empty))
  }

  pureTest("Codec binaryDecoder should decode items promoted to types with larger encodings") {
    val bytes = Codec[List[Int]].binaryEncoder.value.encode(List(0, 1, Int.MaxValue)).value
    val decoder = Codec[List[Float]].binaryDecoder(unsafeSchema[List[Int]]).value
    expect.same(Right(List(0f, 1f, Int.MaxValue.toFloat)), decoder.decode(bytes))
  }

  pureTest("Codec binaryDecoder should error if the writer schema is invalid") {
    val result = Codec[Int].binaryDecoder(Schema.Union(Vector(Schema.Int, Schema.Int)))
    expect.eql("""Duplicate int in union ["int","int"]""", result.swap.value.message)
  }

  pureTest("Codec binaryDecoder should error if the schema is invalid") {
    val result = Codec.decimal(precision = -1, scale = -1).binaryDecoder(Schema.Bytes)
    expect.eql("Invalid decimal precision -1: must be positive", result.swap.value.message)
  }

  pureTest("Codec binaryDecoder should error if the binary does not match the writer schema") {
    val decoder = Codec[Int].binaryDecoder(unsafeSchema[Int]).value
    expect.eql(
      "Unexpected end of input at position 0",
      decoder.decode(ByteVector.empty).swap.value.message
    )
  }

  pureTest("Codec binaryDecoder should error if the writer schema does not match the type") {
    val decoder = Codec[Int].binaryDecoder(unsafeSchema[Long]).value
    expect.eql(
      "Error decoding Int: Got unexpected schema type long, expected schema type int",
      decoder.decode(hex"02").swap.value.message
    )
  }

  pureTest("Codec binaryEncoder should encode to avro binary") {
    val encoder = Codec[Int].binaryEncoder.value
    expect.same(Right(hex"02"), encoder.encode(1)) &&
    expect.same(Right(hex"04"), encoder.encode(2))
  }

  pureTest("Codec boolean schema should be encoded as boolean") {
    expectSchemaIs[Boolean] {
      """"boolean""""
    }
  }

  pureTest("Codec boolean encode should encode as boolean") {
    expectEncodeIs[Boolean](
      true,
      Right(AvroValue.Boolean(true))
    )
  }

  pureTest("Codec boolean decode should error if schema is not boolean") {
    expectDecodeError[Boolean](
      unsafeEncode(false),
      unsafeSchema[Long],
      "Error decoding Boolean: Got unexpected schema type long, expected schema type boolean"
    )
  }

  pureTest("Codec boolean decode should error if value is not boolean") {
    expectDecodeError[Boolean](
      unsafeEncode(10L),
      unsafeSchema[Boolean],
      "Error decoding Boolean: Invalid long value for boolean"
    )
  }

  pureTest("Codec boolean decode should error if value is null") {
    expectDecodeError[Boolean](
      AvroValue.Null,
      unsafeSchema[Boolean],
      "Error decoding Boolean: Invalid null value for boolean"
    )
  }

  pureTest("Codec boolean decode should decode as Boolean") {
    val value = false
    expectDecodeIs[Boolean](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec byte schema should be encoded as int") {
    expectSchemaIs[Byte] {
      """"int""""
    }
  }

  pureTest("Codec byte encode should encode as int") {
    val value = 1.toByte
    expectEncodeIs[Byte](
      value,
      Right(AvroValue.Int(1))
    )
  }

  pureTest("Codec byte decode should error if schema is not int") {
    expectDecodeError[Byte](
      unsafeEncode(1.toByte),
      unsafeSchema[String],
      "Error decoding Byte: Error decoding Int: Got unexpected schema type string, expected schema type int"
    )
  }

  pureTest("Codec byte decode should error if value is not int") {
    expectDecodeError[Byte](
      unsafeEncode("value"),
      unsafeSchema[Byte],
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
        unsafeEncode(nonByte),
        unsafeSchema[Byte],
        s"Error decoding Byte: Got unexpected Int value $nonByte, expected value in range -128 to 127"
      )
    }
  }

  test("Codec byte decode should decode as byte") {
    forall { (byte: Byte) =>
      expectDecodeIs[Byte](
        unsafeEncode(byte),
        Right(byte)
      )
    }
  }

  pureTest("Codec bytes schema should be encoded as bytes") {
    expectSchemaIs[ByteVector] {
      """"bytes""""
    }
  }

  pureTest("Codec bytes encode should encode as bytes") {
    expectEncodeIs[ByteVector](
      hex"01",
      Right(AvroValue.Bytes(hex"01"))
    )
  }

  pureTest("Codec bytes decode should error on schema other than bytes or string") {
    expectDecodeError[ByteVector](
      unsafeEncode(hex"01"),
      unsafeSchema[Int],
      "Error decoding ByteVector: Got unexpected schema type int, expected schema type bytes"
    )
  }

  pureTest("Codec bytes decode should error if value is not bytes") {
    expectDecodeError[ByteVector](
      AvroValue.Int(10),
      unsafeSchema[ByteVector],
      "Error decoding ByteVector: Invalid int value for bytes"
    )
  }

  pureTest("Codec bytes decode should decode string as bytes") {
    expectDecodeIs[ByteVector](
      unsafeEncode("foo"),
      Right(ByteVector("foo".getBytes(StandardCharsets.UTF_8))),
      Some(unsafeSchema[String])
    )
  }

  pureTest("Codec bytes decode should decode bytes as bytes") {
    val bytes =
      ByteVector("c6e6b318-303c-4ffa-8c0b-c689ddc49e94".getBytes(StandardCharsets.UTF_8))
    expectDecodeIs[ByteVector](
      AvroValue.Bytes(bytes),
      Right(bytes)
    )
  }

  pureTest("Codec chain schema should be encoded as array") {
    expectSchemaIs[Chain[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec chain encode should encode as array using encoder for underlying type") {
    expectEncodeIs[Chain[Int]](
      Chain(1, 2, 3),
      Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
    )
  }

  pureTest("Codec chain decode should error if schema is not array") {
    expectDecodeError[Chain[Int]](
      unsafeEncode(Chain(1, 2, 3)),
      unsafeSchema[Int],
      "Error decoding Chain: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec chain decode should error if value is not array") {
    expectDecodeError[Chain[Int]](
      unsafeEncode(10),
      unsafeSchema[Chain[Int]],
      "Error decoding Chain: Invalid int value for array"
    )
  }

  pureTest("Codec chain decode should decode as Chain") {
    val value = Chain(1, 2, 3)
    expectDecodeIs[Chain[Int]](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec char schema should be encoded as string") {
    expectSchemaIs[Char] {
      """"string""""
    }
  }

  pureTest("Codec char encode should encode as string") {
    val value = 'a'
    expectEncodeIs[Char](
      value,
      Right(AvroValue.String("a"))
    )
  }

  pureTest("Codec char decode should error if string value is empty") {
    expectDecodeError[Char](
      unsafeEncode(""),
      unsafeSchema[String],
      "Error decoding Char: Got unexpected String with length 0, expected length 1"
    )
  }

  pureTest("Codec char decode should error if string value has more than 1 char") {
    expectDecodeError[Char](
      unsafeEncode("ab"),
      unsafeSchema[String],
      "Error decoding Char: Got unexpected String with length 2, expected length 1"
    )
  }

  test("Codec char decode should decode as char") {
    forall { (char: Char) =>
      expectDecodeIs[Char](
        unsafeEncode(char),
        Right(char)
      )
    }
  }

  pureTest("Codec decimal schema should be encoded as bytes with logical type decimal") {
    implicit val codec: Codec[BigDecimal] =
      Codec.decimal(precision = 10, scale = 5)

    expectSchemaIs[BigDecimal] {
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
      expectEncodeIs[BigDecimal](
        value,
        Right(AvroValue.Bytes(ByteVector(value.underlying.unscaledValue.toByteArray)))
      )
    }
  }

  locally {
    implicit val codec: Codec[BigDecimal] =
      Codec.decimal(precision = 10, scale = 5)

    pureTest("Codec decimal decode should error if schema is not bytes") {
      expectDecodeError[BigDecimal](
        unsafeEncode(BigDecimal("123.45678")),
        unsafeSchema[String],
        "Error decoding BigDecimal: Got unexpected schema type string, expected schema type bytes"
      )
    }

    pureTest("Codec decimal decode should error if value is not bytes") {
      expectDecodeError[BigDecimal](
        unsafeEncode(10),
        unsafeSchema[BigDecimal],
        "Error decoding BigDecimal: Invalid int value for bytes"
      )
    }

    pureTest("Codec decimal decode should error if logical type is missing") {
      expectDecodeError[BigDecimal](
        unsafeEncode(BigDecimal("123.45678")),
        Schema.Bytes,
        "Error decoding BigDecimal: Got unexpected missing logical type"
      )
    }

    pureTest("Codec decimal decode should error if logical type is not decimal") {
      expectDecodeError[BigDecimal](
        unsafeEncode(BigDecimal("123.45678")),
        Schema.Bytes.withLogicalType(LogicalType.bigDecimal),
        "Error decoding BigDecimal: Got unexpected logical type big-decimal"
      )
    }

    pureTest("Codec decimal decode should error if logical type is invalid") {
      val params = JsonObject("precision" -> Json.fromInt(2), "scale" -> Json.fromInt(3))

      expectDecodeError[BigDecimal](
        unsafeEncode(BigDecimal("123.45678")),
        Schema.Bytes.withLogicalType(LogicalType("decimal", params)),
        "Error decoding BigDecimal: Got invalid logical type decimal for bytes"
      )
    }

    pureTest("Codec decimal decode should error if precision exceeds schema precision") {
      expectDecodeError[BigDecimal](
        unsafeEncode(BigDecimal("123.45678")),
        Schema.Bytes.withLogicalType(LogicalType.decimal(6, 5)),
        "Error decoding BigDecimal: Unable to decode decimal with precision 8 exceeding schema precision 6"
      )
    }

    pureTest("Codec decimal decode should error if bytes are empty") {
      expectDecodeError[BigDecimal](
        AvroValue.Bytes(ByteVector.empty),
        unsafeSchema[BigDecimal],
        "Error decoding BigDecimal: Unable to decode decimal from empty bytes"
      )
    }

    pureTest("Codec decimal decode should decode using the scale of the writer schema") {
      expectDecodeIs[BigDecimal](
        unsafeEncode(BigDecimal("123.45678")),
        Right(BigDecimal("1234.5678")),
        Some(Schema.Bytes.withLogicalType(LogicalType.decimal(10, 4)))
      )
    }

    pureTest("Codec decimal decode should decode bytes") {
      val value = BigDecimal("123.45678")
      expectDecodeIs[BigDecimal](
        unsafeEncode(value),
        Right(value)
      )
    }
  }

  test("Codec decoder should decode using codec for type") {
    val decoder = Codec[Int].decoder(Schema.Int).value
    forall { (n: Int) =>
      expect.eql(n, decoder.decode(AvroValue.Int(n)).value)
    }
  }

  pureTest("Codec decoder should error if the writer schema is invalid") {
    val result = Codec[Int].decoder(Schema.Union(Vector(Schema.Int, Schema.Int)))
    expect.eql("""Duplicate int in union ["int","int"]""", result.swap.value.message)
  }

  pureTest("Codec decoder should error if the schema is invalid") {
    val result = Codec.decimal(precision = -1, scale = -1).decoder(Schema.Bytes)
    expect.eql("Invalid decimal precision -1: must be positive", result.swap.value.message)
  }

  pureTest("Codec decoder should decode values of unions using the written type") {
    expectDecodeIs[Long](
      AvroValue.Union(1, AvroValue.Int(1)),
      Right(1L),
      Some(Schema.Union(Vector(Schema.Null, Schema.Int)))
    )
  }

  pureTest("Codec decoder should error if the written type of a union cannot be decoded") {
    expectDecodeError[Long](
      AvroValue.Union(0, AvroValue.Null),
      Schema.Union(Vector(Schema.Null, Schema.Int)),
      "Error decoding Long: Got unexpected schema type null, expected schema type long"
    )
  }

  pureTest("Codec decoder should error if the index is invalid for the union") {
    expectDecodeError[Long](
      AvroValue.Union(2, AvroValue.Int(1)),
      Schema.Union(Vector(Schema.Null, Schema.Int)),
      "Invalid index 2 for union with 2 types"
    )
  }

  pureTest("Codec decoder should error if the value is not a union value") {
    expectDecodeError[Long](
      AvroValue.Int(1),
      Schema.Union(Vector(Schema.Null, Schema.Int)),
      "Invalid int value for union"
    )
  }

  pureTest("Codec decoder should error if the writer schema references an undefined name") {
    expectDecodeError[Int](
      AvroValue.Int(1),
      Schema.Ref(Schema.Name("Undefined", Some("com.example"))),
      "Undefined name com.example.Undefined"
    )
  }

  pureTest("Codec double schema should be encoded as double") {
    expectSchemaIs[Double] {
      """"double""""
    }
  }

  pureTest("Codec double encode should encode as double") {
    val value = 123d
    expectEncodeIs[Double](
      value,
      Right(AvroValue.Double(value))
    )
  }

  pureTest("Codec double decode should error if schema is not double or promotable to double") {
    expectDecodeError[Double](
      unsafeEncode(123d),
      unsafeSchema[String],
      "Error decoding Double: Got unexpected schema type string, expected schema type double"
    )
  }

  pureTest("Codec double decode should error if value is not double or promotable to double") {
    expectDecodeError[Double](
      unsafeEncode("foo"),
      unsafeSchema[Double],
      "Error decoding Double: Invalid string value for double"
    )
  }

  pureTest("Codec double decode should decode Float as Double") {
    expectDecodeIs[Double](
      unsafeEncode(123f),
      Right(123d),
      Some(unsafeSchema[Float])
    )
  }

  pureTest("Codec double decode should decode Int as Double") {
    expectDecodeIs[Double](
      unsafeEncode(123),
      Right(123d),
      Some(unsafeSchema[Int])
    )
  }

  pureTest("Codec double decode should decode Long as Double") {
    expectDecodeIs[Double](
      unsafeEncode(123L),
      Right(123d),
      Some(unsafeSchema[Long])
    )
  }

  pureTest("Codec double decode should decode Double as Double") {
    val value = 123d
    expectDecodeIs[Double](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec either schema should be encoded as union") {
    expectSchemaIs[Either[Int, Double]] {
      """["int","double"]"""
    }
  }

  pureTest("Codec either encode should encode left using left schema") {
    expectEncodeIs[Either[Int, Double]](
      Left(1),
      Right(AvroValue.Union(0, AvroValue.Int(1)))
    )
  }

  pureTest("Codec either encode should encode right using right schema") {
    expectEncodeIs[Either[Int, Double]](
      Right(1d),
      Right(AvroValue.Union(1, AvroValue.Double(1d)))
    )
  }

  pureTest("Codec either decode should error if schema type not in union") {
    expectDecodeError[Either[Int, Double]](
      unsafeEncode[Either[Int, Double]](Right(1d)),
      unsafeSchema[String],
      "Error decoding Either: Missing alternative for string in union"
    )
  }

  pureTest("Codec either decode should decode left value as left") {
    expectDecodeIs[Either[Int, Double]](
      unsafeEncode[Either[Int, Double]](Left(1)),
      Right(Left(1))
    )
  }

  pureTest("Codec either decode should decode right value as right") {
    expectDecodeIs[Either[Int, Double]](
      unsafeEncode[Either[Int, Double]](Right(1d)),
      Right(Right(1d))
    )
  }

  test("Codec encoder should encode using codec for type") {
    val encoder = Codec[Int].encoder.value
    forall { (n: Int) =>
      expect.same(AvroValue.Int(n), encoder.encode(n).value)
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
      expectSchemaIs[SealedTraitEnum] {
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
      expectEncodeIs[SealedTraitEnum](
        FirstInSealedTraitEnum,
        Right(AvroValue.Enum("first"))
      )
    }

    pureTest("Codec enum decode should error if schema type is not enum") {
      expectDecodeError[SealedTraitEnum](
        unsafeEncode[SealedTraitEnum](FirstInSealedTraitEnum),
        unsafeSchema[String],
        "Error decoding vulcan.examples.SealedTraitEnum: Got unexpected schema type string, expected schema type enum"
      )
    }

    pureTest("Codec enum decode should error if value is not enum") {
      expectDecodeError[SealedTraitEnum](
        unsafeEncode(10),
        unsafeSchema[SealedTraitEnum],
        "Error decoding vulcan.examples.SealedTraitEnum: Invalid int value for enum vulcan.examples.SealedTraitEnum"
      )
    }

    pureTest("Codec enum decode should return default value if encoded value is not a schema symbol") {
      expectDecodeIs[SealedTraitEnum](
        AvroValue.Enum("symbol"),
        Right(FirstInSealedTraitEnum),
        Some(
          Schema.Enum(Schema.Name("SealedTraitEnum", Some("vulcan.examples")), Vector("symbol"))
        )
      )
    }

    pureTest(
      "Codec enum decode should error if encoded value is not a schema symbol and there is no default value"
    ) {
      expectDecodeError[SealedTraitEnumNoDefault](
        AvroValue.Enum("symbol"),
        Schema.Enum(
          Schema.Name("SealedTraitEnumNoDefault", Some("vulcan.examples")),
          Vector("symbol")
        ),
        "Error decoding vulcan.examples.SealedTraitEnumNoDefault: symbol is not one of schema symbols [first, second]"
      )
    }

    pureTest("Codec enum decode should decode a valid symbol") {
      expectDecodeIs[SealedTraitEnum](
        unsafeEncode[SealedTraitEnum](FirstInSealedTraitEnum),
        Right(FirstInSealedTraitEnum)
      )
    }

    pureTest("Codec enum decode should decode a valid symbol with a different namespace") {
      expectDecodeIs[SealedTraitEnum](
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

    pureTest("Codec enum decode should decode symbols by name, regardless of their order") {
      expectDecodeIs[SealedTraitEnum](
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
      expectSchemaIs[FixedBoolean] {
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
      expectEncodeIs[FixedBoolean](
        FalseFixedBoolean,
        Right(AvroValue.Fixed(hex"00"))
      ) &&
      expectEncodeIs[FixedBoolean](
        TrueFixedBoolean,
        Right(AvroValue.Fixed(hex"01"))
      )
    }

    pureTest("Codec fixed encode should pad bytes with zeros to the size") {
      implicit val codec: Codec[ByteVector] =
        fixedCodec(size = 3)

      expectEncodeIs[ByteVector](
        hex"01",
        Right(AvroValue.Fixed(hex"010000"))
      )
    }

    pureTest("Codec fixed decode should error if schema is not fixed") {
      expectDecodeError[FixedBoolean](
        unsafeEncode[FixedBoolean](FalseFixedBoolean),
        unsafeSchema[String],
        "Error decoding vulcan.examples.FixedBoolean: Got unexpected schema type string, expected schema type fixed"
      )
    }

    pureTest("Codec fixed decode should error if value is not fixed") {
      expectDecodeError[FixedBoolean](
        AvroValue.Int(123),
        unsafeSchema[FixedBoolean],
        "Error decoding vulcan.examples.FixedBoolean: Invalid int value for fixed vulcan.examples.FixedBoolean"
      )
    }

    pureTest("Codec fixed decode should error if length does not match schema size") {
      expectDecodeError[FixedBoolean](
        AvroValue.Fixed(hex"0001"),
        unsafeSchema[FixedBoolean],
        "Error decoding vulcan.examples.FixedBoolean: Got 2 bytes, expected fixed size 1"
      )
    }

    pureTest("Codec fixed decode should error if size of writer schema is different") {
      expectDecodeError[FixedBoolean](
        AvroValue.Fixed(hex"0001"),
        Schema.Fixed(Schema.Name("FixedBoolean", Some("vulcan.examples")), 2),
        "Error decoding vulcan.examples.FixedBoolean: Got unexpected fixed size 2, expected fixed size 1"
      )
    }

    pureTest("Codec fixed decode should decode as fixed") {
      expectDecodeIs[FixedBoolean](
        unsafeEncode[FixedBoolean](FalseFixedBoolean),
        Right(FalseFixedBoolean)
      ) &&
      expectDecodeIs[FixedBoolean](
        unsafeEncode[FixedBoolean](TrueFixedBoolean),
        Right(TrueFixedBoolean)
      )
    }
  }

  pureTest("Codec float schema should be encoded as float") {
    expectSchemaIs[Float] {
      """"float""""
    }
  }

  pureTest("Codec float encode should encode as float") {
    val value = 123f
    expectEncodeIs[Float](
      value,
      Right(AvroValue.Float(value))
    )
  }

  pureTest("Codec float decode should error if schema is not float or promotable to float") {
    expectDecodeError[Float](
      unsafeEncode(123f),
      unsafeSchema[String],
      "Error decoding Float: Got unexpected schema type string, expected schema type float"
    )
  }

  pureTest("Codec float decode should error if value is not float or promotable to float") {
    expectDecodeError[Float](
      unsafeEncode("foo"),
      unsafeSchema[Float],
      "Error decoding Float: Invalid string value for float"
    )
  }

  pureTest("Codec float decode should decode Int as Float") {
    expectDecodeIs[Float](
      unsafeEncode(123),
      Right(123f),
      Some(unsafeSchema[Int])
    )
  }

  pureTest("Codec float decode should decode Long as Float") {
    expectDecodeIs[Float](
      unsafeEncode(123L),
      Right(123f),
      Some(unsafeSchema[Long])
    )
  }

  pureTest("Codec float decode should decode Float as Float") {
    val value = 123f
    expectDecodeIs[Float](
      unsafeEncode(value),
      Right(value)
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
      expectSchemaIs[PosInt] {
        """"int""""
      }
    }

    pureTest("Codec imapError encode should encode using the underlying codec") {
      expectEncodeIs[PosInt](
        PosInt(1).value,
        Right(unsafeEncode(1))
      )
    }

    pureTest("Codec imapError decode should succeed for valid values") {
      expectDecodeIs[PosInt](
        unsafeEncode(1),
        PosInt(1)
      )
    }

    pureTest("Codec imapError decode should error for invalid values") {
      expectDecodeError[PosInt](
        unsafeEncode(0),
        unsafeSchema[PosInt],
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
      expectSchemaIs[PosInt] {
        """"int""""
      }
    }

    pureTest("Codec imapTry encode should encode using the underlying codec") {
      expectEncodeIs[PosInt](
        PosInt(1).get,
        Right(unsafeEncode(1))
      )
    }

    pureTest("Codec imapTry decode should succeed for valid values") {
      expectDecodeIs[PosInt](
        unsafeEncode(1),
        PosInt(1).toEither.leftMap(e => AvroException(e.toString, e))
      )
    }

    pureTest("Codec imapTry decode should error for invalid values") {
      expectDecodeError[PosInt](
        unsafeEncode(0),
        unsafeSchema[PosInt],
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
        """Invalid name "1Invalid": must start with a letter or underscore""",
        invalid.schema.swap.value.message
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

      val decoder = counting.decoder(Schema.Int).value
      expect.same(Right(1), decoder.decode(AvroValue.Int(1))) &&
      expect.same(Right(1), decoder.decode(AvroValue.Int(2))) &&
      expect.eql(1, calls)
    }

    pureTest("Codec instance decode should include the definitions of referenced names in writer schemas") {
      val inner = Schema.Fixed(Schema.Name("Inner"), 1)

      def outer(innerSchema: Schema): Schema =
        Schema.Record(Schema.Name("Outer"), Vector(Schema.Field("inner", innerSchema)))

      val delegating =
        Codec.instance[Int](Right(Schema.Int), int => Right(AvroValue.Int(int)), codec.decoder)

      val decoder = delegating.decoder(Schema.Union(Vector(inner, outer(Schema.Ref(inner.name))))).value
      val value = AvroValue.Union(1, AvroValue.Record(Vector(AvroValue.Fixed(hex"00"))))
      expect.eql(s"Got schema ${outer(inner)}", decoder.decode(value).swap.value.message)
    }

    pureTest("Codec instance decode should error if the decoder function errors") {
      expect.eql(
        s"Got schema ${Schema.String}",
        codec.decoder(Schema.String).flatMap(_.decode(AvroValue.String("a"))).swap.value.message
      )
    }
  }

  pureTest("Codec instant schema should be encoded as long with logical type timestamp-millis") {
    expectSchemaIs[Instant] {
      """{"type":"long","logicalType":"timestamp-millis"}"""
    }
  }

  pureTest("Codec instant encode should encode as long") {
    val value = Instant.now()
    expectEncodeIs[Instant](
      value,
      Right(AvroValue.Long(value.toEpochMilli))
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
      unsafeEncode("value"),
      unsafeSchema[String],
      "Error decoding Instant: Error decoding Long: Got unexpected schema type string, expected schema type long"
    )
  }

  pureTest("Codec instant decode should error if logical type is missing") {
    expectDecodeError[Instant](
      unsafeEncode(Instant.now()),
      unsafeSchema[Long],
      "Error decoding Instant: Got unexpected missing logical type"
    )
  }

  pureTest("Codec instant decode should error if logical type is not timestamp-millis") {
    expectDecodeError[Instant](
      unsafeEncode(Instant.now()),
      Schema.Long.withLogicalType(LogicalType.timestampMicros),
      "Error decoding Instant: Got unexpected logical type timestamp-micros"
    )
  }

  pureTest("Codec instant decode should error if logical type is invalid") {
    expectDecodeError[Instant](
      AvroValue.Int(1),
      Schema.Int.withLogicalType(LogicalType.timestampMillis),
      "Error decoding Instant: Got invalid logical type timestamp-millis for int"
    )
  }

  pureTest("Codec instant decode should error if logical type is missing in nested writer schemas") {
    expectDecodeError[List[Instant]](
      unsafeEncode(List(Instant.now())),
      Schema.Array(Schema.Long),
      "Error decoding List: Error decoding Instant: Got unexpected missing logical type"
    )
  }

  pureTest("Codec instant decode should check the logical type of the written type of unions") {
    val value = Instant.ofEpochMilli(1767225600000L)

    expectDecodeIs[Instant](
      AvroValue.Union(1, unsafeEncode(value)),
      Right(value),
      Some(Schema.Union(Vector(Schema.Null, unsafeSchema[Instant])))
    ) &&
    expectDecodeError[Instant](
      AvroValue.Union(1, unsafeEncode(value)),
      Schema.Union(Vector(Schema.Null, Schema.Long)),
      "Error decoding Instant: Got unexpected missing logical type"
    )
  }

  pureTest("Codec instant decode should decode as Instant") {
    val value = Instant.ofEpochMilli(1767225600000L)
    expectDecodeIs[Instant](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec instant micros schema should be encoded as long with logical type timestamp-micros") {
    expectSchemaIs[InstantMicros] {
      """{"type":"long","logicalType":"timestamp-micros"}"""
    }
  }

  pureTest("Codec instant micros encode should encode as long") {
    val instantMicros = InstantMicros(Instant.now())
    expectEncodeIs[InstantMicros](
      instantMicros,
      Right(AvroValue.Long(instantMicros.toEpochMicro))
    )
  }

  pureTest("Codec instant micros encode should encode mid-term future Instant") {
    // Epoch 10_000_000_000, or 2286-11-20T17:46:40Z, exceeds Long.MaxValue in nanos
    val futureEpoch = 10L * 1000 * 1000 * 1000
    expectEncodeIs[InstantMicros](
      InstantMicros(Instant.ofEpochSecond(futureEpoch, 0L)),
      Right(AvroValue.Long(futureEpoch * 1000 * 1000))
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
      unsafeEncode(InstantMicros(Instant.now())),
      unsafeSchema[Long],
      "Error decoding InstantMicros: Got unexpected missing logical type"
    )
  }

  pureTest("Codec instant micros decode should error if logical type is not timestamp-micros") {
    expectDecodeError[InstantMicros](
      unsafeEncode(InstantMicros(Instant.now())),
      Schema.Long.withLogicalType(LogicalType.timestampMillis),
      "Error decoding InstantMicros: Got unexpected logical type timestamp-millis"
    )
  }

  pureTest("Codec instant micros decode should truncate Instant to micros") {
    val instant = Instant.now()
    expectDecodeIs[InstantMicros](
      unsafeEncode(InstantMicros(instant)),
      Right(InstantMicros(instant.truncatedTo(ChronoUnit.MICROS)))
    )
  }

  pureTest("Codec instant micros decode should roundtrip negative Instant with microsecond offsets") {
    // 1969-12-31T23:59:59.999999Z is one microsecond before the epoch
    val instantMicros = InstantMicros(Instant.ofEpochSecond(-1L, 999999L * 1000))
    expectDecodeIs[InstantMicros](
      unsafeEncode(instantMicros),
      Right(instantMicros)
    )
  }

  pureTest("Codec instant micros decode should roundtrip the minimum and maximum epoch microseconds") {
    forEach(List(Long.MinValue, Long.MaxValue)) { micros =>
      val decoded = unsafeDecode[InstantMicros](AvroValue.Long(micros))
      expect.same(AvroValue.Long(micros), unsafeEncode(decoded))
    }
  }

  pureTest("Codec int schema should be encoded as int") {
    expectSchemaIs[Int] {
      """"int""""
    }
  }

  pureTest("Codec int encode should encode as int") {
    val value = 123
    expectEncodeIs[Int](
      value,
      Right(AvroValue.Int(value))
    )
  }

  pureTest("Codec int decode should error if schema is not int") {
    expectDecodeError[Int](
      unsafeEncode(123),
      unsafeSchema[Long],
      "Error decoding Int: Got unexpected schema type long, expected schema type int"
    )
  }

  pureTest("Codec int decode should error if value is not int") {
    expectDecodeError[Int](
      unsafeEncode(10L),
      unsafeSchema[Int],
      "Error decoding Int: Invalid long value for int"
    )
  }

  pureTest("Codec int decode should decode as Int") {
    val value = 123
    expectDecodeIs[Int](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec jsonDecoder should decode values using the writer schema") {
    val decoder = Codec[Long].jsonDecoder(unsafeSchema[Int]).value
    expect.same(Right(1L), decoder.decode(Json.fromInt(1))) &&
    expect.same(Right(2L), decoder.decode(Json.fromInt(2)))
  }

  pureTest("Codec jsonDecoder should error if the writer schema is invalid") {
    val result = Codec[Int].jsonDecoder(Schema.Union(Vector(Schema.Int, Schema.Int)))
    expect.eql("""Duplicate int in union ["int","int"]""", result.swap.value.message)
  }

  pureTest("Codec jsonDecoder should error if the schema is invalid") {
    val result = Codec.decimal(precision = -1, scale = -1).jsonDecoder(Schema.Bytes)
    expect.eql("Invalid decimal precision -1: must be positive", result.swap.value.message)
  }

  pureTest("Codec jsonDecoder should error if the json does not match the writer schema") {
    val decoder = Codec[Int].jsonDecoder(unsafeSchema[Int]).value
    expect.eql(
      "Invalid string for int",
      decoder.decode(Json.fromString("badValue")).swap.value.message
    )
  }

  pureTest("Codec jsonDecoder should error if the writer schema does not match the type") {
    val decoder = Codec[Int].jsonDecoder(unsafeSchema[String]).value
    expect.eql(
      "Error decoding Int: Got unexpected schema type string, expected schema type int",
      decoder.decode(Json.fromString("1")).swap.value.message
    )
  }

  pureTest("Codec jsonEncoder should encode to avro json") {
    val encoder = Codec[Int].jsonEncoder.value
    expect.same(Right(Json.fromInt(1)), encoder.encode(1)) &&
    expect.same(Right(Json.fromInt(2)), encoder.encode(2))
  }

  pureTest("Codec list schema should be encoded as array") {
    expectSchemaIs[List[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec list encode should encode as array using encoder for underlying type") {
    expectEncodeIs[List[Int]](
      List(1, 2, 3),
      Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
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
      unsafeEncode(List(1, 2, 3)),
      unsafeSchema[Int],
      "Error decoding List: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec list decode should error if value is not array") {
    expectDecodeError[List[Int]](
      unsafeEncode(10),
      unsafeSchema[List[Int]],
      "Error decoding List: Invalid int value for array"
    )
  }

  pureTest("Codec list decode should error if an item cannot be decoded") {
    expectDecodeError[List[Int]](
      AvroValue.Array(Vector(AvroValue.Long(1L))),
      Schema.Array(Schema.Long),
      "Error decoding List: Error decoding Int: Got unexpected schema type long, expected schema type int"
    )
  }

  pureTest("Codec list decode should decode items using the writer schema") {
    expectDecodeIs[List[Long]](
      AvroValue.Array(Vector(AvroValue.Int(1), AvroValue.Int(2))),
      Right(List(1L, 2L)),
      Some(Schema.Array(Schema.Int))
    )
  }

  pureTest("Codec list decode should decode empty arrays even if items cannot be decoded") {
    expectDecodeIs[List[Int]](
      AvroValue.Array(Vector.empty),
      Right(List.empty),
      Some(Schema.Array(Schema.String))
    )
  }

  pureTest("Codec list decode should decode as List") {
    val value = List(1, 2, 3)
    expectDecodeIs[List[Int]](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec localDate schema should be encoded as int with logical type date") {
    expectSchemaIs[LocalDate] {
      """{"type":"int","logicalType":"date"}"""
    }
  }

  pureTest("Codec localDate encode should encode as int") {
    val value = LocalDate.now(ZoneOffset.UTC)
    expectEncodeIs[LocalDate](
      value,
      Right(AvroValue.Int(value.toEpochDay.toInt))
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
      unsafeEncode(LocalDate.now(ZoneOffset.UTC)),
      unsafeSchema[Int],
      "Error decoding LocalDate: Got unexpected missing logical type"
    )
  }

  pureTest("Codec localDate decode should decode int as local date") {
    val value = LocalDate.now(ZoneOffset.UTC)
    expectDecodeIs[LocalDate](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec localDateTime schema should be encoded as long with logical type local-timestamp-millis") {
    expectSchemaIs[LocalDateTime] {
      """{"type":"long","logicalType":"local-timestamp-millis"}"""
    }
  }

  pureTest("Codec localDateTime encode should encode as long") {
    val value = LocalDateTime.ofInstant(Instant.ofEpochMilli(1767225600000L), ZoneOffset.UTC)
    expectEncodeIs[LocalDateTime](
      value,
      Right(AvroValue.Long(1767225600000L))
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
      unsafeEncode(LocalDateTime.now(ZoneOffset.UTC)),
      unsafeSchema[Long],
      "Error decoding LocalDateTime: Got unexpected missing logical type"
    )
  }

  pureTest("Codec localDateTime decode should error if logical type is not local-timestamp-millis") {
    expectDecodeError[LocalDateTime](
      unsafeEncode(LocalDateTime.now(ZoneOffset.UTC)),
      Schema.Long.withLogicalType(LogicalType.timestampMillis),
      "Error decoding LocalDateTime: Got unexpected logical type timestamp-millis"
    )
  }

  pureTest("Codec localDateTime decode should decode as LocalDateTime") {
    val value = LocalDateTime.ofInstant(Instant.ofEpochMilli(1767225600000L), ZoneOffset.UTC)
    expectDecodeIs[LocalDateTime](
      unsafeEncode(value),
      Right(value)
    )
  }

  locally {
    implicit val codec: Codec[LocalTime] = Codec.localTimeMicros

    pureTest("Codec localTimeMicros schema should be encoded as long with logical type time-micros") {
      expectSchemaIs[LocalTime] {
        """{"type":"long","logicalType":"time-micros"}"""
      }
    }

    pureTest("Codec localTimeMicros encode should encode as long") {
      val value = LocalTime.now(ZoneOffset.UTC)
      expectEncodeIs[LocalTime](
        value,
        Right(AvroValue.Long(value.toNanoOfDay / 1000L))
      )
    }

    pureTest("Codec localTimeMicros decode should error if logical type is not time-micros") {
      expectDecodeError[LocalTime](
        unsafeEncode(LocalTime.now(ZoneOffset.UTC)),
        unsafeSchema[Long],
        "Error decoding LocalTime: Got unexpected missing logical type"
      )
    }

    pureTest("Codec localTimeMicros decode should error if value is outside of the day") {
      forEach(List(-1L, 86400000000L)) { micros =>
        expectDecodeError[LocalTime](
          AvroValue.Long(micros),
          unsafeSchema[LocalTime],
          s"Error decoding LocalTime: Got unexpected Long value $micros, expected value in range 0 to 86399999999"
        )
      }
    }

    pureTest("Codec localTimeMicros decode should decode long as local time-micros") {
      val value = LocalTime.now(ZoneOffset.UTC)
      expectDecodeIs[LocalTime](
        unsafeEncode(value),
        Right(value.truncatedTo(ChronoUnit.MICROS))
      )
    }
  }

  locally {
    implicit val codec: Codec[LocalTime] = Codec.localTimeMillis

    pureTest("Codec localTimeMillis schema should be encoded as int with logical type time-millis") {
      expectSchemaIs[LocalTime] {
        """{"type":"int","logicalType":"time-millis"}"""
      }
    }

    pureTest("Codec localTimeMillis encode should encode as int") {
      val value = LocalTime.now(ZoneOffset.UTC)
      expectEncodeIs[LocalTime](
        value,
        Right(AvroValue.Int((value.toNanoOfDay / 1000000L).toInt))
      )
    }

    pureTest("Codec localTimeMillis decode should error if logical type is not time-millis") {
      expectDecodeError[LocalTime](
        unsafeEncode(LocalTime.now(ZoneOffset.UTC)),
        unsafeSchema[Int],
        "Error decoding LocalTime: Got unexpected missing logical type"
      )
    }

    pureTest("Codec localTimeMillis decode should error if value is outside of the day") {
      forEach(List(-1, 86400000)) { millis =>
        expectDecodeError[LocalTime](
          AvroValue.Int(millis),
          unsafeSchema[LocalTime],
          s"Error decoding LocalTime: Got unexpected Int value $millis, expected value in range 0 to 86399999"
        )
      }
    }

    pureTest("Codec localTimeMillis decode should decode int as local time-millis") {
      val value = LocalTime.now(ZoneOffset.UTC)
      expectDecodeIs[LocalTime](
        unsafeEncode(value),
        Right(value.truncatedTo(ChronoUnit.MILLIS))
      )
    }
  }

  pureTest("Codec long schema should be encoded as long") {
    expectSchemaIs[Long] {
      """"long""""
    }
  }

  pureTest("Codec long encode should encode as long") {
    val value = 123L
    expectEncodeIs[Long](
      value,
      Right(AvroValue.Long(value))
    )
  }

  pureTest("Codec long decode should error if schema is not int or long") {
    expectDecodeError[Long](
      unsafeEncode(123L),
      unsafeSchema[String],
      "Error decoding Long: Got unexpected schema type string, expected schema type long"
    )
  }

  pureTest("Codec long decode should error if value is not int or long") {
    expectDecodeError[Long](
      unsafeEncode(123.0),
      unsafeSchema[Long],
      "Error decoding Long: Invalid double value for long"
    )
  }

  pureTest("Codec long decode should decode int as long") {
    expectDecodeIs[Long](
      unsafeEncode(123),
      Right(123L),
      Some(unsafeSchema[Int])
    )
  }

  pureTest("Codec long decode should decode long as long") {
    val value = 123L
    expectDecodeIs[Long](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec map schema should be encoded as map") {
    expectSchemaIs[Map[String, Int]] {
      """{"type":"map","values":"int"}"""
    }
  }

  pureTest("Codec map encode should encode as map using encoder for value") {
    expectEncodeIs[Map[String, Int]](
      Map("key" -> 1),
      Right(AvroValue.Map(Vector("key" -> unsafeEncode(1))))
    )
  }

  pureTest("Codec map decode should error if schema is not map") {
    expectDecodeError[Map[String, Int]](
      unsafeEncode[Map[String, Int]](Map("key" -> 1)),
      unsafeSchema[Int],
      "Error decoding Map: Got unexpected schema type int, expected schema type map"
    )
  }

  pureTest("Codec map decode should error if value is not map") {
    expectDecodeError[Map[String, Int]](
      unsafeEncode(123),
      unsafeSchema[Map[String, Int]],
      "Error decoding Map: Invalid int value for map"
    )
  }

  pureTest("Codec map decode should decode values using the writer schema") {
    expectDecodeIs[Map[String, Long]](
      AvroValue.Map(Vector("key" -> AvroValue.Int(1))),
      Right(Map("key" -> 1L)),
      Some(Schema.Map(Schema.Int))
    )
  }

  pureTest("Codec map decode should decode the last value for duplicate keys") {
    expectDecodeIs[Map[String, Int]](
      AvroValue.Map(Vector("key" -> AvroValue.Int(1), "key" -> AvroValue.Int(2))),
      Right(Map("key" -> 2))
    )
  }

  pureTest("Codec map decode should decode to map using decoder for value") {
    expectDecodeIs[Map[String, Int]](
      unsafeEncode[Map[String, Int]](Map("key" -> 1)),
      Right(Map("key" -> 1))
    )
  }

  pureTest("Codec none schema should be encoded as null") {
    expectSchemaIs[None.type] {
      """"null""""
    }
  }

  pureTest("Codec none encode should encode as null") {
    expectEncodeIs[None.type](
      None,
      Right(AvroValue.Null)
    )
  }

  pureTest("Codec none decode should error if schema is not null") {
    expectDecodeError[None.type](
      unsafeEncode(None),
      unsafeSchema[Int],
      "Error decoding None: Got unexpected schema type int, expected schema type null"
    )
  }

  pureTest("Codec none decode should error if value is not null") {
    expectDecodeError[None.type](
      unsafeEncode(10),
      unsafeSchema[None.type],
      "Error decoding None: Invalid int value for null"
    )
  }

  pureTest("Codec none decode should decode null as None") {
    expectDecodeIs[None.type](
      unsafeEncode(()),
      Right(None)
    )
  }

  pureTest("Codec nonEmptyChain schema should be encoded as array") {
    expectSchemaIs[NonEmptyChain[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec nonEmptyChain encode should encode as array using encoder for underlying type") {
    expectEncodeIs[NonEmptyChain[Int]](
      NonEmptyChain(1, 2, 3),
      Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
    )
  }

  pureTest("Codec nonEmptyChain decode should error if schema is not array") {
    expectDecodeError[NonEmptyChain[Int]](
      unsafeEncode(NonEmptyChain(1, 2, 3)),
      unsafeSchema[Int],
      "Error decoding NonEmptyChain: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec nonEmptyChain decode should error if value is not array") {
    expectDecodeError[NonEmptyChain[Int]](
      unsafeEncode(10),
      unsafeSchema[NonEmptyChain[Int]],
      "Error decoding NonEmptyChain: Invalid int value for array"
    )
  }

  pureTest("Codec nonEmptyChain decode should error on empty collection") {
    expectDecodeError[NonEmptyChain[Int]](
      unsafeEncode(Chain.empty[Int]),
      unsafeSchema[NonEmptyChain[Int]],
      "Error decoding NonEmptyChain: Got unexpected empty collection"
    )
  }

  pureTest("Codec nonEmptyChain decode should decode as NonEmptyChain") {
    val value = NonEmptyChain(1, 2, 3)
    expectDecodeIs[NonEmptyChain[Int]](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec nonEmptyList schema should be encoded as array") {
    expectSchemaIs[NonEmptyList[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec nonEmptyList encode should encode as array using encoder for underlying type") {
    expectEncodeIs[NonEmptyList[Int]](
      NonEmptyList.of(1, 2, 3),
      Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
    )
  }

  pureTest("Codec nonEmptyList decode should error if schema is not array") {
    expectDecodeError[NonEmptyList[Int]](
      unsafeEncode(NonEmptyList.of(1, 2, 3)),
      unsafeSchema[Int],
      "Error decoding NonEmptyList: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec nonEmptyList decode should error if value is not array") {
    expectDecodeError[NonEmptyList[Int]](
      unsafeEncode(10),
      unsafeSchema[NonEmptyList[Int]],
      "Error decoding NonEmptyList: Invalid int value for array"
    )
  }

  pureTest("Codec nonEmptyList decode should error on empty collection") {
    expectDecodeError[NonEmptyList[Int]](
      unsafeEncode(List.empty[Int]),
      unsafeSchema[NonEmptyList[Int]],
      "Error decoding NonEmptyList: Got unexpected empty collection"
    )
  }

  pureTest("Codec nonEmptyList decode should decode as NonEmptyList") {
    val value = NonEmptyList.of(1, 2, 3)
    expectDecodeIs[NonEmptyList[Int]](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec nonEmptyMap schema should be encoded as map") {
    expectSchemaIs[NonEmptyMap[String, Int]] {
      """{"type":"map","values":"int"}"""
    }
  }

  pureTest("Codec nonEmptyMap encode should encode as map using encoder for value") {
    expectEncodeIs[NonEmptyMap[String, Int]](
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

  pureTest("Codec nonEmptyMap decode should error if schema is not map") {
    expectDecodeError[NonEmptyMap[String, Int]](
      unsafeEncode[NonEmptyMap[String, Int]](NonEmptyMap.one("key", 1)),
      unsafeSchema[Int],
      "Error decoding NonEmptyMap: Got unexpected schema type int, expected schema type map"
    )
  }

  pureTest("Codec nonEmptyMap decode should error if value is not map") {
    expectDecodeError[NonEmptyMap[String, Int]](
      unsafeEncode(123),
      unsafeSchema[NonEmptyMap[String, Int]],
      "Error decoding NonEmptyMap: Invalid int value for map"
    )
  }

  pureTest("Codec nonEmptyMap decode should error on empty collection") {
    expectDecodeError[NonEmptyMap[String, Int]](
      unsafeEncode(Map.empty[String, Int]),
      unsafeSchema[NonEmptyMap[String, Int]],
      "Error decoding NonEmptyMap: Got unexpected empty collection"
    )
  }

  pureTest("Codec nonEmptyMap decode should decode to map using decoder for value") {
    val value = NonEmptyMap.of("key1" -> 1, "key2" -> 2, "key3" -> 3)
    expectDecodeIs[NonEmptyMap[String, Int]](
      unsafeEncode[NonEmptyMap[String, Int]](value),
      Right(value)
    )
  }

  pureTest("Codec nonEmptySet schema should be encoded as array") {
    expectSchemaIs[NonEmptySet[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec nonEmptySet encode should encode as array using encoder for underlying type") {
    expectEncodeIs[NonEmptySet[Int]](
      NonEmptySet.of(1, 2, 3),
      Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
    )
  }

  pureTest("Codec nonEmptySet decode should error if schema is not array") {
    expectDecodeError[NonEmptySet[Int]](
      unsafeEncode(NonEmptySet.of(1, 2, 3)),
      unsafeSchema[Int],
      "Error decoding NonEmptySet: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec nonEmptySet decode should error if value is not array") {
    expectDecodeError[NonEmptySet[Int]](
      unsafeEncode(10),
      unsafeSchema[NonEmptySet[Int]],
      "Error decoding NonEmptySet: Invalid int value for array"
    )
  }

  pureTest("Codec nonEmptySet decode should error on empty collection") {
    expectDecodeError[NonEmptySet[Int]](
      unsafeEncode(Set.empty[Int]),
      unsafeSchema[NonEmptySet[Int]],
      "Error decoding NonEmptySet: Got unexpected empty collection"
    )
  }

  pureTest("Codec nonEmptySet decode should decode as NonEmptySet") {
    val value = NonEmptySet.of(1, 2, 3)
    expectDecodeIs[NonEmptySet[Int]](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec nonEmptyVector schema should be encoded as array") {
    expectSchemaIs[NonEmptyVector[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec nonEmptyVector encode should encode as array using encoder for underlying type") {
    expectEncodeIs[NonEmptyVector[Int]](
      NonEmptyVector.of(1, 2, 3),
      Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
    )
  }

  pureTest("Codec nonEmptyVector decode should error if schema is not array") {
    expectDecodeError[NonEmptyVector[Int]](
      unsafeEncode(NonEmptyVector.of(1, 2, 3)),
      unsafeSchema[Int],
      "Error decoding NonEmptyVector: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec nonEmptyVector decode should error if value is not array") {
    expectDecodeError[NonEmptyVector[Int]](
      unsafeEncode(10),
      unsafeSchema[NonEmptyVector[Int]],
      "Error decoding NonEmptyVector: Invalid int value for array"
    )
  }

  pureTest("Codec nonEmptyVector decode should error on empty collection") {
    expectDecodeError[NonEmptyVector[Int]](
      unsafeEncode(Vector.empty[Int]),
      unsafeSchema[NonEmptyVector[Int]],
      "Error decoding NonEmptyVector: Got unexpected empty collection"
    )
  }

  pureTest("Codec nonEmptyVector decode should decode as NonEmptyVector") {
    val value = NonEmptyVector.of(1, 2, 3)
    expectDecodeIs[NonEmptyVector[Int]](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec option schema should be encoded as union") {
    expectSchemaIs[Option[Double]] {
      """["null","double"]"""
    }
  }

  pureTest("Codec option schema should include the types of unions") {
    expectSchemaIs[Option[Either[Int, String]]] {
      """["null","int","string"]"""
    }
  }

  pureTest("Codec option schema should capture errors on nested unions") {
    expectSchemaError[Option[Option[Int]]] {
      """Duplicate null in union ["null","null","int"]"""
    }
  }

  pureTest("Codec option encode should encode Some") {
    expectEncodeIs[Option[Int]](
      Some(1),
      Right(AvroValue.Union(1, AvroValue.Int(1)))
    )
  }

  pureTest("Codec option encode should encode None as null") {
    expectEncodeIs[Option[Int]](
      None,
      Right(AvroValue.Union(0, AvroValue.Null))
    )
  }

  pureTest("Codec option encode should encode Some using the types of unions") {
    expectEncodeIs[Option[Either[Int, String]]](
      Some(Right("a")),
      Right(AvroValue.Union(2, AvroValue.String("a")))
    )
  }

  pureTest("Codec option decode should error if schema is not in union") {
    expectDecodeError[Option[Int]](
      unsafeEncode(Option(1)),
      unsafeSchema[String],
      "Error decoding Option: Error decoding Int: Got unexpected schema type string, expected schema type int"
    )
  }

  pureTest("Codec option decode should decode if schema is part of union") {
    expectDecodeIs[Option[Int]](
      AvroValue.Int(1),
      Right(Some(1)),
      Some(unsafeSchema[Int])
    )
  }

  pureTest("Codec option decode should decode even if there is one schema in union") {
    expectDecodeIs[Option[Int]](
      AvroValue.Union(0, AvroValue.Int(1)),
      Right(Some(1)),
      Some(Schema.Union(Vector(Schema.Int)))
    )
  }

  pureTest("Codec option decode should decode even if there is not null in union") {
    expectDecodeIs[Option[Int]](
      AvroValue.Union(0, AvroValue.Int(1)),
      Right(Some(1)),
      Some(Schema.Union(Vector(Schema.Int, Schema.String)))
    )
  }

  pureTest("Codec option decode should decode if there are more than two schemas in union") {
    expectDecodeIs[Option[Int]](
      AvroValue.Union(0, AvroValue.Int(1)),
      Right(Some(1)),
      Some(Schema.Union(Vector(Schema.Int, Schema.String, Schema.Null)))
    )
  }

  pureTest("Codec option decode should support null as first schema type in union") {
    expectDecodeIs[Option[Int]](
      AvroValue.Union(1, AvroValue.Int(1)),
      Right(Some(1)),
      Some(Schema.Union(Vector(Schema.Null, Schema.Int)))
    )
  }

  pureTest("Codec option decode should support null as second schema type in union") {
    expectDecodeIs[Option[Int]](
      AvroValue.Union(0, AvroValue.Int(1)),
      Right(Some(1)),
      Some(Schema.Union(Vector(Schema.Int, Schema.Null)))
    )
  }

  pureTest("Codec option decode should decode the written type using its schema") {
    expectDecodeIs[Option[Long]](
      AvroValue.Union(1, AvroValue.Int(1)),
      Right(Some(1L)),
      Some(Schema.Union(Vector(Schema.Null, Schema.Int)))
    )
  }

  pureTest("Codec option decode should decode using the types of unions") {
    expectDecodeIs[Option[Either[Int, String]]](
      AvroValue.Union(2, AvroValue.String("a")),
      Right(Some(Right("a")))
    )
  }

  pureTest("Codec option decode should decode null as None") {
    expectDecodeIs[Option[Int]](
      unsafeEncode(Option.empty[Int]),
      Right(None)
    )
  }

  pureTest("Codec option decode should decode null as None if the writer schema is not a union") {
    expectDecodeIs[Option[Int]](
      AvroValue.Null,
      Right(None),
      Some(Schema.Null)
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
      expectSchemaIs[Option[FirstOrSecond]] {
        """["null","int","double"]"""
      }
    }

    pureTest("Codec option.union encode should encode none as null") {
      expect.same(Right(AvroValue.Union(0, AvroValue.Null)), codec.encode(None))
    }

    test("Codec option.union encode should encode first as int") {
      forall { (n: Int) =>
        expect.same(Right(AvroValue.Union(1, AvroValue.Int(n))), codec.encode(Some(First(n))))
      }
    }

    test("Codec option.union encode should encode second as double") {
      forall { (n: Double) =>
        expect.same(Right(AvroValue.Union(2, AvroValue.Double(n))), codec.encode(Some(Second(n))))
      }
    }

    pureTest("Codec option.union decode should decode null as none") {
      expect.same(
        Right(None),
        codec
          .decoder(unsafeSchema[Option[FirstOrSecond]])
          .flatMap(_.decode(AvroValue.Union(0, AvroValue.Null)))
      )
    }

    test("Codec option.union decode should decode int as first") {
      val decoder = codec.decoder(unsafeSchema[Option[FirstOrSecond]]).value
      forall { (n: Int) =>
        expect.same(Right(Some(First(n))), decoder.decode(AvroValue.Union(1, AvroValue.Int(n))))
      }
    }

    test("Codec option.union decode should decode double as second") {
      val decoder = codec.decoder(unsafeSchema[Option[FirstOrSecond]]).value
      forall { (n: Double) =>
        expect.same(Right(Some(Second(n))), decoder.decode(AvroValue.Union(2, AvroValue.Double(n))))
      }
    }
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
      expect.eql(
        """{"type":"record","name":"CaseClassTwoFields","namespace":"vulcan.examples","doc":"some documentation for example","fields":[{"name":"name","type":"string","doc":"some doc","default":"default name","order":"descending","aliases":["TheAlias"],"custom":"value"},{"name":"age","type":"int"}],"custom":[1,2,3],"aliases":["FirstAlias","SecondAlias"]}""",
        Codec[CaseClassTwoFields].schema.value.toString()
      )
    }

    pureTest("Codec record schema should error if default value can't be encoded") {
      implicit val intCodec: Codec[Int] =
        Codec.int.imapErrors(_ => Left(AvroException("error")))(_ => Left(AvroException("error")))

      implicit val caseClassFieldCodec: Codec[CaseClassField] =
        Codec.record[CaseClassField]("CaseClassField", "") { field =>
          field("value", _.value, default = Some(10)).map(CaseClassField(_))
        }

      expect.eql(
        "Error encoding default for field value: error",
        caseClassFieldCodec.schema.swap.value.message
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

      expectSchemaIs[Test] {
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

      expectSchemaIs[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":["int","null"],"default":0}]}"""
      }
    }

    pureTest("Codec record schema default should support null default value") {
      case class Test(value: Unit)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(())).map(Test(_))
        }

      expectSchemaIs[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"null","default":null}]}"""
      }
    }

    pureTest("Codec record schema default should support boolean default value") {
      case class Test(value: Boolean)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(false)).map(Test(_))
        }

      expectSchemaIs[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"boolean","default":false}]}"""
      }
    }

    pureTest("Codec record schema default should support int default value") {
      case class Test(value: Int)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(0)).map(Test(_))
        }

      expectSchemaIs[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"int","default":0}]}"""
      }
    }

    pureTest("Codec record schema default should support long default value") {
      case class Test(value: Long)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(0L)).map(Test(_))
        }

      expectSchemaIs[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"long","default":0}]}"""
      }
    }

    pureTest("Codec record schema default should support float default value") {
      case class Test(value: Float)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(1.5f)).map(Test(_))
        }

      expectSchemaIs[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":"float","default":1.5}]}"""
      }
    }

    pureTest("Codec record schema default should support double default value") {
      case class Test(value: Double)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(1.5d)).map(Test(_))
        }

      expectSchemaIs[Test] {
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

      expectSchemaIs[Test] {
        s"""{"type":"record","name":"Test","fields":[{"name":"value","type":"bytes","default":"$expectedDefault"}]}"""
      }
    }

    pureTest("Codec record schema default should support string default value") {
      case class Test(value: String)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some("default-value")).map(Test(_))
        }

      expectSchemaIs[Test] {
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

      expectSchemaIs[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"record","name":"Inner","fields":[{"name":"value","type":"int"},{"name":"customEnum","type":{"type":"enum","name":"CustomEnum","symbols":["first","second"]},"default":"first"}]},"default":{"value":0,"customEnum":"second"}}]}"""
      }
    }

    pureTest("Codec record schema default should support enum default value") {
      case class Test(value: CustomEnum)

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(First)).map(Test(_))
        }

      expectSchemaIs[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"enum","name":"CustomEnum","symbols":["first","second"]},"default":"first"}]}"""
      }
    }

    pureTest("Codec record schema default should support array default value") {
      case class Test(value: List[Int])

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(List(123, 456))).map(Test(_))
        }

      expectSchemaIs[Test] {
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

      expectSchemaIs[Test] {
        """{"type":"record","name":"Test","fields":[{"name":"value","type":{"type":"array","items":{"type":"record","name":"Element","fields":[{"name":"value","type":"int"}]}},"default":[{"value":123},{"value":456}]}]}"""
      }
    }

    pureTest("Codec record schema default should support map default value") {
      case class Test(value: Map[String, Int])

      implicit val testCodec: Codec[Test] =
        Codec.record("Test", "") { field =>
          field("value", _.value, default = Some(Map("key" -> 0))).map(Test(_))
        }

      expectSchemaIs[Test] {
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

      expectSchemaIs[Test] {
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

      expectSchemaIs[Test] {
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

      expectSchemaIs[Test] {
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
      expectEncodeIs[CaseClassTwoFields](
        CaseClassTwoFields("name", 0),
        Right(AvroValue.Record(Vector(AvroValue.String("name"), AvroValue.Int(0))))
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

      expectEncodeIs[Test](
        Test(None),
        Right(AvroValue.Record(Vector(AvroValue.Union(0, AvroValue.Null))))
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

      expectEncodeIs[Test](
        Test(None),
        Right(AvroValue.Record(Vector(AvroValue.Union(1, AvroValue.Null))))
      )
    }

    locally {
      val nameField = Schema.Field("name", Schema.String)
      val ageField = Schema.Field("age", Schema.Int)

      def writerSchema(fields: Schema.Field*): Schema =
        Schema.Record(Schema.Name("CaseClassTwoFields", Some("vulcan.examples")), fields.toVector)

      pureTest("Codec record decode should error if schema is not record") {
        expectDecodeError[CaseClassTwoFields](
          unsafeEncode(CaseClassTwoFields("name", 123)),
          unsafeSchema[String],
          "Error decoding vulcan.examples.CaseClassTwoFields: Got unexpected schema type string, expected schema type record"
        )
      }

      pureTest("Codec record decode should error if value is not record") {
        expectDecodeError[CaseClassTwoFields](
          unsafeEncode(123),
          unsafeSchema[CaseClassTwoFields],
          "Error decoding vulcan.examples.CaseClassTwoFields: Invalid int value for record vulcan.examples.CaseClassTwoFields"
        )
      }

      pureTest("Codec record decode should error if the record does not have a value for every field") {
        expectDecodeError[CaseClassTwoFields](
          AvroValue.Record(Vector(AvroValue.String("name"))),
          unsafeSchema[CaseClassTwoFields],
          "Error decoding vulcan.examples.CaseClassTwoFields: Invalid record with 1 values for record vulcan.examples.CaseClassTwoFields with 2 fields"
        )
      }

      pureTest("Codec record decode should error if any field without default value is missing") {
        expectDecodeError[CaseClassTwoFields](
          AvroValue.Record(Vector(AvroValue.String("name"))),
          writerSchema(nameField),
          "Error decoding vulcan.examples.CaseClassTwoFields: Record writer schema is missing field 'age'"
        )
      }

      pureTest("Codec record decode should error if a field cannot be decoded") {
        expectDecodeError[CaseClassTwoFields](
          AvroValue.Record(Vector(AvroValue.String("name"), AvroValue.String("123"))),
          writerSchema(nameField, Schema.Field("age", Schema.String)),
          "Error decoding vulcan.examples.CaseClassTwoFields: Error decoding field age: Error decoding Int: Got unexpected schema type string, expected schema type int"
        )
      }

      pureTest("Codec record decode should decode if field with default value is missing") {
        expectDecodeIs[CaseClassTwoFields](
          AvroValue.Record(Vector(AvroValue.Int(123))),
          Right(CaseClassTwoFields("default name", 123)),
          Some(writerSchema(ageField))
        )
      }

      pureTest("Codec record decode should decode fields in a different order") {
        expectDecodeIs[CaseClassTwoFields](
          AvroValue.Record(Vector(AvroValue.Int(123), AvroValue.String("name"))),
          Right(CaseClassTwoFields("name", 123)),
          Some(writerSchema(ageField, nameField))
        )
      }

      pureTest("Codec record decode should ignore fields which are not part of the record") {
        expectDecodeIs[CaseClassTwoFields](
          AvroValue.Record(
            Vector(AvroValue.String("name"), AvroValue.Double(1.0), AvroValue.Int(123))
          ),
          Right(CaseClassTwoFields("name", 123)),
          Some(writerSchema(nameField, Schema.Field("other", Schema.Double), ageField))
        )
      }

      pureTest("Codec record decode should decode fields using the writer schema") {
        case class Test(value: Long)

        implicit val testCodec: Codec[Test] =
          Codec.record("Test", "") { field =>
            field("value", _.value).map(Test(_))
          }

        expectDecodeIs[Test](
          AvroValue.Record(Vector(AvroValue.Int(1))),
          Right(Test(1L)),
          Some(Schema.Record(Schema.Name("Test"), Vector(Schema.Field("value", Schema.Int))))
        )
      }

      pureTest("Codec record decode should decode as case class") {
        expectDecodeIs[CaseClassTwoFields](
          unsafeEncode(CaseClassTwoFields("name", 123)),
          Right(CaseClassTwoFields("name", 123))
        )
      }

      pureTest("Codec record decode should support None as default value") {
        case class Test(value: Option[Int])

        implicit val testCodec: Codec[Test] =
          Codec.record("Test", "") { field =>
            field("value", _.value, default = Some(None)).map(Test(_))
          }

        expectDecodeIs[Test](
          unsafeEncode(Test(None)),
          Right(Test(None))
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

        expectDecodeIs[Test](
          unsafeEncode(Test(None)),
          Right(Test(None))
        )
      }

      pureTest("Codec record decode should decode field with aliased name") {
        case class Aliased(aliasedField: Int)

        implicit val codec: Codec[Aliased] =
          Codec.record("CaseClassField", "") { field =>
            field("aliasedField", _.aliasedField, aliases = Seq("value")).map(Aliased(_))
          }

        expectDecodeIs[Aliased](
          unsafeEncode(CaseClassField(3)),
          Right(Aliased(3)),
          Some(unsafeSchema[CaseClassField])
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

        expectDecodeIs[Node](
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

  pureTest("Codec seq schema should be encoded as array") {
    expectSchemaIs[Seq[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec seq encode should encode as array using encoder for underlying type") {
    expectEncodeIs[Seq[Int]](
      Seq(1, 2, 3),
      Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
    )
  }

  pureTest("Codec seq decode should error if schema is not array") {
    expectDecodeError[Seq[Int]](
      unsafeEncode(Seq(1, 2, 3)),
      unsafeSchema[Int],
      "Error decoding Seq: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec seq decode should error if value is not array") {
    expectDecodeError[Seq[Int]](
      unsafeEncode(10),
      unsafeSchema[Seq[Int]],
      "Error decoding Seq: Invalid int value for array"
    )
  }

  pureTest("Codec seq decode should decode as Seq") {
    val value = Seq(1, 2, 3)
    expectDecodeIs[Seq[Int]](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec set schema should be encoded as array") {
    expectSchemaIs[Set[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec set encode should encode as array using encoder for underlying type") {
    expectEncodeIs[Set[Int]](
      Set(1, 2, 3),
      Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
    )
  }

  pureTest("Codec set decode should error if schema is not array") {
    expectDecodeError[Set[Int]](
      unsafeEncode(Set(1, 2, 3)),
      unsafeSchema[Int],
      "Error decoding Set: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec set decode should error if value is not array") {
    expectDecodeError[Set[Int]](
      unsafeEncode(10),
      unsafeSchema[Set[Int]],
      "Error decoding Set: Invalid int value for array"
    )
  }

  pureTest("Codec set decode should decode as Set") {
    val value = Set(1, 2, 3)
    expectDecodeIs[Set[Int]](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec short schema should be encoded as int") {
    expectSchemaIs[Short] {
      """"int""""
    }
  }

  pureTest("Codec short encode should encode as int") {
    val value = 1.toShort
    expectEncodeIs[Short](
      value,
      Right(AvroValue.Int(1))
    )
  }

  pureTest("Codec short decode should error if schema is not int") {
    expectDecodeError[Short](
      unsafeEncode(1.toShort),
      unsafeSchema[String],
      "Error decoding Short: Error decoding Int: Got unexpected schema type string, expected schema type int"
    )
  }

  pureTest("Codec short decode should error if value is not int") {
    expectDecodeError[Short](
      unsafeEncode("value"),
      unsafeSchema[Short],
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
        unsafeEncode(nonShort),
        unsafeSchema[Short],
        s"Error decoding Short: Got unexpected Int value $nonShort, expected value in range -32768 to 32767"
      )
    }
  }

  test("Codec short decode should decode as short") {
    forall { (short: Short) =>
      expectDecodeIs[Short](
        unsafeEncode(short),
        Right(short)
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
    expectSchemaIs[String] {
      """"string""""
    }
  }

  pureTest("Codec string encode should encode as string") {
    val value = "abc"
    expectEncodeIs[String](
      value,
      Right(AvroValue.String(value))
    )
  }

  pureTest("Codec string decode should error if schema is not string or bytes") {
    expectDecodeError[String](
      unsafeEncode("abc"),
      unsafeSchema[Int],
      "Error decoding String: Got unexpected schema type int, expected schema type string"
    )
  }

  pureTest("Codec string decode should error if value is not string or bytes") {
    expectDecodeError[String](
      unsafeEncode(10),
      unsafeSchema[String],
      "Error decoding String: Invalid int value for string"
    )
  }

  pureTest("Codec string decode should decode string as string") {
    val value = "abc"
    expectDecodeIs[String](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec string decode should decode bytes as string") {
    expectDecodeIs[String](
      AvroValue.Bytes(ByteVector("abc".getBytes(StandardCharsets.UTF_8))),
      Right("abc"),
      Some(unsafeSchema[ByteVector])
    )
  }

  pureTest("Codec string decode should decode a slice of bytes as string") {
    expectDecodeIs[String](
      AvroValue.Bytes(ByteVector("xabcx".getBytes(StandardCharsets.UTF_8)).slice(1, 4)),
      Right("abc"),
      Some(unsafeSchema[ByteVector])
    )
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
      expectSchemaIs[SealedTraitCaseClass] {
        """[{"type":"record","name":"FirstInSealedTraitCaseClass","namespace":"com.example","fields":[{"name":"value","type":"int"}]},{"type":"record","name":"SecondInSealedTraitCaseClass","namespace":"com.example","fields":[{"name":"value","type":"string"}]},{"type":"array","items":"int"}]"""
      }
    }

    pureTest("Codec union schema should encode as union") {
      implicit val codec: Codec[Either[String, String]] =
        unionOf(enumCodec("Suit", "a"), enumCodec("Suit", "b"))

      expectSchemaIs[Either[String, String]] {
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
      expectEncodeIs[SealedTraitCaseClassIncomplete](
        FirstInSealedTraitCaseClassIncomplete(0),
        Right(AvroValue.Union(0, AvroValue.Int(0)))
      )
    }

    pureTest("Codec union decode should error if there is no alternative for the written type") {
      expectDecodeError[Either[Int, String]](
        AvroValue.Boolean(true),
        Schema.Boolean,
        "Error decoding Either: Missing alternative for boolean in union"
      )
    }

    pureTest("Codec union decode should decode using the alternative for the written type") {
      expectDecodeIs[Either[Int, String]](
        AvroValue.Union(1, AvroValue.Int(1)),
        Right(Left(1)),
        Some(Schema.Union(Vector(Schema.String, Schema.Int)))
      )
    }

    pureTest("Codec union decode should decode written types which are not unions") {
      expectDecodeIs[Either[Int, String]](
        AvroValue.String("a"),
        Right(Right("a")),
        Some(Schema.String)
      )
    }

    pureTest("Codec union decode should decode using the alternative of the same type before promotion") {
      expectDecodeIs[Either[Double, Long]](
        AvroValue.Long(1L),
        Right(Right(1L)),
        Some(Schema.Long)
      )
    }

    pureTest("Codec union decode should decode using promotion if there is no alternative of the same type") {
      expectDecodeIs[Either[Long, String]](
        AvroValue.Int(1),
        Right(Left(1L)),
        Some(Schema.Int)
      )
    }

    pureTest("Codec union decode should return the error of the alternative for the written type") {
      expectDecodeError[Either[Byte, String]](
        AvroValue.Int(1000),
        Schema.Int,
        "Error decoding Either: Error decoding Byte: Got unexpected Int value 1000, expected value in range -128 to 127"
      )
    }

    pureTest("Codec union decode should decode named types using the full name") {
      implicit val codec: Codec[Either[String, String]] =
        unionOf(enumCodec("Suit", "a"), enumCodec("Suit", "b"))

      expectDecodeIs[Either[String, String]](
        AvroValue.Enum("A"),
        Right(Right("A")),
        Some(enumSchema("Suit", "b"))
      )
    }

    pureTest("Codec union decode should decode named types using an alias") {
      implicit val codec: Codec[Either[String, String]] =
        unionOf(enumCodec("Card", "a"), enumCodec("Suit", "b", aliases = List("a.Old")))

      expectDecodeIs[Either[String, String]](
        AvroValue.Enum("A"),
        Right(Right("A")),
        Some(enumSchema("Old", "a"))
      )
    }

    pureTest("Codec union decode should decode named types using the name if there is no full name") {
      implicit val codec: Codec[Either[String, String]] =
        unionOf(enumCodec("Card", "a"), enumCodec("Suit", "b"))

      expectDecodeIs[Either[String, String]](
        AvroValue.Enum("A"),
        Right(Right("A")),
        Some(enumSchema("Suit", "c"))
      )
    }

    pureTest("Codec union decode should error if there is no named type of the same kind") {
      implicit val codec: Codec[Either[String, String]] =
        unionOf(enumCodec("Card", "a"), enumCodec("Suit", "b"))

      expectDecodeError[Either[String, String]](
        AvroValue.Fixed(hex"00"),
        Schema.Fixed(Schema.Name("Suit", Some("b")), 1),
        "Missing alternative for fixed b.Suit in union"
      )
    }

    pureTest("Codec union decode should error if schema is not in union") {
      expectDecodeError[SealedTraitCaseClass](
        AvroValue.String("value"),
        unsafeSchema[String],
        "Missing alternative for string in union"
      )
    }

    pureTest("Codec union decode should decode if schema is part of union") {
      expectDecodeIs[SealedTraitCaseClass](
        unsafeEncode(FirstInSealedTraitCaseClass(0)),
        Right(FirstInSealedTraitCaseClass(0)),
        Some(unsafeSchema[FirstInSealedTraitCaseClass])
      )
    }

    pureTest("Codec union decode should decode if schema is union with records of the same name") {
      expectDecodeIs[SealedTraitCaseClassSharedName](
        unsafeEncode(Second.SharedNameSealedTraitCaseClass("hello")),
        Right(Second.SharedNameSealedTraitCaseClass("hello")),
        Some(unsafeSchema[Second.SharedNameSealedTraitCaseClass])
      )
    }

    pureTest("Codec union decode should error if value is not a union value") {
      expectDecodeError[SealedTraitCaseClass](
        AvroValue.Double(123d),
        unsafeSchema[SealedTraitCaseClass],
        "Invalid double value for union"
      )
    }

    pureTest("Codec union decode should error if value is null and not an alternative") {
      expectDecodeError[SealedTraitCaseClass](
        AvroValue.Null,
        unsafeSchema[SealedTraitCaseClass],
        "Invalid null value for union"
      )
    }

    pureTest("Codec union decode should error if no alternative with the name of the written type") {
      expectDecodeError[SealedTraitCaseClass](
        unsafeEncode(CaseClassInSealedTraitCaseClassSingle(0)),
        unsafeSchema[CaseClassInSealedTraitCaseClassSingle],
        "Missing alternative for record vulcan.examples.CaseClassInSealedTraitCaseClassSingle in union"
      )
    }

    pureTest("Codec union decode should decode using schema and decoder for alternative") {
      expectDecodeIs[SealedTraitCaseClass](
        unsafeEncode[SealedTraitCaseClass](FirstInSealedTraitCaseClass(0)),
        Right(FirstInSealedTraitCaseClass(0))
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

      expectDecodeIs[SealedTraitCaseClass](
        unsafeEncode[SealedTraitCaseClass](SecondInSealedTraitCaseClass("foo"))(
          SealedTraitCaseClass.sealedTraitCaseClassCodec
        ),
        Right(SecondInSealedTraitCaseClass("foo")),
        Some(SealedTraitCaseClass.sealedTraitCaseClassCodec.schema.value)
      )
    }
  }

  pureTest("Codec unit schema should be encoded as null") {
    expectSchemaIs[Unit] {
      """"null""""
    }
  }

  pureTest("Codec unit encode should encode as null") {
    val value = ()
    expectEncodeIs[Unit](
      value,
      Right(AvroValue.Null)
    )
  }

  pureTest("Codec unit decode should error if schema is not null") {
    expectDecodeError[Unit](
      unsafeEncode(()),
      unsafeSchema[Int],
      "Error decoding Unit: Got unexpected schema type int, expected schema type null"
    )
  }

  pureTest("Codec unit decode should error if value is not null") {
    expectDecodeError[Unit](
      unsafeEncode(10),
      unsafeSchema[Unit],
      "Error decoding Unit: Invalid int value for null"
    )
  }

  pureTest("Codec unit decode should decode null as unit") {
    expectDecodeIs[Unit](
      unsafeEncode(()),
      Right(())
    )
  }

  locally {
    val uuid = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")

    pureTest("Codec uuid schema should be encoded as string with logical type uuid") {
      expectSchemaIs[UUID] {
        """{"type":"string","logicalType":"uuid"}"""
      }
    }

    pureTest("Codec uuid encode should encode as string") {
      val value = uuid
      expectEncodeIs[UUID](
        value,
        Right(AvroValue.String(value.toString))
      )
    }

    pureTest("Codec uuid decode should error if logical type is not uuid") {
      expectDecodeError[UUID](
        unsafeEncode(uuid),
        unsafeSchema[String],
        "Error decoding UUID: Got unexpected missing logical type"
      )
    }

    pureTest("Codec uuid decode should error if value is not uuid") {
      expectDecodeError[UUID](
        AvroValue.String("not-uuid"),
        unsafeSchema[UUID],
        "Error decoding UUID: Invalid UUID string: not-uuid"
      )
    }

    pureTest("Codec uuid decode should decode uuid") {
      val value = uuid
      expectDecodeIs[UUID](
        unsafeEncode(value),
        Right(value)
      )
    }
  }

  pureTest("Codec vector schema should be encoded as array") {
    expectSchemaIs[Vector[String]] {
      """{"type":"array","items":"string"}"""
    }
  }

  pureTest("Codec vector encode should encode as array using encoder for underlying type") {
    expectEncodeIs[Vector[Int]](
      Vector(1, 2, 3),
      Right(AvroValue.Array(Vector(unsafeEncode(1), unsafeEncode(2), unsafeEncode(3))))
    )
  }

  pureTest("Codec vector decode should error if schema is not array") {
    expectDecodeError[Vector[Int]](
      unsafeEncode(Vector(1, 2, 3)),
      unsafeSchema[Int],
      "Error decoding Vector: Got unexpected schema type int, expected schema type array"
    )
  }

  pureTest("Codec vector decode should error if value is not array") {
    expectDecodeError[Vector[Int]](
      unsafeEncode(10),
      unsafeSchema[Vector[Int]],
      "Error decoding Vector: Invalid int value for array"
    )
  }

  pureTest("Codec vector decode should decode as Vector") {
    val value = Vector(1, 2, 3)
    expectDecodeIs[Vector[Int]](
      unsafeEncode(value),
      Right(value)
    )
  }

  pureTest("Codec vector decode should decode as Vector for any number of items") {
    forEach((0 to 64).toList) { size =>
      val value = Vector.range(0, size)
      expectDecodeIs[Vector[Int]](
        unsafeEncode(value),
        Right(value)
      )
    }
  }
}

trait CodecSpecHelpers {
  self: BaseSuite =>

  def unsafeSchema[A](implicit codec: Codec[A]): Schema =
    codec.schema.value

  def unsafeEncode[A](a: A)(implicit codec: Codec[A]): AvroValue =
    codec.encode(a).value

  def unsafeDecode[A](value: AvroValue)(implicit codec: Codec[A]): A =
    codec.schema.flatMap(codec.decoder).flatMap(_.decode(value)).value

  def expectSchemaIs[A](
    expectedSchema: String
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.eql(expectedSchema, codec.schema.value.toString)

  def expectEncodeIs[A](
    a: A,
    encoded: Either[AvroException, AvroValue]
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.same(encoded.value, codec.encode(a).value)

  def expectDecodeIs[A](
    value: AvroValue,
    decoded: Either[AvroException, A],
    schema: Option[Schema] = None
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations = {
    val decode =
      schema
        .map(codec.decoder(_).flatMap(_.decode(value)).value)
        .getOrElse(unsafeDecode(value))
    expect.same(decoded.value, decode)
  }

  def expectSchemaError[A](
    expectedErrorMessage: String
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.eql(expectedErrorMessage, codec.schema.swap.value.message)

  def expectDecodeError[A](
    value: AvroValue,
    schema: Schema,
    expectedErrorMessage: String
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.eql(expectedErrorMessage, codec.decoder(schema).flatMap(_.decode(value)).swap.value.message)

  def expectEncodeError[A](
    a: A,
    expectedErrorMessage: String
  )(implicit codec: Codec[A], loc: SourceLocation): Expectations =
    expect.eql(expectedErrorMessage, codec.encode(a).swap.value.message)
}
