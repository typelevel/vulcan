/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json
import io.circe.ParsingFailure
import scodec.bits.ByteVector

object AvroExceptionSpec extends BaseSuite {
  test("AvroException should use specified message for the exception") {
    forall { (message: String) =>
      val exception = AvroException(message)
      expect.eql(message, exception.message) &&
      expect.eql(message, exception.getMessage())
    }
  }

  test("AvroException should have no cause unless specified") {
    forall { (message: String) =>
      val exception = AvroException(message)
      expect.all(exception.cause.isEmpty, exception.getCause() == null)
    }
  }

  test("AvroException should use specified cause for the exception") {
    forall { (message: String) =>
      val cause = new IllegalArgumentException(message)
      val exception = AvroException(message, cause)
      expect.all(exception.cause.contains(cause), exception.getCause() eq cause)
    }
  }

  test("AvroException should include the message in toString") {
    forall { (message: String) =>
      expect.eql(s"vulcan.AvroException: $message", AvroException(message).toString())
    }
  }

  pureTest("AvroException should not have a stack trace") {
    expect(AvroException("message").getStackTrace().isEmpty)
  }

  pureTest("AvroException ErrorDecodingType should have the exception decoding the type as cause") {
    val decoder = Codec[Int].decoder(Schema.Boolean)
    matches(decoder.flatMap(_.decode(AvroValue.Boolean(true)))) {
      case Left(exception: AvroException.ErrorDecodingType) =>
        matches(exception.getCause()) { case _: AvroException.UnexpectedSchemaType => success }
    }
  }

  pureTest("AvroException InvalidBinary should be the exception for invalid binary") {
    val decoder = Codec[Int].binaryDecoder(Schema.Int)
    matches(decoder.flatMap(_.decode(ByteVector.empty))) { case Left(_: AvroException.InvalidBinary) =>
      success
    }
  }

  pureTest("AvroException InvalidJson should be the exception for invalid JSON") {
    val decoder = Codec[Int].jsonDecoder(Schema.Int)
    matches(decoder.flatMap(_.decode(Json.fromString("1")))) { case Left(_: AvroException.InvalidJson) =>
      success
    }
  }

  pureTest("AvroException InvalidSchema should have the parsing failure as cause for invalid JSON") {
    matches(Schema.parse("{")) { case Left(exception: AvroException.InvalidSchema) =>
      matches(exception.getCause()) { case _: ParsingFailure => success }
    }
  }
}
