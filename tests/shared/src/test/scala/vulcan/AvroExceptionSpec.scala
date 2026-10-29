/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json
import io.circe.ParsingFailure
import scodec.bits.ByteVector

final class AvroExceptionSpec extends BaseSpec {
  describe("AvroException") {
    it("should use specified message for the exception") {
      forAll { (message: String) =>
        val exception = AvroException(message)
        assert(exception.message === message)
        assert(exception.getMessage() === message)
      }
    }

    it("should have no cause unless specified") {
      forAll { (message: String) =>
        val exception = AvroException(message)
        assert(exception.cause.isEmpty)
        assert(exception.getCause() == null)
      }
    }

    it("should use specified cause for the exception") {
      forAll { (message: String) =>
        val cause = new IllegalArgumentException(message)
        val exception = AvroException(message, cause)
        assert(exception.cause.contains(cause))
        assert(exception.getCause() eq cause)
      }
    }

    it("should include the message in toString") {
      forAll { (message: String) =>
        assert(AvroException(message).toString() === s"vulcan.AvroException: $message")
      }
    }

    it("should not have a stack trace") {
      assert(AvroException("message").getStackTrace().isEmpty)
    }

    describe("ErrorDecodingType") {
      it("should have the exception decoding the type as cause") {
        val decoder = Codec[Int].decoder(Schema.Boolean).value
        val exception = decoder.decode(AvroValue.Boolean(true)).swap.value
        assert(exception.isInstanceOf[AvroException.ErrorDecodingType])
        assert(exception.getCause().isInstanceOf[AvroException.UnexpectedSchemaType])
      }
    }

    describe("InvalidBinary") {
      it("should be the exception for invalid binary") {
        val decoder = Codec[Int].binaryDecoder(Schema.Int).value
        val exception = decoder.decode(ByteVector.empty).swap.value
        assert(exception.isInstanceOf[AvroException.InvalidBinary])
      }
    }

    describe("InvalidJson") {
      it("should be the exception for invalid JSON") {
        val decoder = Codec[Int].jsonDecoder(Schema.Int).value
        val exception = decoder.decode(Json.fromString("1")).swap.value
        assert(exception.isInstanceOf[AvroException.InvalidJson])
      }
    }

    describe("InvalidSchema") {
      it("should have the parsing failure as cause for invalid JSON") {
        val exception = Schema.parse("{").swap.value
        assert(exception.isInstanceOf[AvroException.InvalidSchema])
        assert(exception.getCause().isInstanceOf[ParsingFailure])
      }
    }
  }
}
