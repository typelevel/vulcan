/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scodec.bits.*
import vulcan.internal.ByteVectors

final class ByteVectorsSpec extends BaseSpec {
  private def decode(bytes: ByteVector): String =
    ByteVectors.decodeString(bytes, StandardCharsets.UTF_8)

  describe("ByteVectors") {
    describe("decodeString") {
      it("should decode bytes backed by an array") {
        assert(decode(hex"616263") == "abc")
      }

      it("should decode a slice of bytes backed by an array") {
        assert(decode(hex"78616263".drop(1)) == "abc")
      }

      it("should decode bytes of multiple chunks") {
        assert(decode(hex"61" ++ hex"6263") == "abc")
      }

      it("should decode bytes not backed by an array") {
        val buffer = ByteBuffer.allocateDirect(3)
        buffer.put(hex"616263".toArray)
        buffer.flip()
        val bytes = ByteVector.view(buffer)
        assert(!bytes.toByteBufferUnsafe.hasArray)
        assert(decode(bytes) == "abc")
      }
    }
  }
}
