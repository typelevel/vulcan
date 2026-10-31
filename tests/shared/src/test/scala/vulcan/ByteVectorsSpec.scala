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

object ByteVectorsSpec extends BaseSuite {
  private def decode(bytes: ByteVector): String =
    ByteVectors.decodeString(bytes, StandardCharsets.UTF_8)

  pureTest("ByteVectors decodeString should decode bytes backed by an array") {
    expect.eql("abc", decode(hex"616263"))
  }

  pureTest("ByteVectors decodeString should decode a slice of bytes backed by an array") {
    expect.eql("abc", decode(hex"78616263".drop(1)))
  }

  pureTest("ByteVectors decodeString should decode bytes of multiple chunks") {
    expect.eql("abc", decode(hex"61" ++ hex"6263"))
  }

  pureTest("ByteVectors decodeString should decode bytes without an accessible array") {
    val bytes = ByteVector.view(ByteBuffer.wrap(hex"616263".toArray).asReadOnlyBuffer)
    expect(!clue(bytes.toByteBufferUnsafe).hasArray) &&
    expect.eql("abc", decode(bytes))
  }
}
