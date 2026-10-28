/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import java.nio.charset.Charset
import scodec.bits.ByteVector

private[vulcan] object ByteVectors {

  /**
    * Applies the function to the array, offset and length of the bytes, without copying them if
    * they are backed by an array.
    */
  def withArray[A](bytes: ByteVector)(f: (Array[Byte], Int, Int) => A): A = {
    val buffer = bytes.toByteBufferUnsafe
    if (buffer.hasArray) f(buffer.array, buffer.arrayOffset + buffer.position, buffer.remaining)
    else {
      val array = bytes.toArray
      f(array, 0, array.length)
    }
  }

  /** Decodes the bytes as a string, without copying them if they are backed by an array. */
  def decodeString(bytes: ByteVector, charset: Charset): String =
    withArray(bytes)(new String(_, _, _, charset))
}
