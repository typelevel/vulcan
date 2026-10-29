/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import scodec.bits.ByteVector

/**
  * Decodes Avro binary, written using a writer schema, to type `A`.
  *
  * Note decoders are compiled once for the writer schema using the
  * [[Codec#binaryDecoder]] function, and can be reused as many times
  * as required, while also supporting concurrent decodes.
  */
trait BinaryDecoder[A] {

  /** Attempts to decode a value from the start of the bytes, ignoring any remaining bytes. */
  def decode(bytes: ByteVector): Either[AvroError, A]
}
