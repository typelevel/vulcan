/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import scodec.bits.ByteVector

/**
  * Encodes values of type `A` to Avro binary.
  *
  * Note encoders are compiled once for the schema using the
  * [[Codec#binaryEncoder]] function, and can be reused as
  * many times as needed, supporting concurrent encodes.
  */
trait BinaryEncoder[A] {

  /** Attempts to encode the specified value to Avro binary. */
  def encode(a: A): Either[AvroError, ByteVector]
}
