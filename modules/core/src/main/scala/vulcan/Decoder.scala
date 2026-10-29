/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

/**
  * Decodes Avro values, written using a writer schema, to type `A`.
  *
  * Note decoders are compiled once for the writer schema using the
  * [[Codec#decoder]] function, and can be reused as many times
  * as required, while also supporting concurrent decodes.
  */
trait Decoder[A] {

  /** Attempts to decode the specified value. */
  def decode(value: AvroValue): Either[AvroError, A]
}
