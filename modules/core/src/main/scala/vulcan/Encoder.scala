/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

/**
  * Encodes values of type `A` to Avro.
  *
  * Note encoders are compiled once for the schema using the
  * [[Codec#encoder]] function, and can be reused as many
  * times as needed, supporting concurrent encodes.
  */
trait Encoder[A] {

  /** Attempts to encode the specified value. */
  def encode(a: A): Either[AvroError, AvroValue]
}
