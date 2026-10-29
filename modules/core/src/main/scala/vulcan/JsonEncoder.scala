/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json

/**
  * Encodes values of type `A` to Avro `Json`.
  *
  * Note encoders are compiled once for the schema using the
  * [[Codec#jsonEncoder]] function, and can be reused as many
  * times as needed, supporting concurrent encodes.
  */
trait JsonEncoder[A] {

  /** Attempts to encode the specified value to Avro `Json`. */
  def encode(a: A): Either[AvroError, Json]
}
