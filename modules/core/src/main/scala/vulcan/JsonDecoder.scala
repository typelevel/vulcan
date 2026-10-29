/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import io.circe.Json

/**
  * Decodes Avro `Json`, written using a writer schema, to type `A`.
  *
  * Note decoders are compiled once for the writer schema using the
  * [[Codec#jsonDecoder]] function, and can be reused as many times
  * as required, while also supporting concurrent decodes.
  */
trait JsonDecoder[A] {

  /** Attempts to decode the specified Avro `Json`. */
  def decode(json: Json): Either[AvroError, A]
}
