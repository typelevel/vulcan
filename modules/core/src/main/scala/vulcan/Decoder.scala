/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.MonadError
import vulcan.internal.DecoderMonadError

/**
  * Decodes Avro values, written using a writer schema, to type `A`.
  *
  * Note decoders are compiled once for the writer schema using the
  * [[Codec#decoder]] function, and can be reused as many times
  * as required, while also supporting concurrent decodes.
  */
trait Decoder[A] {

  /** Attempts to decode the specified value. */
  def decode(value: AvroValue): Either[AvroException, A]
}

object Decoder {
  implicit final val decoderMonadError: MonadError[Decoder, AvroException] =
    new DecoderMonadError[Decoder, AvroValue] {
      override protected def decoder[A](f: AvroValue => Either[AvroException, A]): Decoder[A] =
        new Decoder[A] {
          override def decode(value: AvroValue): Either[AvroException, A] =
            f(value)
        }

      override protected def decode[A](fa: Decoder[A], input: AvroValue): Either[AvroException, A] =
        fa.decode(input)
    }
}
