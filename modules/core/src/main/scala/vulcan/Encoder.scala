/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Contravariant

/**
  * Encodes values of type `A` to Avro.
  *
  * Note encoders are compiled once for the schema using the
  * [[Codec#encoder]] function, and can be reused as many
  * times as needed, supporting concurrent encodes.
  */
trait Encoder[A] {

  /** Attempts to encode the specified value. */
  def encode(a: A): Either[AvroException, AvroValue]
}

object Encoder {
  implicit final val encoderContravariant: Contravariant[Encoder] =
    new Contravariant[Encoder] {
      override final def contramap[A, B](encoder: Encoder[A])(f: B => A): Encoder[B] =
        new Encoder[B] {
          override def encode(b: B): Either[AvroException, AvroValue] =
            encoder.encode(f(b))
        }
    }
}
