/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import cats.MonadError
import scala.annotation.tailrec
import vulcan.AvroException

private[vulcan] abstract class DecoderMonadError[F[_], I] extends MonadError[F, AvroException] {
  protected def decoder[A](f: I => Either[AvroException, A]): F[A]

  protected def decode[A](fa: F[A], input: I): Either[AvroException, A]

  override final def pure[A](a: A): F[A] = {
    val result = Right(a)
    decoder(_ => result)
  }

  override final def map[A, B](fa: F[A])(f: A => B): F[B] =
    decoder(decode(fa, _).map(f))

  override final def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] =
    decoder(input => decode(fa, input).flatMap(a => decode(f(a), input)))

  override final def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
    decoder { input =>
      @tailrec def loop(a: A): Either[AvroException, B] =
        decode(f(a), input) match {
          case Right(Left(next)) => loop(next)
          case Right(Right(b)) => Right(b)
          case Left(error) => Left(error)
        }

      loop(a)
    }

  override final def raiseError[A](error: AvroException): F[A] = {
    val result = Left(error)
    decoder(_ => result)
  }

  override final def handleErrorWith[A](fa: F[A])(f: AvroException => F[A]): F[A] =
    decoder { input =>
      decode(fa, input) match {
        case Left(error) => decode(f(error), input)
        case result => result
      }
    }
}
