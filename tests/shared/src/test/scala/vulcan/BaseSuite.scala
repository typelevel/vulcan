/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.Show
import scodec.bits.ByteVector
import weaver.SimpleIOSuite
import weaver.scalacheck.Checkers

abstract class BaseSuite extends SimpleIOSuite with Checkers with EitherValues {
  implicit val byteVectorShow: Show[ByteVector] = Show.fromToString
}
