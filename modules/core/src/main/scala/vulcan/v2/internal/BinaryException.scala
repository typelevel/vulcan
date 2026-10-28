/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.internal

import scala.util.control.NoStackTrace

/** Thrown by [[BinaryReader]] and [[BinaryWriter]] for invalid input. */
private[v2] final class BinaryException(message: String) extends RuntimeException(message) with NoStackTrace
