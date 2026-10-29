/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.internal

import java.nio.charset.StandardCharsets
import java.util.Arrays
import scodec.bits.ByteVector
import vulcan.AvroException
import vulcan.AvroException.InvalidBinary

/** Reads values in the Avro binary encoding, throwing [[AvroException]] for invalid input. */
private[vulcan] final class BinaryReader private (bytes: Array[Byte], start: Int, limit: Int) {
  private[this] var index: Int =
    start

  /** The number of bytes read so far. */
  def position: Int =
    index - start

  /** The number of bytes left to read. */
  def remaining: Int =
    limit - index

  def readBoolean(): Boolean = {
    ensureRemaining(1)
    bytes(index) match {
      case 0 =>
        index += 1
        false

      case 1 =>
        index += 1
        true

      case byte =>
        throw invalid(index, s"boolean ${byte & 0xff}")
    }
  }

  /** Reads an int with zig-zag variable-length encoding, using at most 5 bytes. */
  def readInt(): Int = {
    val begin = index
    var value = 0
    var shift = 0
    var byte = 0x80
    while ((byte & 0x80) != 0) {
      if (shift == 35) throw invalid(begin, "int with more than 5 bytes")
      ensureRemaining(1)
      byte = bytes(index) & 0xff
      value |= (byte & 0x7f) << shift
      index += 1
      shift += 7
    }
    (value >>> 1) ^ -(value & 1)
  }

  /** Reads a long with zig-zag variable-length encoding, using at most 10 bytes. */
  def readLong(): Long = {
    val begin = index
    var value = 0L
    var shift = 0
    var byte = 0x80
    while ((byte & 0x80) != 0) {
      if (shift == 70) throw invalid(begin, "long with more than 10 bytes")
      ensureRemaining(1)
      byte = bytes(index) & 0xff
      value |= (byte & 0x7fL) << shift
      index += 1
      shift += 7
    }
    (value >>> 1) ^ -(value & 1)
  }

  /** Reads the bits of a float in little-endian order. */
  def readFloat(): Float = {
    ensureRemaining(4)
    val bits = intBits(index)
    index += 4
    java.lang.Float.intBitsToFloat(bits)
  }

  /** Reads the bits of a double in little-endian order. */
  def readDouble(): Double = {
    ensureRemaining(8)
    val bits = (intBits(index) & 0xffffffffL) | (intBits(index + 4).toLong << 32)
    index += 8
    java.lang.Double.longBitsToDouble(bits)
  }

  /** Reads bytes prefixed by their length. */
  def readBytes(): ByteVector =
    readFixed(readLength())

  /** Reads the given number of bytes. */
  def readFixed(size: Int): ByteVector = {
    ensureRemaining(size.toLong)
    val fixed = Arrays.copyOfRange(bytes, index, index + size)
    index += size
    ByteVector.view(fixed)
  }

  /** Reads a string encoded as UTF-8, prefixed by the length in bytes. */
  def readString(): String = {
    val length = readLength()
    val string = new String(bytes, index, length, StandardCharsets.UTF_8)
    index += length
    string
  }

  /**
    * Reads the number of items in the next block of an array or map, being zero after the last
    * block. A negative number of items is followed by the size of the block in bytes, which is read
    * but not used.
    */
  def readBlockCount(): Long = {
    val begin = index
    val count = readLong()
    if (count >= 0) count
    else if (count == Long.MinValue) throw invalid(begin, s"block count $count")
    else {
      val sizeBegin = index
      val size = readLong()
      if (size < 0) throw invalid(sizeBegin, s"negative block size $size")
      -count
    }
  }

  /** Reads a length, which cannot exceed the remaining bytes. */
  private def readLength(): Int = {
    val begin = index
    val length = readLong()
    if (length < 0) throw invalid(begin, s"negative length $length")
    if (length > remaining)
      throw new InvalidBinary(
        s"Invalid length $length at position ${begin - start}, exceeding the remaining input"
      )
    length.toInt
  }

  private def intBits(at: Int): Int =
    (bytes(at) & 0xff) |
      ((bytes(at + 1) & 0xff) << 8) |
      ((bytes(at + 2) & 0xff) << 16) |
      ((bytes(at + 3) & 0xff) << 24)

  private def ensureRemaining(count: Long): Unit =
    if (count > remaining)
      throw new InvalidBinary(s"Unexpected end of input at position $position")

  private def invalid(at: Int, description: String): AvroException =
    new InvalidBinary(s"Invalid $description at position ${at - start}")
}

private[vulcan] object BinaryReader {

  /** Returns a reader for the bytes, without copying them if they are backed by an array. */
  def apply(bytes: ByteVector): BinaryReader =
    ByteVectors.withArray(bytes) { (array, offset, length) =>
      new BinaryReader(array, offset, offset + length)
    }
}
