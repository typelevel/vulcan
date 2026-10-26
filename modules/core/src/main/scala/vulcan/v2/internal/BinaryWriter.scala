/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2.internal

import java.util.Arrays
import scodec.bits.ByteVector

/** Writes values in the Avro binary encoding to a buffer which grows as needed. */
private[v2] final class BinaryWriter(initialCapacity: Int) {
  private[this] var buffer: Array[Byte] =
    new Array[Byte](math.max(initialCapacity, 16))

  private[this] var size: Int =
    0

  def writeBoolean(value: Boolean): Unit = {
    ensureCapacity(1)
    buffer(size) = if (value) 1.toByte else 0.toByte
    size += 1
  }

  /** Writes the int with zig-zag variable-length encoding. */
  def writeInt(value: Int): Unit = {
    ensureCapacity(5)
    var bits = (value << 1) ^ (value >> 31)
    while ((bits & ~0x7f) != 0) {
      buffer(size) = ((bits & 0x7f) | 0x80).toByte
      size += 1
      bits >>>= 7
    }
    buffer(size) = bits.toByte
    size += 1
  }

  /** Writes the long with zig-zag variable-length encoding. */
  def writeLong(value: Long): Unit = {
    ensureCapacity(10)
    var bits = (value << 1) ^ (value >> 63)
    while ((bits & ~0x7fL) != 0) {
      buffer(size) = ((bits & 0x7f) | 0x80).toByte
      size += 1
      bits >>>= 7
    }
    buffer(size) = bits.toByte
    size += 1
  }

  /** Writes the bits of the float in little-endian order, including NaN payloads. */
  def writeFloat(value: Float): Unit = {
    ensureCapacity(4)
    writeIntBits(java.lang.Float.floatToRawIntBits(value))
  }

  /** Writes the bits of the double in little-endian order, including NaN payloads. */
  def writeDouble(value: Double): Unit = {
    ensureCapacity(8)
    val bits = java.lang.Double.doubleToRawLongBits(value)
    writeIntBits(bits.toInt)
    writeIntBits((bits >>> 32).toInt)
  }

  /** Writes the bytes prefixed by their length. */
  def writeBytes(bytes: ByteVector): Unit = {
    writeLong(bytes.size)
    writeFixed(bytes)
  }

  /** Writes the bytes without their length. */
  def writeFixed(bytes: ByteVector): Unit = {
    ensureCapacity(bytes.size)
    bytes.copyToArray(buffer, size)
    size += bytes.size.toInt
  }

  /** Writes the string encoded as UTF-8, prefixed by the length in bytes. */
  def writeString(value: String): Unit = {
    val length = utf8Length(value)
    writeLong(length)
    ensureCapacity(length)

    var index = 0
    while (index < value.length) {
      val char = value.charAt(index)

      if (char < 0x80) {
        buffer(size) = char.toByte
        size += 1
      } else if (char < 0x800) {
        buffer(size) = (0xc0 | (char >> 6)).toByte
        buffer(size + 1) = (0x80 | (char & 0x3f)).toByte
        size += 2
      } else if (isSurrogatePair(value, index)) {
        val codePoint = Character.toCodePoint(char, value.charAt(index + 1))
        buffer(size) = (0xf0 | (codePoint >> 18)).toByte
        buffer(size + 1) = (0x80 | ((codePoint >> 12) & 0x3f)).toByte
        buffer(size + 2) = (0x80 | ((codePoint >> 6) & 0x3f)).toByte
        buffer(size + 3) = (0x80 | (codePoint & 0x3f)).toByte
        size += 4
        index += 1
      } else if (Character.isSurrogate(char)) {
        buffer(size) = '?'.toByte
        size += 1
      } else {
        buffer(size) = (0xe0 | (char >> 12)).toByte
        buffer(size + 1) = (0x80 | ((char >> 6) & 0x3f)).toByte
        buffer(size + 2) = (0x80 | (char & 0x3f)).toByte
        size += 3
      }

      index += 1
    }
  }

  /** The bytes written so far. The writer must not be used afterwards. */
  def toByteVector: ByteVector =
    if (size == buffer.length) ByteVector.view(buffer)
    else ByteVector.view(Arrays.copyOf(buffer, size))

  private def writeIntBits(bits: Int): Unit = {
    buffer(size) = bits.toByte
    buffer(size + 1) = (bits >>> 8).toByte
    buffer(size + 2) = (bits >>> 16).toByte
    buffer(size + 3) = (bits >>> 24).toByte
    size += 4
  }

  private def ensureCapacity(count: Long): Unit =
    if (count > buffer.length - size) {
      val required = size + count
      if (required > BinaryWriter.MaxSize)
        throw new BinaryException(s"Unable to write more than ${BinaryWriter.MaxSize} bytes")

      val capacity = math.min(math.max(required, buffer.length * 2L), BinaryWriter.MaxSize.toLong)
      buffer = Arrays.copyOf(buffer, capacity.toInt)
    }

  /** The length in bytes of the string encoded as UTF-8. */
  private def utf8Length(value: String): Long = {
    var length = 0L
    var index = 0
    while (index < value.length) {
      val char = value.charAt(index)

      if (char < 0x80) length += 1
      else if (char < 0x800) length += 2
      else if (isSurrogatePair(value, index)) {
        length += 4
        index += 1
      } else if (Character.isSurrogate(char)) length += 1
      else length += 3

      index += 1
    }
    length
  }

  private def isSurrogatePair(value: String, index: Int): Boolean =
    Character.isHighSurrogate(value.charAt(index)) &&
      index + 1 < value.length &&
      Character.isLowSurrogate(value.charAt(index + 1))
}

private[v2] object BinaryWriter {

  /** The maximum array size supported on the JVM. */
  private val MaxSize: Int =
    Int.MaxValue - 8
}
