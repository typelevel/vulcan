/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import org.scalacheck.Gen
import scodec.bits.*
import vulcan.internal.BinaryException
import vulcan.internal.BinaryReader
import vulcan.internal.BinaryWriter

final class BinarySpec extends BaseSpec {
  private def write(f: BinaryWriter => Unit): ByteVector = {
    val writer = new BinaryWriter(0)
    f(writer)
    writer.toByteVector
  }

  /** Reads the value, checking that all bytes were read. */
  private def read[A](bytes: ByteVector)(f: BinaryReader => A): A = {
    val reader = BinaryReader(bytes)
    val value = f(reader)
    assert(reader.remaining == 0)
    value
  }

  private def readError(bytes: ByteVector)(f: BinaryReader => Any): String =
    intercept[BinaryException](f(BinaryReader(bytes))).getMessage

  private def string(codePoints: Int*): String =
    codePoints.map(codePoint => new String(Character.toChars(codePoint))).mkString

  /** Strings without unpaired surrogates. */
  private val genString: Gen[String] =
    Gen
      .listOf(
        Gen.frequency(
          4 -> Gen.choose(0, 0x7f),
          2 -> Gen.choose(0x80, 0xd7ff),
          1 -> Gen.choose(0xe000, 0x10ffff)
        )
      )
      .map(codePoints => string(codePoints: _*))

  private val genBytes: Gen[ByteVector] =
    Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(bytes => ByteVector(bytes.toArray))

  describe("BinaryWriter") {
    it("should write booleans as a single byte") {
      assert(write(_.writeBoolean(false)) == hex"00")
      assert(write(_.writeBoolean(true)) == hex"01")
    }

    it("should write ints with zig-zag variable-length encoding") {
      assert(write(_.writeInt(0)) == hex"00")
      assert(write(_.writeInt(-1)) == hex"01")
      assert(write(_.writeInt(1)) == hex"02")
      assert(write(_.writeInt(-2)) == hex"03")
      assert(write(_.writeInt(2)) == hex"04")
      assert(write(_.writeInt(-64)) == hex"7f")
      assert(write(_.writeInt(64)) == hex"8001")
      assert(write(_.writeInt(Int.MaxValue)) == hex"feffffff0f")
      assert(write(_.writeInt(Int.MinValue)) == hex"ffffffff0f")
    }

    it("should write longs with zig-zag variable-length encoding") {
      assert(write(_.writeLong(0L)) == hex"00")
      assert(write(_.writeLong(-1L)) == hex"01")
      assert(write(_.writeLong(1L)) == hex"02")
      assert(write(_.writeLong(-64L)) == hex"7f")
      assert(write(_.writeLong(64L)) == hex"8001")
      assert(write(_.writeLong(Long.MaxValue)) == hex"feffffffffffffffff01")
      assert(write(_.writeLong(Long.MinValue)) == hex"ffffffffffffffffff01")
    }

    it("should write floats and doubles in little-endian order") {
      assert(write(_.writeFloat(1.0f)) == hex"0000803f")
      assert(write(_.writeDouble(1.0)) == hex"000000000000f03f")
    }

    it("should write bytes prefixed by their length") {
      assert(write(_.writeBytes(ByteVector.empty)) == hex"00")
      assert(write(_.writeBytes(hex"0102")) == hex"040102")
    }

    it("should write fixed without their length") {
      assert(write(_.writeFixed(hex"0102")) == hex"0102")
    }

    it("should write strings as UTF-8 prefixed by the length in bytes") {
      assert(write(_.writeString("")) == hex"00")
      assert(write(_.writeString("foo")) == hex"06666f6f")
      assert(write(_.writeString(string(0xe9))) == hex"04c3a9")
      assert(write(_.writeString(string(0x20ac))) == hex"06e282ac")
      assert(write(_.writeString(string(0x1f600))) == hex"08f09f9880")
    }

    it("should write unpaired surrogates as question marks") {
      val high = 0xd83d.toChar.toString
      val low = 0xde00.toChar.toString
      assert(write(_.writeString(high)) == hex"023f")
      assert(write(_.writeString(low + high)) == hex"043f3f")
      assert(write(_.writeString(s"a${high}b")) == hex"06613f62")
      assert(write(_.writeString(high + high + low)) == hex"0a3ff09f9880")
    }

    it("should grow the buffer as needed") {
      val bytes = ByteVector.fill(1000L)(1)
      assert(
        write(writer => (1 to 3).foreach(_ => writer.writeFixed(bytes))) == bytes ++ bytes ++ bytes
      )
    }
  }

  describe("BinaryReader") {
    it("should read written values") {
      forAll(minSuccessful(1000)) { (boolean: Boolean, int: Int, long: Long, float: Float, double: Double) =>
        val bytes =
          write { writer =>
            writer.writeBoolean(boolean)
            writer.writeInt(int)
            writer.writeLong(long)
            writer.writeFloat(float)
            writer.writeDouble(double)
          }

        read(bytes) { reader =>
          assert(reader.readBoolean() == boolean)
          assert(reader.readInt() == int)
          assert(reader.readLong() == long)
          assert(java.lang.Float.compare(reader.readFloat(), float) == 0)
          assert(java.lang.Double.compare(reader.readDouble(), double) == 0)
        }
      }
    }

    it("should read written bytes, fixed, and strings") {
      forAll(genBytes, genBytes, genString, minSuccessful(1000)) { (bytes, fixed, string) =>
        val written =
          write { writer =>
            writer.writeBytes(bytes)
            writer.writeFixed(fixed)
            writer.writeString(string)
          }

        read(written) { reader =>
          assert(reader.readBytes() == bytes)
          assert(reader.readFixed(fixed.size.toInt) == fixed)
          assert(reader.readString() == string)
        }
      }
    }

    it("should track the position") {
      val reader = BinaryReader(hex"8001ff")
      assert(reader.readInt() == 64)
      assert(reader.position == 2)
      assert(reader.remaining == 1)
    }

    it("should read slices and concatenations") {
      assert(read(hex"ffff06666f6f".drop(2))(_.readString()) == "foo")
      assert(read(hex"06" ++ hex"666f6f")(_.readString()) == "foo")
    }

    it("should read ints and longs using more bytes than needed") {
      assert(read(hex"8000")(_.readInt()) == 0)
      assert(read(hex"8180808000")(_.readInt()) == -1)
      assert(read(hex"80808080808080808000")(_.readLong()) == 0L)
    }

    it("should not read ints and longs using too many bytes") {
      assert(
        readError(hex"8080808080")(
          _.readInt()
        ) == "Invalid int with more than 5 bytes at position 0"
      )

      assert {
        readError(hex"80808080808080808080")(_.readLong()) ==
          "Invalid long with more than 10 bytes at position 0"
      }
    }

    it("should only read booleans which are 0 or 1") {
      assert(readError(hex"02")(_.readBoolean()) == "Invalid boolean 2 at position 0")
      assert(readError(hex"ff")(_.readBoolean()) == "Invalid boolean 255 at position 0")
    }

    it("should read strings with invalid UTF-8 using replacement characters") {
      val replacement = 0xfffd.toChar.toString
      assert(read(hex"02ff")(_.readString()) == replacement)
      assert(read(hex"0661ff62")(_.readString()) == s"a${replacement}b")
    }

    it("should read the number of items in blocks") {
      assert(read(hex"00")(_.readBlockCount()) == 0L)
      assert(read(hex"06")(_.readBlockCount()) == 3L)
      assert(read(hex"050a")(_.readBlockCount()) == 3L)
    }

    it("should not read invalid block counts") {
      assert {
        readError(hex"ffffffffffffffffff01")(_.readBlockCount()) ==
          "Invalid block count -9223372036854775808 at position 0"
      }

      assert(
        readError(hex"0501")(_.readBlockCount()) == "Invalid negative block size -1 at position 1"
      )
    }

    it("should not read negative lengths") {
      assert(readError(hex"01")(_.readBytes()) == "Invalid negative length -1 at position 0")
      assert(readError(hex"0003")(r => {
        r.readInt(); r.readString()
      }) == "Invalid negative length -2 at position 1")
    }

    it("should not read lengths exceeding the remaining input") {
      assert {
        readError(hex"0601")(_.readBytes()) ==
          "Invalid length 3 at position 0, exceeding the remaining input"
      }

      assert {
        readError(hex"feffffffffffffffff01")(_.readString()) ==
          "Invalid length 9223372036854775807 at position 0, exceeding the remaining input"
      }
    }

    it("should not read past the end of the input") {
      List[BinaryReader => Any](
        _.readBoolean(),
        _.readInt(),
        _.readLong(),
        _.readFloat(),
        _.readDouble(),
        _.readFixed(1)
      ).foreach { f =>
        assert(readError(ByteVector.empty)(f) == "Unexpected end of input at position 0")
      }

      assert(readError(hex"80")(_.readInt()) == "Unexpected end of input at position 1")
      assert(readError(hex"000000")(_.readFloat()) == "Unexpected end of input at position 0")
      assert(readError(hex"05")(_.readBlockCount()) == "Unexpected end of input at position 1")
    }
  }
}
