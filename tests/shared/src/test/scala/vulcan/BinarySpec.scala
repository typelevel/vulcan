/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import org.scalacheck.Gen
import scodec.bits.*
import vulcan.internal.BinaryReader
import vulcan.internal.BinaryWriter
import weaver.Expectations
import weaver.SourceLocation
import weaver.scalacheck.CheckConfig

object BinarySpec extends BaseSuite {
  override def checkConfig: CheckConfig =
    super.checkConfig.withMinimumSuccessful(1000)

  private def write(f: BinaryWriter => Unit): ByteVector = {
    val writer = new BinaryWriter(0)
    f(writer)
    writer.toByteVector
  }

  /** Reads the bytes, expecting all bytes to be read. */
  private def read(bytes: ByteVector)(f: BinaryReader => Expectations)(implicit
    loc: SourceLocation
  ): Expectations = {
    val reader = BinaryReader(bytes)
    f(reader) && expect.eql(0, reader.remaining)
  }

  /** Reads the bytes, returning the message of the exception if reading fails. */
  private def readError(bytes: ByteVector)(f: BinaryReader => Any): Option[String] =
    try {
      f(BinaryReader(bytes))
      None
    } catch {
      case error: AvroException => Some(error.getMessage)
    }

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

  pureTest("BinaryWriter should write booleans as a single byte") {
    expect.same(hex"00", write(_.writeBoolean(false))) &&
    expect.same(hex"01", write(_.writeBoolean(true)))
  }

  pureTest("BinaryWriter should write ints with zig-zag variable-length encoding") {
    expect.same(hex"00", write(_.writeInt(0))) &&
    expect.same(hex"01", write(_.writeInt(-1))) &&
    expect.same(hex"02", write(_.writeInt(1))) &&
    expect.same(hex"03", write(_.writeInt(-2))) &&
    expect.same(hex"04", write(_.writeInt(2))) &&
    expect.same(hex"7f", write(_.writeInt(-64))) &&
    expect.same(hex"8001", write(_.writeInt(64))) &&
    expect.same(hex"feffffff0f", write(_.writeInt(Int.MaxValue))) &&
    expect.same(hex"ffffffff0f", write(_.writeInt(Int.MinValue)))
  }

  pureTest("BinaryWriter should write longs with zig-zag variable-length encoding") {
    expect.same(hex"00", write(_.writeLong(0L))) &&
    expect.same(hex"01", write(_.writeLong(-1L))) &&
    expect.same(hex"02", write(_.writeLong(1L))) &&
    expect.same(hex"7f", write(_.writeLong(-64L))) &&
    expect.same(hex"8001", write(_.writeLong(64L))) &&
    expect.same(hex"feffffffffffffffff01", write(_.writeLong(Long.MaxValue))) &&
    expect.same(hex"ffffffffffffffffff01", write(_.writeLong(Long.MinValue)))
  }

  pureTest("BinaryWriter should write floats and doubles in little-endian order") {
    expect.same(hex"0000803f", write(_.writeFloat(1.0f))) &&
    expect.same(hex"000000000000f03f", write(_.writeDouble(1.0)))
  }

  pureTest("BinaryWriter should write bytes prefixed by their length") {
    expect.same(hex"00", write(_.writeBytes(ByteVector.empty))) &&
    expect.same(hex"040102", write(_.writeBytes(hex"0102")))
  }

  pureTest("BinaryWriter should write fixed without their length") {
    expect.same(hex"0102", write(_.writeFixed(hex"0102")))
  }

  pureTest("BinaryWriter should write strings as UTF-8 prefixed by the length in bytes") {
    expect.same(hex"00", write(_.writeString(""))) &&
    expect.same(hex"06666f6f", write(_.writeString("foo"))) &&
    expect.same(hex"04c3a9", write(_.writeString(string(0xe9)))) &&
    expect.same(hex"06e282ac", write(_.writeString(string(0x20ac)))) &&
    expect.same(hex"08f09f9880", write(_.writeString(string(0x1f600))))
  }

  pureTest("BinaryWriter should write unpaired surrogates as question marks") {
    val high = 0xd83d.toChar.toString
    val low = 0xde00.toChar.toString
    expect.same(hex"023f", write(_.writeString(high))) &&
    expect.same(hex"043f3f", write(_.writeString(low + high))) &&
    expect.same(hex"06613f62", write(_.writeString(s"a${high}b"))) &&
    expect.same(hex"0a3ff09f9880", write(_.writeString(high + high + low)))
  }

  pureTest("BinaryWriter should grow the buffer as needed") {
    val bytes = ByteVector.fill(1000L)(1)
    expect.same(
      bytes ++ bytes ++ bytes,
      write(writer => (1 to 3).foreach(_ => writer.writeFixed(bytes)))
    )
  }

  test("BinaryReader should read written values") {
    forall { (boolean: Boolean, int: Int, long: Long, float: Float, double: Double) =>
      val bytes =
        write { writer =>
          writer.writeBoolean(boolean)
          writer.writeInt(int)
          writer.writeLong(long)
          writer.writeFloat(float)
          writer.writeDouble(double)
        }

      read(bytes) { reader =>
        expect.eql(boolean, reader.readBoolean()) &&
        expect.eql(int, reader.readInt()) &&
        expect.eql(long, reader.readLong()) &&
        expect(java.lang.Float.compare(reader.readFloat(), float) == 0) &&
        expect(java.lang.Double.compare(reader.readDouble(), double) == 0)
      }
    }
  }

  test("BinaryReader should read written bytes, fixed, and strings") {
    forall(Gen.zip(genBytes, genBytes, genString)) { case (bytes, fixed, string) =>
      val written =
        write { writer =>
          writer.writeBytes(bytes)
          writer.writeFixed(fixed)
          writer.writeString(string)
        }

      read(written) { reader =>
        expect.same(bytes, reader.readBytes()) &&
        expect.same(fixed, reader.readFixed(fixed.size.toInt)) &&
        expect.eql(string, reader.readString())
      }
    }
  }

  pureTest("BinaryReader should track the position") {
    val reader = BinaryReader(hex"8001ff")
    expect.eql(64, reader.readInt()) &&
    expect.eql(2, reader.position) &&
    expect.eql(1, reader.remaining)
  }

  pureTest("BinaryReader should read slices and concatenations") {
    read(hex"ffff06666f6f".drop(2))(reader => expect.eql("foo", reader.readString())) &&
    read(hex"06" ++ hex"666f6f")(reader => expect.eql("foo", reader.readString()))
  }

  pureTest("BinaryReader should read ints and longs using more bytes than needed") {
    read(hex"8000")(reader => expect.eql(0, reader.readInt())) &&
    read(hex"8180808000")(reader => expect.eql(-1, reader.readInt())) &&
    read(hex"80808080808080808000")(reader => expect.eql(0L, reader.readLong()))
  }

  pureTest("BinaryReader should not read ints and longs using too many bytes") {
    expect.eql(
      Some("Invalid int with more than 5 bytes at position 0"),
      readError(hex"8080808080")(_.readInt())
    ) &&
    expect.eql(
      Some("Invalid long with more than 10 bytes at position 0"),
      readError(hex"80808080808080808080")(_.readLong())
    )
  }

  pureTest("BinaryReader should only read booleans which are 0 or 1") {
    expect.eql(Some("Invalid boolean 2 at position 0"), readError(hex"02")(_.readBoolean())) &&
    expect.eql(Some("Invalid boolean 255 at position 0"), readError(hex"ff")(_.readBoolean()))
  }

  pureTest("BinaryReader should read strings with invalid UTF-8 using replacement characters") {
    val replacement = 0xfffd.toChar.toString
    read(hex"02ff")(reader => expect.eql(replacement, reader.readString())) &&
    read(hex"0661ff62")(reader => expect.eql(s"a${replacement}b", reader.readString()))
  }

  pureTest("BinaryReader should read the number of items in blocks") {
    read(hex"00")(reader => expect.eql(0L, reader.readBlockCount())) &&
    read(hex"06")(reader => expect.eql(3L, reader.readBlockCount())) &&
    read(hex"050a")(reader => expect.eql(3L, reader.readBlockCount()))
  }

  pureTest("BinaryReader should not read invalid block counts") {
    expect.eql(
      Some("Invalid block count -9223372036854775808 at position 0"),
      readError(hex"ffffffffffffffffff01")(_.readBlockCount())
    ) &&
    expect.eql(
      Some("Invalid negative block size -1 at position 1"),
      readError(hex"0501")(_.readBlockCount())
    )
  }

  pureTest("BinaryReader should not read negative lengths") {
    expect.eql(Some("Invalid negative length -1 at position 0"), readError(hex"01")(_.readBytes())) &&
    expect.eql(
      Some("Invalid negative length -2 at position 1"),
      readError(hex"0003")(reader => (reader.readInt(), reader.readString()))
    )
  }

  pureTest("BinaryReader should not read lengths exceeding the remaining input") {
    expect.eql(
      Some("Invalid length 3 at position 0, exceeding the remaining input"),
      readError(hex"0601")(_.readBytes())
    ) &&
    expect.eql(
      Some("Invalid length 9223372036854775807 at position 0, exceeding the remaining input"),
      readError(hex"feffffffffffffffff01")(_.readString())
    )
  }

  pureTest("BinaryReader should not read past the end of the input") {
    forEach(
      List[BinaryReader => Any](
        _.readBoolean(),
        _.readInt(),
        _.readLong(),
        _.readFloat(),
        _.readDouble(),
        _.readFixed(1)
      )
    ) { f =>
      expect.eql(Some("Unexpected end of input at position 0"), readError(ByteVector.empty)(f))
    } &&
    expect.eql(Some("Unexpected end of input at position 1"), readError(hex"80")(_.readInt())) &&
    expect.eql(Some("Unexpected end of input at position 0"), readError(hex"000000")(_.readFloat())) &&
    expect.eql(Some("Unexpected end of input at position 1"), readError(hex"05")(_.readBlockCount()))
  }
}
