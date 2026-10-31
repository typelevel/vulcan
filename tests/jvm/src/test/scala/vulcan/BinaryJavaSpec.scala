/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import java.io.ByteArrayOutputStream
import org.apache.avro.io.BinaryDecoder as JavaDecoder
import org.apache.avro.io.BinaryEncoder as JavaEncoder
import org.apache.avro.io.DecoderFactory
import org.apache.avro.io.EncoderFactory
import org.scalacheck.Gen
import scala.util.Try
import scodec.bits.ByteVector
import vulcan.internal.BinaryReader
import vulcan.internal.BinaryWriter
import weaver.scalacheck.CheckConfig

/** Checks the binary encoding against the Java Avro library. */
object BinaryJavaSpec extends BaseSuite {
  override def checkConfig: CheckConfig =
    super.checkConfig.withMinimumSuccessful(1000)

  private def write(f: BinaryWriter => Unit): ByteVector = {
    val writer = new BinaryWriter(0)
    f(writer)
    writer.toByteVector
  }

  private def javaWrite(f: JavaEncoder => Unit): ByteVector =
    javaWriteWith(EncoderFactory.get().binaryEncoder(_, null))(f)

  private def javaWriteWith(
    encoder: ByteArrayOutputStream => JavaEncoder
  )(f: JavaEncoder => Unit): ByteVector = {
    val output = new ByteArrayOutputStream()
    val javaEncoder = encoder(output)
    f(javaEncoder)
    javaEncoder.flush()
    ByteVector.view(output.toByteArray)
  }

  private def javaDecoder(bytes: ByteVector): JavaDecoder =
    DecoderFactory.get().binaryDecoder(bytes.toArray, null)

  /** Returns the value read by both libraries, if either can read a value. */
  private def readBoth[A](bytes: ByteVector)(
    read: BinaryReader => A,
    javaRead: JavaDecoder => A
  ): (Option[A], Option[A]) =
    (Try(read(BinaryReader(bytes))).toOption, Try(javaRead(javaDecoder(bytes))).toOption)

  /** Bytes which are likely to include continuation bits, and are sometimes too short. */
  private val genVarBytes: Gen[ByteVector] =
    Gen
      .choose(0, 12)
      .flatMap { size =>
        Gen.listOfN(
          size,
          Gen.frequency(
            4 -> Gen.choose(Byte.MinValue, Byte.MaxValue),
            2 -> Gen.const(0xff.toByte),
            1 -> Gen.const(0x80.toByte),
            1 -> Gen.const(0x7f.toByte),
            1 -> Gen.const(0.toByte)
          )
        )
      }
      .map(bytes => ByteVector(bytes.toArray))

  private val genBytes: Gen[ByteVector] =
    Gen.listOf(Gen.choose(Byte.MinValue, Byte.MaxValue)).map(bytes => ByteVector(bytes.toArray))

  /** Strings with any characters, including unpaired surrogates. */
  private val genString: Gen[String] =
    Gen.listOf(Gen.choose(Char.MinValue, Char.MaxValue)).map(_.mkString)

  test("BinaryWriter should write the same as the Java Avro library") {
    forall { (boolean: Boolean, int: Int, long: Long, floatBits: Int, doubleBits: Long) =>
      val float = java.lang.Float.intBitsToFloat(floatBits)
      val double = java.lang.Double.longBitsToDouble(doubleBits)

      val bytes =
        write { writer =>
          writer.writeBoolean(boolean)
          writer.writeInt(int)
          writer.writeLong(long)
          writer.writeFloat(float)
          writer.writeDouble(double)
        }

      val javaBytes =
        javaWrite { encoder =>
          encoder.writeBoolean(boolean)
          encoder.writeInt(int)
          encoder.writeLong(long)
          encoder.writeFloat(float)
          encoder.writeDouble(double)
        }

      expect.same(javaBytes, bytes)
    }
  }

  test("BinaryWriter should write bytes, fixed, and strings the same as the Java Avro library") {
    forall(Gen.zip(genBytes, genBytes, genString)) { case (bytes, fixed, string) =>
      val written =
        write { writer =>
          writer.writeBytes(bytes)
          writer.writeFixed(fixed)
          writer.writeString(string)
        }

      val javaWritten =
        javaWrite { encoder =>
          encoder.writeBytes(bytes.toArray)
          encoder.writeFixed(fixed.toArray)
          encoder.writeString(string)
        }

      expect.same(javaWritten, written)
    }
  }

  test("BinaryReader should read ints and longs the same as the Java Avro library") {
    forall(genVarBytes) { bytes =>
      val (int, javaInt) = readBoth(bytes)(_.readInt(), _.readInt())
      val (long, javaLong) = readBoth(bytes)(_.readLong(), _.readLong())
      expect.eql(javaInt, int) && expect.eql(javaLong, long)
    }
  }

  test("BinaryReader should read floats and doubles the same as the Java Avro library") {
    forall(genVarBytes) { bytes =>
      val (float, javaFloat) =
        readBoth(bytes)(
          reader => java.lang.Float.floatToRawIntBits(reader.readFloat()),
          decoder => java.lang.Float.floatToRawIntBits(decoder.readFloat())
        )

      val (double, javaDouble) =
        readBoth(bytes)(
          reader => java.lang.Double.doubleToRawLongBits(reader.readDouble()),
          decoder => java.lang.Double.doubleToRawLongBits(decoder.readDouble())
        )

      expect.eql(javaFloat, float) && expect.eql(javaDouble, double)
    }
  }

  test("BinaryReader should read booleans the same as the Java Avro library, except for invalid booleans") {
    forall(genVarBytes) { bytes =>
      val (boolean, javaBoolean) = readBoth(bytes)(_.readBoolean(), _.readBoolean())
      if (bytes.headOption.exists(byte => byte == 0 || byte == 1)) expect.eql(javaBoolean, boolean)
      else expect.eql(None, boolean)
    }
  }

  test("BinaryReader should read bytes and strings the same as the Java Avro library") {
    forall(Gen.zip(genVarBytes, genBytes)) { case (prefix, payload) =>
      val bytes = prefix ++ payload

      val (read, javaRead) =
        readBoth(bytes)(_.readBytes(), decoder => ByteVector(decoder.readBytes(null)))

      val (string, javaString) = readBoth(bytes)(_.readString(), _.readString())
      expect.same(javaRead, read) && expect.eql(javaString, string)
    }
  }

  test("BinaryReader should read strings with invalid UTF-8 the same as the Java Avro library") {
    forall(genBytes) { payload =>
      val bytes = write(_.writeBytes(payload))
      expect.eql(javaDecoder(bytes).readString(), BinaryReader(bytes).readString())
    }
  }

  test("BinaryReader should read blocks written by the Java Avro library") {
    val factory = new EncoderFactory().configureBlockSize(64)

    def javaWriteArray(values: List[Long]): ByteVector =
      javaWriteWith(factory.blockingBinaryEncoder(_, null)) { encoder =>
        encoder.writeArrayStart()
        encoder.setItemCount(values.size.toLong)
        values.foreach { value =>
          encoder.startItem()
          encoder.writeLong(value)
        }
        encoder.writeArrayEnd()
      }

    // Blocks are written with negative counts followed by their size in bytes
    val negativeCount =
      expect(clue(BinaryReader(javaWriteArray(List.fill(100)(Long.MaxValue))).readLong()) < 0)

    forall { (values: List[Long]) =>
      val bytes = javaWriteArray(values)
      val reader = BinaryReader(bytes)
      val read = List.newBuilder[Long]
      var count = reader.readBlockCount()
      while (count > 0) {
        (1L to count).foreach(_ => read += reader.readLong())
        count = reader.readBlockCount()
      }

      expect.eql(values, read.result()) && expect.eql(0, reader.remaining)
    }.map(negativeCount && _)
  }
}
