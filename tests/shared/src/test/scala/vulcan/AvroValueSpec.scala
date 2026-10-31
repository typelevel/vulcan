/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import cats.syntax.all.*
import scodec.bits.ByteVector

object AvroValueSpec extends BaseSuite {
  test("AvroValue Float should be equal for equal values") {
    forall { (float: Float) =>
      expect.eql[AvroValue](AvroValue.Float(float), AvroValue.Float(float)) &&
      expect.eql(AvroValue.Float(float).hashCode, AvroValue.Float(float).hashCode)
    }
  }

  pureTest("AvroValue Float should be equal to itself for NaN") {
    expect.eql[AvroValue](AvroValue.Float(Float.NaN), AvroValue.Float(Float.NaN)) &&
    expect.eql(AvroValue.Float(Float.NaN).hashCode, AvroValue.Float(Float.NaN).hashCode)
  }

  pureTest("AvroValue Float should distinguish positive and negative zero") {
    val negativeZero = java.lang.Float.intBitsToFloat(Int.MinValue)
    expect(clue(AvroValue.Float(0.0f)) != clue(AvroValue.Float(negativeZero)))
  }

  test("AvroValue Double should be equal for equal values") {
    forall { (double: Double) =>
      expect.eql[AvroValue](AvroValue.Double(double), AvroValue.Double(double)) &&
      expect.eql(AvroValue.Double(double).hashCode, AvroValue.Double(double).hashCode)
    }
  }

  pureTest("AvroValue Double should be equal to itself for NaN") {
    expect.eql[AvroValue](AvroValue.Double(Double.NaN), AvroValue.Double(Double.NaN)) &&
    expect.eql(AvroValue.Double(Double.NaN).hashCode, AvroValue.Double(Double.NaN).hashCode)
  }

  pureTest("AvroValue Double should distinguish positive and negative zero") {
    val negativeZero = java.lang.Double.longBitsToDouble(Long.MinValue)
    expect(clue(AvroValue.Double(0.0d)) != clue(AvroValue.Double(negativeZero)))
  }

  pureTest("AvroValue Bytes should be equal for equal contents") {
    expect.eql[AvroValue](AvroValue.Bytes(ByteVector(1, 2, 3)), AvroValue.Bytes(ByteVector(1, 2, 3))) &&
    expect(clue(AvroValue.Bytes(ByteVector(1, 2, 3))) != clue(AvroValue.Bytes(ByteVector(1, 2))))
  }

  pureTest("AvroValue show should have a Show instance consistent with toString") {
    val value: AvroValue = AvroValue.Record(Vector(AvroValue.Int(1), AvroValue.String("a")))
    expect.eql(value.toString, value.show)
  }
}
