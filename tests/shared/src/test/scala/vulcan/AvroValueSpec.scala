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
      expect.same(AvroValue.Float(float), AvroValue.Float(float)) &&
      expect.eql(AvroValue.Float(float).hashCode, AvroValue.Float(float).hashCode)
    }
  }

  pureTest("AvroValue Float should be equal to itself for NaN") {
    expect.same(AvroValue.Float(Float.NaN), AvroValue.Float(Float.NaN)) &&
    expect.eql(AvroValue.Float(Float.NaN).hashCode, AvroValue.Float(Float.NaN).hashCode)
  }

  pureTest("AvroValue Float should distinguish positive and negative zero") {
    val negativeZero = java.lang.Float.intBitsToFloat(Int.MinValue)
    expect(AvroValue.Float(0.0f) != AvroValue.Float(negativeZero))
  }

  test("AvroValue Double should be equal for equal values") {
    forall { (double: Double) =>
      expect.same(AvroValue.Double(double), AvroValue.Double(double)) &&
      expect.eql(AvroValue.Double(double).hashCode, AvroValue.Double(double).hashCode)
    }
  }

  pureTest("AvroValue Double should be equal to itself for NaN") {
    expect.same(AvroValue.Double(Double.NaN), AvroValue.Double(Double.NaN)) &&
    expect.eql(AvroValue.Double(Double.NaN).hashCode, AvroValue.Double(Double.NaN).hashCode)
  }

  pureTest("AvroValue Double should distinguish positive and negative zero") {
    val negativeZero = java.lang.Double.longBitsToDouble(Long.MinValue)
    expect(AvroValue.Double(0.0d) != AvroValue.Double(negativeZero))
  }

  pureTest("AvroValue Bytes should be equal for equal contents") {
    expect.same(AvroValue.Bytes(ByteVector(1, 2, 3)), AvroValue.Bytes(ByteVector(1, 2, 3))) &&
    expect(AvroValue.Bytes(ByteVector(1, 2, 3)) != AvroValue.Bytes(ByteVector(1, 2)))
  }

  pureTest("AvroValue show should have a Show instance consistent with toString") {
    val value: AvroValue = AvroValue.Record(Vector(AvroValue.Int(1), AvroValue.String("a")))
    expect.eql(value.toString, value.show)
  }
}
