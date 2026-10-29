/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import scodec.bits.ByteVector

final class AvroValueSpec extends BaseSpec {
  describe("AvroValue") {
    describe("Float") {
      it("should be equal for equal values") {
        forAll { (float: Float) =>
          assert(AvroValue.Float(float) == AvroValue.Float(float))
          assert(AvroValue.Float(float).hashCode == AvroValue.Float(float).hashCode)
        }
      }

      it("should be equal to itself for NaN") {
        assert(AvroValue.Float(Float.NaN) == AvroValue.Float(Float.NaN))
        assert(AvroValue.Float(Float.NaN).hashCode == AvroValue.Float(Float.NaN).hashCode)
      }

      it("should distinguish positive and negative zero") {
        val negativeZero = java.lang.Float.intBitsToFloat(Int.MinValue)
        assert(AvroValue.Float(0.0f) != AvroValue.Float(negativeZero))
      }
    }

    describe("Double") {
      it("should be equal for equal values") {
        forAll { (double: Double) =>
          assert(AvroValue.Double(double) == AvroValue.Double(double))
          assert(AvroValue.Double(double).hashCode == AvroValue.Double(double).hashCode)
        }
      }

      it("should be equal to itself for NaN") {
        assert(AvroValue.Double(Double.NaN) == AvroValue.Double(Double.NaN))
        assert(AvroValue.Double(Double.NaN).hashCode == AvroValue.Double(Double.NaN).hashCode)
      }

      it("should distinguish positive and negative zero") {
        val negativeZero = java.lang.Double.longBitsToDouble(Long.MinValue)
        assert(AvroValue.Double(0.0d) != AvroValue.Double(negativeZero))
      }
    }

    describe("Bytes") {
      it("should be equal for equal contents") {
        assert(AvroValue.Bytes(ByteVector(1, 2, 3)) == AvroValue.Bytes(ByteVector(1, 2, 3)))
        assert(AvroValue.Bytes(ByteVector(1, 2, 3)) != AvroValue.Bytes(ByteVector(1, 2)))
      }
    }
  }
}
