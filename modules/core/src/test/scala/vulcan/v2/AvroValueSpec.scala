/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import scodec.bits.ByteVector
import vulcan.BaseSpec

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
        assert(AvroValue.Float(0.0f) != AvroValue.Float(-0.0f))
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
        assert(AvroValue.Double(0.0d) != AvroValue.Double(-0.0d))
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
