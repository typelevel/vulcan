/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.v2

import org.apache.avro.{Schema => JavaSchema}
import vulcan.BaseSpec

/** Checks schemas against the Java Avro library. */
final class SchemaJavaSpec extends BaseSpec {
  describe("Schema") {
    describe("toString") {
      it("should print the same as the Java Avro library") {
        forAll(SchemaGenerators.genSchema, minSuccessful(1000)) { schema =>
          val printed = schema.toString
          val parsed = new JavaSchema.Parser().parse(printed)
          assert(parsed.toString == printed)
        }
      }
    }
  }
}
