/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import vulcan.BaseSuite

object AvroDocSpec extends BaseSuite {
  test("AvroDoc should provide documentation via doc") {
    forall { (s: String) =>
      expect.eql(s, new AvroDoc(s).doc)
    }
  }

  test("AvroDoc should include documentation in toString") {
    forall { (s: String) =>
      expect(clue(new AvroDoc(s).toString).contains(s))
    }
  }

  test("AvroDoc should provide an extractor for documentation") {
    forall { (s1: String) =>
      matches(new AvroDoc(s1)) { case AvroDoc(s2) =>
        expect.eql(s1, s2)
      }
    }
  }
}
