/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import vulcan.BaseSuite

object AvroNameSpec extends BaseSuite {
  test("AvroName should provide name via name") {
    forall { (s: String) =>
      expect.eql(s, new AvroName(s).name)
    }
  }

  test("AvroName should include name in toString") {
    forall { (s: String) =>
      expect(clue(new AvroName(s).toString).contains(s))
    }
  }

  test("AvroName should provide an extractor for documentation") {
    forall { (s1: String) =>
      matches(new AvroName(s1)) { case AvroName(s2) =>
        expect.eql(s1, s2)
      }
    }
  }
}
