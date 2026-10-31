/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import vulcan.BaseSuite

object AvroAliasSpec extends BaseSuite {
  test("AvroAlias should provide alias via alias") {
    forall { (s: String) =>
      expect.eql(s, new AvroAlias(s).alias)
    }
  }

  test("AvroAlias should include alias in toString") {
    forall { (s: String) =>
      expect(clue(new AvroAlias(s).toString).contains(s))
    }
  }

  test("AvroAlias should provide an extractor for alias") {
    forall { (s1: String) =>
      matches(new AvroAlias(s1)) { case AvroAlias(s2) =>
        expect.eql(s1, s2)
      }
    }
  }
}
