/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan.generic

import vulcan.BaseSuite

object AvroNamespaceSpec extends BaseSuite {
  test("AvroNamespace should provide the namespace via namespace") {
    forall { (s: String) =>
      expect.eql(s, new AvroNamespace(s).namespace)
    }
  }

  test("AvroNamespace should include namespace in toString") {
    forall { (s: String) =>
      expect(clue(new AvroNamespace(s).toString).contains(s))
    }
  }

  test("AvroNamespace should provide an extractor for namespace") {
    forall { (s1: String) =>
      matches(new AvroNamespace(s1)) { case AvroNamespace(s2) =>
        expect.eql(s1, s2)
      }
    }
  }
}
