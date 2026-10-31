/*
 * Copyright 2019 OVO Energy Limited
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package vulcan

import vulcan.examples.*
import weaver.Expectations
import weaver.SourceLocation

object PrismSpec extends BaseSuite {

  /** Expects `reverseGet` to return `s` from what `getOption` returns, if anything. */
  private def expectPrismPartialRoundtrip[S, A](s: S, prism: Prism[S, A])(implicit
    loc: SourceLocation
  ): Expectations =
    expect.same(s, prism.getOption(s).fold(s)(prism.reverseGet))

  /** Expects `getOption` to return `a` from what `reverseGet` returns. */
  private def expectPrismRoundtrip[S, A](a: A, prism: Prism[S, A])(implicit
    loc: SourceLocation
  ): Expectations =
    expect.same(Some(a), prism.getOption(prism.reverseGet(a)))

  test("Prism derive partialRoundtrip") {
    forall { (s: SealedTraitCaseClass) =>
      expectPrismPartialRoundtrip(s, Prism[SealedTraitCaseClass, FirstInSealedTraitCaseClass]) &&
      expectPrismPartialRoundtrip(s, Prism[SealedTraitCaseClass, SecondInSealedTraitCaseClass]) &&
      expectPrismPartialRoundtrip(s, Prism[SealedTraitCaseClass, ThirdInSealedTraitCaseClass])
    }
  }

  test("Prism derive roundtrip") {
    forall {
      (
        first: FirstInSealedTraitCaseClass,
        second: SecondInSealedTraitCaseClass,
        third: ThirdInSealedTraitCaseClass
      ) =>
        expectPrismRoundtrip(first, Prism[SealedTraitCaseClass, FirstInSealedTraitCaseClass]) &&
        expectPrismRoundtrip(second, Prism[SealedTraitCaseClass, SecondInSealedTraitCaseClass]) &&
        expectPrismRoundtrip(third, Prism[SealedTraitCaseClass, ThirdInSealedTraitCaseClass])
    }
  }

  test("Prism identity roundtrip") {
    forall { (n: Int) =>
      expectPrismRoundtrip(n, Prism.identity[Int])
    }
  }

  pureTest("Prism instance toString") {
    expect(clue(Prism[SealedTraitCaseClass, FirstInSealedTraitCaseClass].toString()).startsWith("Prism$"))
  }

  pureTest("Prism none none") {
    expect.same(Some(None), Prism[Option[Int], None.type].getOption(None))
  }

  test("Prism none some") {
    forall { (n: Int) =>
      expect.same(None, Prism[Option[Int], None.type].getOption(Some(n)))
    }
  }

  test("Prism partial getOption defined when get is defined") {
    val prism =
      Prism.partial[SealedTraitCaseClass, FirstInSealedTraitCaseClass] {
        case first @ FirstInSealedTraitCaseClass(_) => first
      }(identity)

    forall {
      (
        first: FirstInSealedTraitCaseClass,
        second: SecondInSealedTraitCaseClass,
        third: ThirdInSealedTraitCaseClass
      ) =>
        expect.same(Some(first), prism.getOption(first)) &&
        expect.same(None, prism.getOption(second)) &&
        expect.same(None, prism.getOption(third))
    }
  }

  pureTest("Prism some none") {
    expect.same(None, Prism[Option[Int], Some[Int]].getOption(None))
  }

  test("Prism some some") {
    forall { (n: Int) =>
      expect.same(Some(Some(n)), Prism[Option[Int], Some[Int]].getOption(Some(n)))
    }
  }

  test("Prism some some.coproduct") {
    sealed trait FirstOrSecond
    final case class First(a: Int) extends FirstOrSecond
    final case class Second(b: Double) extends FirstOrSecond

    val prism = Prism[Option[FirstOrSecond], Some[First]]

    forall { (n: Int, d: Double) =>
      expect.same(None, prism.getOption(None)) &&
      expect.same(None, prism.getOption(Some(Second(d)))) &&
      expect.same(Some(Some(First(n))), prism.getOption(Some(First(n))))
    }
  }
}
