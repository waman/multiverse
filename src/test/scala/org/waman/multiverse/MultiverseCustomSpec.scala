package org.waman.multiverse

import org.scalatest.matchers.{MatchResult, Matcher}
import org.waman.scalatest_util.WamanCustomSpec

class MultiverseCustomSpec extends WamanCustomSpec{

  def truncateLast(s: String): String = s.substring(0, s.length-1)

  class ContainTheSameElementAsMatcherWithDetailErrorMessage[E](expected: Set[E])
    extends Matcher[Traversable[E]]{

    override def apply(left: Traversable[E]) = {
      val sut = left.toSet
      val result = sut == expected
      val unexpectedElements = sut -- expected
      val requiredElements   = expected -- sut
      MatchResult(
        result,
        s"""SUT set does not have the same elements as the expected set
           |  unexpected elements: ${unexpectedElements.mkString(", ")}
           |  required elements : ${requiredElements.mkString(", ")}
         """.stripMargin,
        "SUT set has the same elements as the expected set"
      )
    }
  }

  def containTheSameElementsAs[E](elem: Traversable[E]) =
    new ContainTheSameElementAsMatcherWithDetailErrorMessage[E](elem.toSet)
}
