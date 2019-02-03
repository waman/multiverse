package org.waman.multiverse

import org.scalactic.TripleEqualsSupport.Spread
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.PropertyChecks

class MultiverseCustomSpec  extends FreeSpec with Matchers with PropertyChecks{

  //***** Utility methods *****
  def convertImplicitly[T](t: T): T = t

  def %(expected: Double): Spread[Double] = %(expected, 2)
  def %%(expected: Double): Spread[Double] = %(expected, 4)
  def %%%(expected: Double): Spread[Double] = %(expected, 6)
  def %%%%(expected: Double): Spread[Double] = %(expected, 8)
  def %(expected: Double, n: Int): Spread[Double] = expected +- error(expected, n)

  private def error(expected: Double, n: Int): Double = expected match {
    case 0.0 => Math.pow(0.1, n)
    case _ => expected.abs * Math.pow(0.1, n)
  }

  def truncateLast(s: String): String = s.substring(0, s.length-1)

  class ContainTheSameElementAsMatcherWithDetailErrorMessage[E](expected: Set[E])
    extends Matcher[Traversable[E]]{

    override def apply(left: Traversable[E]): MatchResult = {
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
