package org.waman.multiverse

import org.scalactic.TripleEqualsSupport.Spread
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.prop.TableDrivenPropertyChecks

class MultiverseCustomSpec  extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks{

  //***** Utility methods *****
  def convertImplicitly[T](t: T): T = t

  def %(expected: Double): Spread[Double] = error(expected, 2)
  def %%(expected: Double): Spread[Double] = error(expected, 4)
  def %%%(expected: Double): Spread[Double] = error(expected, 6)
  def %%%%(expected: Double): Spread[Double] = error(expected, 8)

  private def error(expected: Double, n: Int): Spread[Double] = {
    val err = expected match {
      case 0.0 => Math.pow(0.1, n)
      case _ => expected.abs * Math.pow(0.1, n)
    }
    expected +- err
  }

  def truncateLast(s: String): String = s.substring(0, s.length-1)

  class ContainTheSameElementAsMatcherWithDetailErrorMessage[E](expected: Set[E])
    extends Matcher[Iterable[E]]{

    override def apply(left: Iterable[E]): MatchResult = {
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

  def containTheSameElementsAs[E](elem: Iterable[E]) =
    new ContainTheSameElementAsMatcherWithDetailErrorMessage[E](elem.toSet)
}
