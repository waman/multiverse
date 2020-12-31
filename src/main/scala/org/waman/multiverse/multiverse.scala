package org.waman

import spire.math.Real.Exact
import spire.math.{Rational, Real, SafeLong}
import spire.implicits._

import scala.annotation.tailrec

package object multiverse {

  def printUnitdefsVersion(): Unit = println(UnitdefsProperties.version)

  def help(): Unit = {
    println("Predefined Units:")
    UnitdefsProperties.getUnitInfo.map(_.getClass.getName.replace("$", "")).foreach(println)
  }

  def toReadableString(r: Real): String = {

    def toReadableString(r: Rational): String = {
      val (num, deno) = (r.numerator, r.denominator)
      val (x, n10) = divideRepeatedly(deno, 10L)
      if (x == 1L) {
        formatToDecimal(num, n10)
      } else {
        val (y, n2) = divideRepeatedly(deno, 2L)
        val (z, n5) = divideRepeatedly(y, 5L)
        if (z != 1L) return r.toString // cannot be represented as an exact decimal

        if (n2 > n5) {
          formatToDecimal(num * (5L ** (n2 - n5)), n2)
        } else {
          formatToDecimal(num * (2L ** (n5 - n2)), n5)
        }
      }
    }

    def divideRepeatedly(n: SafeLong, p: SafeLong): (SafeLong, Int) = {
      @tailrec
      def f(n: SafeLong, count: Int): (SafeLong, Int) = n /% p match {
        case (a, b) if b == 0 => f(a, count + 1)
        case _ => (n, count)
      }

      f(n, 0)
    }

    def formatToDecimal(num: SafeLong, n: Int): String = num.toString match {
      case s if s.length > n =>
        val sep = s.length - n
        s.substring(0, sep) + "." + s.substring(sep)
      case s =>
        val zeros = "0" * (n - s.length)
        "0." + zeros + s
    }

    r match {
      case _ if r.isWhole() => String.format("%,d", r.toRational.toBigInt.bigInteger)
      case Exact(n) => toReadableString(n)
      case _ => r.getString(10) + "..."
    }
  }

  def toSuperscripts(n: Int): String = {

    val superscripts = "⁰¹²³⁴⁵⁶⁷⁸⁹⁻"

    def toSuper(c: Char): Char = c match {
      case '-' => superscripts.charAt(10)
      case n => superscripts.charAt(n - '0')
    }

    n.toString.map(toSuper).mkString("")
  }
}
