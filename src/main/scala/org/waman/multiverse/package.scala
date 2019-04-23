package org.waman

import spire.math._
import spire.implicits._

import scala.annotation.tailrec

package object multiverse {

//  def help(): Unit = {
//    println("***** Supported Quantities *****")
//    printSupportedQuantities()
//    println()
//    println("For more info, execute the following command:")
//    println("""  printSupportedUnits("<<Length etc.>>")""")
//  }
//
//  def printSupportedQuantities(): Unit =
//    UnitSystem.supportedQuantities.map(_.getSimpleName).foreach(println)
//
//  def printSupportedUnits(quantityName: String): Unit = {
//    UnitSystem.supportedQuantities.find(_.getSimpleName == quantityName) match {
//      case None => println(s"'$quantityName' is not supported")
//
//      case Some(c) =>
//        val unitObjectClass = Class.forName(c.getName + "Unit$")
//        val unitObject = unitObjectClass.getField("MODULE$").get(null)
//        unitObject match {
//          case cd: ConstantsDefined[_] =>
//            cd.values
//              .map(_.asInstanceOf[PhysicalUnit[_]])
//              .map(_.toDetailString)
//              .foreach(println)
//
//          case _ => println("No constant is defined: " + quantityName)
//        }
//    }
//  }

  def toReadableString(r: Real): String =
    if(r.isWhole) {
      String.format("%,d", r.toRational.toBigInt.bigInteger)
    }else{
      val ra = r.toRational
      if(ra == r) toReadableString(ra)
      else r.toString
    }

  private def toReadableString(r: Rational): String = {
    val (num, deno) = (r.numerator, r.denominator)
    val (x, n10) = divideRepeatedly(deno, 10L)
    if(x == 1L){
      formatToDecimal(num, n10)
    }else{
      val g = num gcd deno
      val d = deno / g
      val (y, n2) = divideRepeatedly(d, 2L)
      val (z, n5) = divideRepeatedly(y, 5L)
      if(z != 1L) return r.toString  // cannot represent as an exact decimal

      if(n2 > n5){
        formatToDecimal(num / g * (5L**(n2-n5)), n2)
      }else{
        formatToDecimal(num / g * (2L**(n5-n2)), n5)
      }
    }
  }

  private def divideRepeatedly(n: SafeLong, p: SafeLong): (SafeLong, Int) = {
    @tailrec
    def f(n: SafeLong, count: Int): (SafeLong, Int) = n /% p match {
      case (a, b) if b == 0 => f(a, count+1)
      case _ => (n, count)
    }

    f(n, 0)
  }

  private def formatToDecimal(num: SafeLong, n: Int): String = num.toString match {
    case s if s.length > n =>
      val sep = s.length - n
      val (s0, s1) = (s.substring(0, sep), s.substring(sep))
      s0+"."+s1
    case s =>
      val zeros = "0"*(n-s.length)
      "0."+zeros + s
  }
}
