package org.waman

import scala.util.matching.Regex

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


  protected[multiverse] def getBigger[U <: ScaleUnit[U]](u:U, v: U): U = if(u.compare(v) >= 0) u else v

  // pattern like $u00B0
  private[multiverse] val escaped: Regex = """\$u([0-9A-F]{4})""".r

  // transform string like $u00B0C → °C
  private[multiverse] def decodeLiteralId(s: String): String = {
    def decode(s: String): String = Integer.parseInt(s, 16).asInstanceOf[Char].toString
    escaped.replaceAllIn(s, m => decode(m.group(1)))
  }
}
