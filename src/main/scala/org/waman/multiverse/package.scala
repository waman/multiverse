package org.waman

package object multiverse {

  def printSupportedUnits[U <: PhysicalUnit](unitName: String): Unit =
    printSupportedUnits(Class.forName(s"org.waman.multiverse.${unitName}Unit").asInstanceOf[Class[U]])

  def printSupportedUnits[U <: PhysicalUnit](unitType: Class[U]): Unit =
    UnitSystem.getSupportedUnits(unitType).map(_.toString).foreach(println)
}
