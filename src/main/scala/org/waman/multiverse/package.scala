package org.waman

package object multiverse {

  def printSupportedQuantities(): Unit = UnitSystem.getSupportedQuantities.foreach(println)

  def printSupportedUnits(quantityName: String): Unit =
    UnitSystem.getSupportedUnits[PhysicalUnit](quantityName)
      .map(u => s"${u.name.padTo(20, ' ')} (${u.symbol})").foreach(println)
}
