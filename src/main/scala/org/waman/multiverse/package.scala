package org.waman

package object multiverse {

  def help(): Unit = {
    println("***** Supported Quantities *****")
    printSupportedQuantities()
    println()
    println("For more info, execute the following command:")
    println("""  printSupportedUnits("<<Length etc.>>")""")
  }

  def printSupportedQuantities(): Unit = UnitSystem.getSupportedQuantities.foreach(println)

  def printSupportedUnits(quantityName: String): Unit =
    UnitSystem.getSupportedUnits[PhysicalUnit](quantityName).foreach(println)
}
