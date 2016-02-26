package org.waman

package object multiverse {

  def help(): Unit = {
    println("***** Supported Quantities *****")
    printSupportedQuantities()
    println()
    println("For more info, execute the following command:")
    println("""  printSupportedUnits("<<Length etc.>>")""")
  }

  def printSupportedQuantities(): Unit =
    UnitSystem.supportedQuantities.map(_.getSimpleName).foreach(println)

  def printSupportedUnits(quantityName: String): Unit = {
    UnitSystem.supportedQuantities.find(_.getSimpleName == quantityName) match {
      case None => println(s"'$quantityName' is not supported")

      case Some(c) =>
        val unitObjectClass = Class.forName(c.getName + "Unit$")
        val unitObject = unitObjectClass.getField("MODULE$").get(null)
        unitObject match {
          case cd: ConstantsDefined[_] =>
            cd.values
              .map(_.asInstanceOf[PhysicalUnit[_]])
              .map(_.toDetailString)
              .foreach(println)

          case _ => println("No constant is defined: " + quantityName)
        }
    }
  }
}
