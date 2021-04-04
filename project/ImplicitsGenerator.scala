import java.io.File

import sbt.io.IO

object ImplicitsGenerator {

  import GenerationUtil._

  def generate(destRoot: File, jsons: JsonResources): File = {
    val destDir = IO.resolve(destRoot, new File("implicits"))
    if (!destDir.exists()) IO.createDirectory(destDir)

    val destFile = IO.resolve(destDir, new File("package.scala"))
    IO.writer(destFile, "", utf8, append=false){ writer =>
      writer.write(
        s"""package $rootPackage
           |
           |import scala.language.implicitConversions
           |import spire.math._
           |
           |import $rootPackage.unit.defs._
           |""".stripMargin)

      jsons.unitdefs.map(_.subpackage).distinct.filter(_ != "").foreach{ sp =>
        writer.write(s"""import $rootPackage.unit.defs.$sp._\n""")
      }

      writer.write(
        s"""
           |import $rootPackage.typeless._
           |
           |package object implicits {
           |
           |  implicit class QuantityFactory[A: Fractional](val value: A){
           |
           |""".stripMargin)

      jsons.unitdefs.map(_.id).foreach{ id =>
        writer.write(s"""    def apply(unit: ${id}Unit): $id[A] = new $id(value, unit)\n""")
      }

      writer.write(
        s"""
           |    def apply(unit: TypelessLinearUnit): TypelessLinearQuantity[A] = new TypelessLinearQuantity(value, unit)
           |  }
           |
           |  // An integral value (like 1(m), not 1.0(m)) create a Quantity[Real] instance
           |  implicit def convertIntToQuantityFactory(value: Int): QuantityFactory[Real] =
           |    new QuantityFactory(Real(value))
           |
           |  implicit def convertLongToQuantityFactory(value: Long): QuantityFactory[Real] =
           |    new QuantityFactory(Real(value))
           |
           |  implicit def convertSafeLongToQuantityFactory(value: SafeLong): QuantityFactory[Real] =
           |    new QuantityFactory(Real(value))
           |
           |  implicit def convertBigIntToQuantityFactory(value: BigInt): QuantityFactory[Real] =
           |    new QuantityFactory(Real(value))
           |
           |  // Implicit conversions between Temperature and AbsoluteTemperature)
           |  implicit def convertAbsoluteTemperatureToTemperature[A: Fractional](q: AbsoluteTemperature[A]): Temperature[A] =
           |    q.toTemperature
           |
           |  implicit def convertTemperatureToAbsoluteTemperature[A: Fractional](q: Temperature[A]): AbsoluteTemperature[A] =
           |    q.toAbsoluteTemperature
           |
           |""".stripMargin)

      writer.write("}\n")
    }

    println("[GENERATE] " + destFile)
    destFile
  }
}
