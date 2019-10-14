import java.io.File
import sbt.io.IO

object ImplicitsGenerator {

  def generate(srcManaged: File, jsons: Seq[UnitDefinitionJson]): File = {
    val destDir = IO.resolve(srcManaged, new File("org/waman/multiverse/implicits"))
    if (!destDir.exists()) IO.createDirectory(destDir)

    val destFile = IO.resolve(destDir, new File("package.scala"))
    IO.writer(destFile, "", GenerationUtil.utf8, append=false){ writer =>
      writer.write(
        s"""package org.waman.multiverse
           |
           |import scala.language.implicitConversions
           |import spire.math._
           |
           |""".stripMargin)

      jsons.map(_.subpackage).distinct.foreach{ sp =>
        writer.write(s"""import org.waman.multiverse.unit.$sp._\n""")
      }

      writer.write(
        s"""
           |package object implicits {
           |
           |  implicit class QuantityFactory[A: Fractional](val value: A){
           |
           |""".stripMargin)

      jsons.map(_.id).foreach{ id =>
        writer.write(s"""    def apply(unit: ${id}Unit): ${id}[A] = new ${id}(value, unit)\n""")
      }

      writer.write(
        s"""}
           |
           |  // Integral value (like 1(m), not 1.0(m)) create a Quantity[Real] instance
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
           |}
           |""".stripMargin)
    }

    println("[GENERATE] " + destFile)
    destFile
  }
}
