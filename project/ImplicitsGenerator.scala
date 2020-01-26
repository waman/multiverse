import java.io.File

import sbt.io.IO

object ImplicitsGenerator {

  import GenerationUtil._

  def generate(jsons: JsonResources, srcManaged: File): File = {
    val destDir = IO.resolve(srcManaged, new File("org/waman/multiverse/implicits"))
    if (!destDir.exists()) IO.createDirectory(destDir)

    val destFile = IO.resolve(destDir, new File("package.scala"))
    IO.writer(destFile, "", utf8, append=false){ writer =>
      writer.write(
        s"""package $rootPackage
           |
           |import scala.language.implicitConversions
           |import spire.math._
           |
           |""".stripMargin)

      jsons.unitDefs.map(_.subpackage).distinct.foreach{ sp =>
        writer.write(s"""import $rootPackage.unit.$sp._\n""")
      }

      writer.write(
        s"""
           |package object implicits {
           |
           |  implicit class QuantityFactory[A: Fractional](val value: A){
           |
           |""".stripMargin)

      jsons.unitDefs.map(_.id).foreach{ id =>
        writer.write(s"""    def apply(unit: ${id}Unit): $id[A] = new $id(value, unit)\n""")
      }

      writer.write(
        s"""}
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
           |""".stripMargin)

      writer.write("  // Implicit conversions between unrelated units (like energy and absolute temperature)\n")
      jsons.extractResources(classOf[LinearUnitDefinitionJson]).filter(_.unitCategory._convertibles.nonEmpty).foreach{ ud =>
        ud.unitCategory._convertibles.foreach{ conv =>
          writer.write(
            s"""  implicit def convert${ud.id}To${conv.target}[A: Fractional](q: ${ud.id}[A]): ${conv.target}[A] =
               |    q.to${conv.target}
               |
               |""".stripMargin)
        }
      }

      writer.write("}\n")
    }

    println("[GENERATE] " + destFile)
    destFile
  }
}
