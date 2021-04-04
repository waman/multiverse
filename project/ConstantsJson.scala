import java.io.File

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class Constant(name: String, value: String)

class ConstantsJson(json: File) extends JsonResource(json) {

  import GenerationUtil._

  val constantsType: Class[_ >: Array[Constant]] = new TypeToken[Array[Constant]]() {}.getRawType

  val consts: Array[Constant] = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, constantsType).asInstanceOf[Array[Constant]]
  }

  override protected def getDestFile(destRoot: File): File =
    IO.resolve(destRoot, new File("Constants.scala"))

  override protected def doGenerate(destFile: File): Unit =
    IO.writer(destFile, "", utf8, append = false) { writer =>
      writer.write(
        s"""package $rootPackage
           |
           |import spire.math.Real
           |import spire.implicits._
           |
           |object Constants{
           |""".stripMargin)

      consts.foreach { c =>
        c.name match {
          case "Pi" =>
            writer.write(s"""  val Pi: Real = Real.pi\n""")
          case "ReducedPlanckConstant" =>
            writer.write(s"""  val ${c.name}: Real = PlanckConstant / (Real.two * Real.pi)\n""")
          case _ =>
            writer.write(s"""  val ${c.name}: Real = r"${c.value}"\n""")
        }
      }

      writer.write("}")
    }
}