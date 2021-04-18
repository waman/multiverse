import java.io.File
import sbt.io.IO
import play.api.libs.json.{Json, Reads}

case class Constant(name: String, value: Option[String])

class ConstantsJson(jsonFile: File) extends JsonResource(jsonFile) {

  import GenerationUtil._

  implicit val constantReads: Reads[Constant] = Json.reads[Constant]
  val consts: Seq[Constant] = readJson(jsonFile, _.validate[Seq[Constant]])

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
            writer.write(s"""  val ${c.name}: Real = r"${c.value.get}"\n""")
        }
      }

      writer.write("}")
    }
}