import java.io.{File, BufferedWriter => BW}
import play.api.libs.json.{Json, Reads}
import sbt.io.IO

case class UnitSystem(description: Option[String], parent: Option[String], evaluations: Seq[Evaluation], use: Option[Use])
case class Evaluation(quantity: String, unit: String)

class UnitSystemJson(jsonFile: File) extends JsonResource(jsonFile){

  import GenerationUtil._

  val id: String = jsonFile.getName.replace(".json", "")  // MKS

  implicit val evaluationReads: Reads[Evaluation] = Json.reads[Evaluation]
  implicit val useReads: Reads[Use] = Json.reads[Use]
  implicit val unitsystemReads: Reads[UnitSystem] = Json.reads[UnitSystem]
  val unitsystem: UnitSystem = readJson(jsonFile, _.validate[UnitSystem])

  override protected def getDestFile(destRoot: File): File =
    IO.resolve(destRoot, new File(s"unitsystem/$id.scala"))

  override protected def doGenerate(destFile: File): Unit = {
    IO.writer(destFile, "", utf8, append = false) { writer: BW =>
      writer.write(
        s"""package $rootPackage.unitsystem
           |
           |import scala.language.implicitConversions
           |
           |""".stripMargin)

      this.unitsystem.use.foreach{ use =>
        use.subpackages.foreach{ sps =>
          sps.foreach{ sp =>
            if (sp == "") writer.write(s"""import $rootPackage.unit.defs._\n""")
            else              writer.write(s"""import $rootPackage.unit.defs.$sp._\n""")
            }
          }
          if (use.subpackages.nonEmpty) writer.write("\n")
      }

      this.unitsystem.description.foreach{ desc =>
        writer.write(s"""/** $desc */""")
      }

      val parent = this.unitsystem.parent match {
        case Some(_parent) => _parent
        case  _ => "UnitSystem"
      }
      writer.write(
        s"""
           |trait $id extends $parent {
           |""".stripMargin)

      this.unitsystem.evaluations.foreach{ e =>
        val refinedUnit = regexUnitName.replaceAllIn(e.unit, m => {
          val uType = if (m.group(1) != null) m.group(1) else e.quantity
          val uName = m.group(2)
          s"""${uType}UnitObjects.$uName"""
        })

        writer.write(s"""  implicit def evaluate${e.quantity}[A: Fractional](q: ${e.quantity}[A]): A = q($refinedUnit)\n""")
      }

      writer.write(
        s"""}
           |
           |object $id extends $id
           |""".stripMargin)
    }
  }
}