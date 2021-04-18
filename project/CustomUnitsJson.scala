import java.io.{File, BufferedWriter => BW}
import play.api.libs.json.{Json, Reads}
import sbt.io.IO

case class CustomUnits(description: Option[String], units: Seq[UnitEntry], use: Option[Use])
case class UnitEntry(symbol: String, unit: String)

class CustomUnitsJson(jsonFile: File) extends JsonResource(jsonFile){

  import GenerationUtil._

  val id: String = jsonFile.getName.replace(".json", "")  // BasicUnits

  implicit val unitEntryReads: Reads[UnitEntry] = Json.reads[UnitEntry]
  implicit val useReads: Reads[Use] = Json.reads[Use]
  implicit val customUnitReads: Reads[CustomUnits] = Json.reads[CustomUnits]
  val customUnits: CustomUnits = readJson(jsonFile, _.validate[CustomUnits])

  override protected def getDestFile(destRoot: File): File =
    IO.resolve(destRoot, new File(s"unit/custom/$id.scala"))

  override protected def doGenerate(destFile: File): Unit = {
    IO.writer(destFile, "", utf8, append = false) { writer: BW =>

      writer.write(s"""package $rootPackage.unit.custom\n\n""")

      this.customUnits.use.foreach{ use =>
        use.subpackages.foreach{ sps =>
          sps.foreach{ u =>
            if (u == "") writer.write(s"""import $rootPackage.unit.defs._\n""")
            else             writer.write(s"""import $rootPackage.unit.defs.$u._\n""")
          }
        }
      }

      this.customUnits.description.foreach{ desc =>
          writer.write(s"/** $desc */")
      }

      writer.write(
        s"""
           |object $id{
           |""".stripMargin)

      this.customUnits.units.foreach{ entry =>
        val u = entry.unit.split('.')
        val uType = u(0)
        val uName = u(1)
        writer.write(
          s"""  /** $uName */
             |  def ${entry.symbol}: ${uType}Unit = ${uType}UnitObjects.${escape(uName)}
             |""".stripMargin)
      }

      writer.write("}")
    }
  }
}