import java.io.{File, BufferedWriter => BW}
import com.google.gson.reflect.TypeToken
import sbt.io.IO

import scala.util.matching.Regex

case class UnitSystem(description: String, parent: String, evaluations: Array[EvalEntry], use: Use){
  def _parent: String = if (this.parent != null) this.parent else "UnitSystem"
  lazy val _evaluations: Seq[EvalEntry] = GenerationUtil.toSeq(this.evaluations)
}

case class EvalEntry(quantity: String, unit: String)

class UnitSystemJson(jsonFile: File) extends JsonResource(jsonFile){

  import GenerationUtil._

  val id: String = jsonFile.getName.replace(".json", "")  // MKS

  val unitsystemType: Class[_ >: UnitSystem] = new TypeToken[UnitSystem]() {}.getRawType

  val unitsystem: UnitSystem = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, unitsystemType).asInstanceOf[UnitSystem]
  }

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

      if (this.unitsystem.use != null) {
        this.unitsystem.use._subpackages.foreach{ sp =>
          if (sp == "")
            writer.write(s"""import $rootPackage.unit.defs._\n""")
          else
            writer.write(s"""import $rootPackage.unit.defs.$sp._\n""")
        }
        if (this.unitsystem.use._subpackages.nonEmpty) writer.write("\n")
      }

      if (this.unitsystem.description != null) {
        writer.write(
          s"""/**
             | * ${this.unitsystem.description}
             | */""".stripMargin)
      }

      writer.write(
        s"""
           |trait $id extends ${this.unitsystem._parent}{
           |""".stripMargin)

      this.unitsystem._evaluations.foreach{ e =>
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