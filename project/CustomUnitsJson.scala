import java.io.{File, BufferedWriter => BW}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class Units(description: String, units: Array[UnitEntry], use: Use){
  def _units: Seq[UnitEntry] = GenerationUtil.toSeq(this.units)
}

case class UnitEntry(symbol: String, unit: String)

class CustomUnitsJson(jsonFile: File) extends JsonResource(jsonFile){

  import GenerationUtil._

  val id: String = jsonFile.getName.replace(".json", "")  // BasicUnits

  val unitsType: Class[_ >: Units] = new TypeToken[Units]() {}.getRawType

  val units: Units = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, unitsType).asInstanceOf[Units]
  }

  override protected def getDestFile(destRoot: File): File =
    IO.resolve(destRoot, new File(s"unit/custom/$id.scala"))

  override protected def doGenerate(destFile: File): Unit = {
    IO.writer(destFile, "", utf8, append = false) { writer: BW =>

      writer.write(s"""package $rootPackage.unit.custom\n\n""")

      if(this.units.use != null){
        this.units.use._subpackages.foreach{ u =>
          if (u == "")
            writer.write(s"""import $rootPackage.unit.defs._\n""")
          else
            writer.write(s"""import $rootPackage.unit.defs.$u._\n""")
        }
      }

      if (this.units.description != null) {
        writer.write(
          s"""
             |/**
             | * ${this.units.description}
             | */""".stripMargin)

      }

      writer.write(
        s"""
           |object $id{
           |""".stripMargin)

      this.units._units.foreach{ entry =>
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