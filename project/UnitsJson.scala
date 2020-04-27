import java.io.{File, BufferedWriter => BW}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class Units(description: String, unitEntries: Array[UnitEntry])

case class UnitEntry(symbol: String, unit: String)

class UnitsJson(jsonFile: File, destDir: File)
  extends SourceGeneratorJson(jsonFile, destDir){

  import GenerationUtil._

  val id: String = jsonFile.getName.replace(".json", "")  // BasicUnits
  val destFilename: String =  id + ".scala"// BasicUnits.scala
  val packageName: String = GenerationUtil.rootPackage + ".unit"

  val unitsType: Class[_ >: Units] = new TypeToken[Units]() {}.getRawType

  val units: Units = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, unitsType).asInstanceOf[Units]
  }

  override protected def doGenerate(jsons: JsonResources): Unit = {
    IO.writer(this.destFile, "", utf8, append = false) { writer: BW =>

      writer.write(
        s"""package $packageName
           |
           |""".stripMargin)

      val subpackages = this.units.unitEntries.map(_.unit.split('.')(0)).distinct
      foreachUnitDefinition(subpackages, jsons){ ud =>
        writer.write(s"""import $rootPackage.unit.${ud.subpackage}._\n""")
      }

      writer.write("\n")

      if (this.units.description != null) {
        writer.write(
          s"""/**
             | * ${this.units.description}
             | */
             |""".stripMargin)

      }

      writer.write(
        s"""
           |object $id{
           |
           |""".stripMargin)

      this.units.unitEntries.foreach{ e =>
        val u = e.unit.split('.')
        val unitName = escapeSymbol(u(1))
        writer.write(s"""  def ${e.symbol}: ${u(0)}Unit = ${u(0)}UnitObjects.$unitName\n""")
      }

      writer.write("}")
    }
  }
}