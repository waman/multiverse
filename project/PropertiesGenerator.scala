import java.io.File

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class Properties(version: String)

object PropertiesGenerator {

  val PropertiesType: Class[_ >: Properties] = new TypeToken[Properties]() {}.getRawType

  import GenerationUtil._

  def generate(jsonRoot: File, destRoot: File, jsons: JsonResources): File = {
    val propJson = IO.resolve(jsonRoot, new File("Properties.json"))
    val properties: Properties = IO.reader(propJson, utf8) { reader =>
      gson.fromJson(reader, PropertiesType).asInstanceOf[Properties]
    }

    val destFile = IO.resolve(destRoot, new File("UnitdefsProperties.scala"))

    IO.writer(destFile, "", utf8, append = false) { writer =>
      writer.write(
        s"""package $rootPackage
           |
           |object UnitdefsProperties{
           |  val version: String = "${properties.version}"
           |
           |  def getUnitInfo: Seq[UnitInfo[_]] = Seq(
           |""".stripMargin)

      jsons.unitdefs.map{ ud =>
        if (ud.subpackage == "")
          s"    unit.defs.${ud.id}Unit,\n"
        else
          s"    unit.defs.${ud.subpackage}.${ud.id}Unit,\n"
      }.foreach(writer.write)
      writer.write(
        s"""
           |  )
           |}
           |""".stripMargin)
    }
    destFile
  }
}