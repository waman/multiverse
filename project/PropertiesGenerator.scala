import java.io.File

import sbt.io.IO

import play.api.libs.json._

case class Properties(version: String)

object PropertiesGenerator {

  import GenerationUtil._
  implicit val propertiesReads: Reads[Properties] = Json.reads[Properties]

  def generate(jsonRoot: File, destRoot: File, jsons: JsonResources): File = {
    val props = readJson(IO.resolve(jsonRoot, new File("Properties.json")), _.validate[Properties])
    val destFile = IO.resolve(destRoot, new File("UnitdefsProperties.scala"))

    IO.writer(destFile, "", utf8, append = false) { writer =>
      writer.write(
        s"""package $rootPackage
           |
           |object UnitdefsProperties{
           |  val version: String = "${props.version}"
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