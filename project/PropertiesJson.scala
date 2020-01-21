import java.io.File

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class Properties(version: String)

class PropertiesJson(json: File, destDir: File) extends SourceGeneratorJson(json, destDir) {

  import GenerationUtil._

  val PropertiesType: Class[_ >: Properties] = new TypeToken[Properties]() {}.getRawType

  val destFilename: String = "UnitdefsProperties.scala"
  val packageName: String = GenerationUtil.rootPackage

  val properties: Properties = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, PropertiesType).asInstanceOf[Properties]
  }

  override protected def doGenerate(jsons: JsonResources): Unit =
    IO.writer(destFile, "", utf8, append = false) { writer =>
      writer.write(
        s"""package $packageName
           |
           |object UnitdefsProperties{
           |  val version: String = "${properties.version}"
           |
           |  def getUnitInfo: Seq[UnitInfo[_]] = Seq(
           |""".stripMargin)

      writer.write(jsons.unitDefs.map(ud => s"    unit.${ud.subpackage}.${ud.id}Unit").mkString(",\n"))
      writer.write(
        s"""
           |  )
           |}
           |""".stripMargin)
    }
}