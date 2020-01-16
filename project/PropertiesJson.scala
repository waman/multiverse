import java.io.File

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class Properties(version: String)

class PropertiesJson(json: File, destDir: File) extends SourceGeneratorJson(json, destDir) {

  import GenerationUtil._

  val PropertiesType: Class[_ >: Properties] = new TypeToken[Properties]() {}.getRawType

  val destFilename: String = "Properties.scala"
  val packageName: String = GenerationUtil.rootPackage + ".unit"

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
           |}
           |""".stripMargin)
    }
}