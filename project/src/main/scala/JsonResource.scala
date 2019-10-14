import java.io.File
import java.nio.charset.Charset

import com.google.gson.Gson
import sbt.io.IO

class JsonResourceFactory(info: File, srcManaged: File, src: File, destPath: File){
  // info: src/main/resources/physical-units
  // srcManaged: src/main/src_managed
  // src: src/main/scala

  def apply(json: File): JsonResource = {
    val subpackage = IO.relativizeFile(info, json.getParentFile).get // basic
    val destDir = IO.resolve(IO.resolve(srcManaged, destPath), subpackage) // src/main/src_managed/org/waman/multiverse/unit/basic
    val mainDir = IO.resolve(IO.resolve(src, destPath), subpackage) // src/main/scala/org/waman/multiverse/unit/basic

    json.getName match {
      case "Constants.json" => new ConstantsJson(json, destDir, mainDir)
      case "ScalePrefixes.json" => new ScalePrefixJson(json)
      case "TemperatureUnits.json" => new HomogeneousUnitDefinitionJson(json, destDir, mainDir, subpackage.toString)
      case _ => new LinearUnitDefinitionJson(json, destDir, mainDir, subpackage.toString)
    }
  }
}

abstract class JsonResource(val jsonFile: File){

  protected final def gson: Gson = GenerationUtil.gson
  protected final def UTF8: Charset = GenerationUtil.utf8

  def isGenerating: Boolean
  def isUnitDefinitionJson: Boolean
}

abstract class SourceGeneratorJson(jsonFile: File, destDir: File, mainDir: File)
    extends JsonResource (jsonFile){

  def destFilename: String

  lazy val destFile: File = IO.resolve(destDir, new File(destFilename))
  lazy val isGenerating: Boolean = !IO.resolve(mainDir, new File(destFilename)).exists()
  def packageName: String

  def generate(jsons: Seq[JsonResource]): Seq[File] = {
    if (this.isGenerating){
      doGenerate(jsons)
      println("[GENERATE] " + this.destFilename)
    }

    if(this.isGenerating) Seq(this.destFile)
    else Nil
  }

  protected def doGenerate(jsons: Seq[JsonResource]): Unit
}

abstract class UnitDefinitionJson(jsonFile: File, destDir: File, mainDir: File, val subpackage: String)
    extends SourceGeneratorJson(jsonFile, destDir, mainDir){

  val id: String = jsonFile.getName.replace("Units.json", "")  // Length
  val destFilename: String =  id + ".scala"// Length.scala
  val packageName: String = GenerationUtil.rootPackage + ".unit." + subpackage

  override def isUnitDefinitionJson: Boolean = true

  case class Composites(products: Seq[(String, String)], quotients: Seq[(String, String)])

  def composites: Composites
}