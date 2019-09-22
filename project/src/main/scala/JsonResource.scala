import java.io.File
import java.nio.charset.Charset

import com.google.gson.Gson
import sbt.io.IO

abstract class JsonResource(val jsonFile: File){

  protected final def gson: Gson = GenerationUtil.gson
  protected final def UTF8: Charset = GenerationUtil.utf8

  def isGenerating: Boolean
  def isLinearUnit: Boolean
}

abstract class GeneratingJsonResource(jsonFile: File, destDir: File, mainDir: File)
    extends JsonResource (jsonFile){

  def destFilename: String

  lazy val destFile: File = IO.resolve(destDir, new File(destFilename))
  lazy val isGenerating: Boolean = !IO.resolve(mainDir, new File(destFilename)).exists()
  def packageName: String

  def generate(jsons: Seq[JsonResource]): Seq[File] = {
    if (isGenerating){
      doGenerate(jsons)
      println("[GENERATE] " + destFile)
    }

    if(this.isGenerating) Seq(destFile)
    else Nil
  }

  protected def doGenerate(jsons: Seq[JsonResource]): Unit
}

class JsonResourceFactory(info: File, srcManaged: File, src: File, destPath: File){
  // info: src/main/resources/physical-units
  // srcManaged: src/main/src_managed
  // src: src/main/scala

  def apply(json: File): JsonResource = {
    val subpackage = IO.relativizeFile(info, json.getParentFile).get // basic
    val destDir = IO.resolve(IO.resolve(srcManaged, destPath), subpackage) // src/main/src_managed/org/waman/multiverse/unit/basic
    val mainDir = IO.resolve(IO.resolve(src, destPath), subpackage) // src/main/scala/org/waman/multiverse/unit/basic

    json.getName match {
      case "Constants.json" => new ConstantsResource(json, destDir, mainDir)
      case "ScalePrefixes.json" => new ScalePrefixResource(json)
      case _ => new LinearUnitResource(json, destDir, mainDir, subpackage.toString)
    }
  }
}