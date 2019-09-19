import java.io.File
import java.nio.charset.Charset

import com.google.gson.Gson
import sbt.io.IO

import scala.util.matching.Regex

abstract class JsonResource(protected val jsonFile: File, destDir: File, mainDir: File){

  protected final def gson: Gson = GenerationUtil.gson
  protected final def UTF8: Charset = GenerationUtil.utf8

  def destFilename: String

  lazy val destFile: File = IO.resolve(destDir, new File(destFilename))
  lazy val isGenerating: Boolean = !IO.resolve(mainDir, new File(destFilename)).exists()
  def packageName: String

  def isLinearUnit: Boolean
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
    val kind = IO.relativizeFile(info, json.getParentFile).get // basic
    val destDir = IO.resolve(IO.resolve(srcManaged, destPath), kind) // src/main/src_managed/org/waman/multiverse/unit/basic
    val mainDir = IO.resolve(IO.resolve(src, destPath), kind) // src/main/scala/org/waman/multiverse/unit/basic

    if (json.getName == "Constants.json")
      new ConstantsResource(json, destDir, mainDir)
    else
      new LinearUnitResource(json, destDir, mainDir, kind.toString)
  }
}

case class ScalePrefix(name: String, prefix: String, aliases: Seq[String], scale: String)

object GenerationUtil{

  val rootPackage: String = "org.waman.multiverse"
  val gson: Gson = new Gson
  val utf8: Charset = Charset.forName("UTF-8")

  val regId: Regex = """[a-zA-z.]+""".r
  val regCompositeUnit: Regex = """(\w+)\s*([*/])\s*(\w+)""".r
  val regNum: Regex = """(-)?\d+(\.\d+)?(e(-)?\d+)?""".r

  val scalePrefixes: Seq[ScalePrefix] = Seq(
    ScalePrefix("yocto", "y", Nil, "1e-24"),
    ScalePrefix("zepto", "z", Nil, "1e-21"),
    ScalePrefix("atto" , "a", Nil, "1e-18"),
    ScalePrefix("femto", "f", Nil, "1e-15"),
    ScalePrefix("pico" , "p", Nil, "1e-12"),
    ScalePrefix("nano" , "n", Nil, "1e-9"),
    ScalePrefix("micro", "μ", Seq("mc"), "1e-6"),
    ScalePrefix("milli", "m", Nil, "1e-3"),
    ScalePrefix("centi", "c", Nil, "1e-2"),
    ScalePrefix("deci" , "d", Nil, "1e-1"),

    ScalePrefix("deca", "da", Nil, "1e1"),
    ScalePrefix("hecto", "h", Nil, "1e2"),
    ScalePrefix("kilo" , "k", Nil, "1e3"),
    ScalePrefix("mega" , "M", Nil, "1e6"),
    ScalePrefix("giga" , "G", Nil, "1e9"),
    ScalePrefix("tera" , "T", Nil, "1e12"),
    ScalePrefix("peta" , "P", Nil, "1e15"),
    ScalePrefix("exa"  , "E", Nil, "1e18"),
    ScalePrefix("zetta", "Z", Nil, "1e21"),
    ScalePrefix("yotta", "Y", Nil, "1e24")
  )
}