import java.io.File
import java.nio.charset.Charset

import com.google.gson.Gson
import sbt.io.IO

import scala.util.matching.Regex

object MultiverseSourceGenerator {

  private val destPath = new File("org/waman/multiverse/unit")

  def generate(info: File, srcManaged: File, src: File): Seq[File] = {
    // info: src/main/resources/physical-units
    // srcManaged: src/main/src_managed
    // src: src/main/scala

    IO.createDirectory(srcManaged)
    val factory = new JsonResourceFactory(info, srcManaged, src, destPath)

    def walk(f: File, acc: Seq[JsonResource]): Seq[JsonResource] =
      if (f.isFile) {
        // f: src/main/resources/physical-units/basic/LengthUnits.json

        // ex) src/main/resources/physical-units/basic/LengthUnits.json
        //         -> src/main/src_managed/org/waman/multiverse/unit/basic/Length.scala
        factory(f) +: acc

      } else if (f.isDirectory) {
        IO.listFiles(f).toList.flatMap(walk(_, acc))
      } else {
        acc
      }

    val jsons = walk(info, Nil)
    jsons.filter(_.isGenerating).map(_.asInstanceOf[SourceGeneratorJson]).flatMap(_.generate(jsons))
  }
}

object GenerationUtil{

  val rootPackage: String = "org.waman.multiverse"
  val gson: Gson = new Gson
  val utf8: Charset = Charset.forName("UTF-8")

  val regId: Regex = """[a-zA-z.]+""".r
  val regCompositeUnit: Regex = """(\w+)\s*([*/])\s*(\w+)""".r

  private val regNum: Regex = """(-)?\d+(\.\d+)?(e(-)?\d+)?""".r

  def refineNumber(s: String): String =
    regNum.replaceAllIn(s, m => s"""r"${s.substring(m.start, m.end)}"""")

  def toObjectName(s: String): String = {
    val ss = s.replace(' ', '_')
    if (ss.contains("(")) s"""`$ss`"""
    else ss
  }
}
