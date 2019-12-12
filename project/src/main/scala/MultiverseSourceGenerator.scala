import java.io.File
import java.nio.charset.Charset

import com.google.gson.Gson
import sbt.io.IO
import sbt.util.Tracked

import scala.util.matching.Regex

object MultiverseSourceGenerator {
  import GenerationUtil._

  private val destPath = new File("org/waman/multiverse/unit")

  def generate(info: File, srcManaged: File, src: File): Seq[File] = {
    // info: src/main/resources/physical-units
    // srcManaged: src/main/src_managed
    // src: src/main/scala

//    Tracked.inputChanged(info){ f: (Boolean, File) =>
//      Tracked.outputChanged(srcManaged){ f: (Boolean, File) =>
//
//      }
//    }

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
    val generated = extractResources(jsons, classOf[SourceGeneratorJson]).flatMap(_.generate(jsons))

    val unitDefs = extractResources(jsons, classOf[UnitDefinitionJson])
    val implicits = ImplicitsGenerator.generate(srcManaged, unitDefs)

    implicits +: generated
  }
}

object GenerationUtil{

  val rootPackage: String = "org.waman.multiverse"
  val gson: Gson = new Gson
  val utf8: Charset = Charset.forName("UTF-8")

  val regexId: Regex = """[a-zA-z.]+""".r
  val regexCompositeUnit: Regex = """(\w+)\s*        ([*/])\s*(\w+)""".r

  private val regexNum: Regex = """(-)?\d+(\.\d+)?(e(-)?\d+)?""".r
  def refineNumbers(s: String): String = s match {
    // TODO
    case "log(2)" => "Real(2).log()"  // Entropy.bit
    case "log(10)" => "Real(10).log()"  // Entropy.ban
    case "sqrt(1/10)" => """Real("1/10").sqrt()"""  // Length.
    case _ => regexNum.replaceAllIn(s, m => s"""r"${s.substring(m.start, m.end)}"""")
  }

  private val regexUnitName: Regex = """[\w.()]+""".r
  def refineUnitNames(s: String): String = regexUnitName.replaceAllIn(s, m => {
    val str = s.substring(m.start, m.end)
    val (prefix, unitName) = str.indexOf('.') match {
      case -1 => ("", str)
      case i => (str.substring(0, i)+"UnitObjects.", str.substring(i+1))
    }

    val escapedUnitName = if (unitName.contains('(')) s"`$unitName`" else unitName
    prefix + escapedUnitName + ".interval"
  })

  def extraUnitsInBaseUnit(s: String): Seq[String] = regexUnitName.findAllMatchIn(s).map{ m =>
    val str = s.substring(m.start, m.end)
    str.indexOf('.') match {
      case -1 => Nil
      case i => Seq(str.substring(0, i))
    }
  }.toSeq.flatten

  def toObjectName(s: String): String = {
    val ss = s.replace(' ', '_')
    if (ss.contains("(")) s"""`$ss`"""
    else ss
  }

  def extractResources[U <: JsonResource](jsons: Seq[JsonResource], cls: Class[U]): Seq[U] =
    jsons.filter(cls.isInstance(_)).map(cls.cast(_))

  def searchUnitDefinition[U <: UnitDefinitionJson](id: String, unitDefs: Seq[U]): U =
    unitDefs.find(_.id == id) match {
      case Some(ud) => ud
      case _ => throw new RuntimeException(s"""Unknown unit appears: $id""")
    }

  def headToLower(s: String): String = Character.toLowerCase(s.charAt(0)) + s.substring(1)
}
