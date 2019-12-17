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

    val jsons = new JsonResources(walk(info, Nil))
    val generated = jsons.generate()

    val implicits = ImplicitsGenerator.generate(srcManaged, jsons.unitDefs)

    implicits +: generated
  }
}

class JsonResources(val jsons: Seq[JsonResource]){

  def extractResources[U <: JsonResource](jsons: Seq[JsonResource], cls: Class[U]): Seq[U] =
    jsons.filter(cls.isInstance(_)).map(cls.cast(_))

  def searchUnitDefinition(id: String): UnitDefinitionJson =
    unitDefs.find(_.id == id) match {
      case Some(ud) => ud
      case _ => throw new RuntimeException(s"""Unknown unit appears: $id""")
    }

  val scalePrefixJson: ScalePrefixJson = jsons.find(_.isInstanceOf[ScalePrefixJson]).get.asInstanceOf[ScalePrefixJson]
  val unitDefs: Seq[UnitDefinitionJson] = extractResources(jsons, classOf[UnitDefinitionJson])

  def generate(): Seq[File] = extractResources(jsons, classOf[SourceGeneratorJson]).flatMap(_.generate(this))
}

object GenerationUtil{

  val rootPackage: String = "org.waman.multiverse"
  val gson: Gson = new Gson
  val utf8: Charset = Charset.forName("UTF-8")

  val regexId: Regex = """[a-zA-z.]+""".r
  val regexCompositeUnit: Regex = """(\w+)\s*([*/])\s*(\w+)""".r

  private val regexNum: Regex = """(-)?\d+(\.\d+)?(e(-)?\d+)?""".r
  def refineNumbers(s: String): String = s match {
    // TODO
    case "log(2)" => "Real(2).log()"  // Entropy.bit
    case "log(10)" => "Real(10).log()"  // Entropy.ban
    case "sqrt(1/10)" => """Real("1/10").sqrt()"""  // Length.metric_foot
    case _ => regexNum.replaceAllIn(s, m => s"""r"${s.substring(m.start, m.end)}"""")
  }

  private val regexUnitName: Regex = """[\w.()^\d]+""".r
  def refineUnitNames(s: String): String = regexUnitName.replaceAllIn(s, m => {
    val str = s.substring(m.start, m.end)
    val (prefix, unitName) = str.indexOf('.') match {
      case -1 => ("", str)
      case i => (str.substring(0, i)+"UnitObjects.", str.substring(i+1))
    }

    val (baseUN, power) =
      if (unitName.endsWith("^2")) (unitName.substring(0, unitName.length-2), 2)
      else if (unitName.endsWith("^3")) (unitName.substring(0, unitName.length-2), 3)
      else (unitName, 1)

    val escapedUN = if (baseUN.contains('(')) s"`$baseUN`" else baseUN

    power match {
      case 2 | 3 => s"$prefix$escapedUN.interval**$power"
      case _ => s"$prefix$escapedUN.interval"
    }
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

  def headToLower(s: String): String = Character.toLowerCase(s.charAt(0)) + s.substring(1)
}
