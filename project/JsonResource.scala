import java.io.File

import sbt.io.IO

class JsonResources(val jsons: Seq[JsonResource],
//                    val properties: PropertiesJson,  // generate another way
                    val scalePrefixJson: ScalePrefixesJson,
                    val constantsJson: ConstantsJson,
                    val unitdefs: Seq[UnitdefJson],
                    val customUnits: Seq[CustomUnitsJson],
                    val unitsystems: Seq[UnitSystemJson]){

  def generate(destRoot: File): Seq[File] = jsons.map(_.generate(destRoot))
}

object JsonResources {

  def apply(jsonRoot: File): JsonResources = {
    def jsonFile(s: String): File = IO.resolve(jsonRoot, new File(s))
    def jsonFiles(dir: String): Seq[File] = IO.resolve(jsonRoot, new File(dir)).listFiles().toSeq

//    val props = new PropertiesJson(jsonFile("Properties.json"))
    val scales = new ScalePrefixesJson(jsonFile("ScalePrefixes.json"))
    val consts = new ConstantsJson(jsonFile("Constants.json"))

    val unitdefs = getUnitdefs(jsonRoot)

    val customs = jsonFiles(s"unit/custom").map(new CustomUnitsJson(_))
    val unitsystems = jsonFiles("unitsystem").map(new UnitSystemJson(_))

    val jsons = scales +: consts +: unitdefs ++: customs ++: unitsystems ++: Nil
    new JsonResources(jsons, scales, consts, unitdefs, customs, unitsystems)
  }

  private def getUnitdefs(jsonRoot: File): Seq[UnitdefJson] = {
    def readUnitdefs(f: File, subpackage: String): Seq[UnitdefJson] = {
      if (f.isDirectory) {
        val sp = subpackage match {
          case null => ""
          case "" => f.getName
          case _ => subpackage+"."+f.getName
        }
        f.listFiles().flatMap(readUnitdefs(_, sp))

      } else {
        val ud: UnitdefJson = f.getName match {
          case "Temperature.json" => new HomogeneousUnitdefJson(f, subpackage)
          case "Length.json" => new LengthUnitdefJson(f, subpackage)
          case "Area.json" | "Volume.json" =>
            val unitName = f.getName.replaceAll(".json", "")
            new LengthPoweredUnitdefJson(f, subpackage, unitName)
          case "Time.json" => new TimeUnitdefJson(f, subpackage)
          case "TimeSquared.json" => new TimeSquaredUnitdefJson(f, subpackage)
          case _ => new LinearUnitdefJson(f, subpackage)
        }
        Seq(ud)
      }
    }

    val defDir = IO.resolve(jsonRoot, new File(s"unit/def"))
    readUnitdefs(defDir, null)
  }
}

abstract class JsonResource(val jsonFile: File){

  def generate(destRoot: File): File = {
    val destFile = getDestFile(destRoot)
    doGenerate(destFile)
    println("[GENERATE] " + destFile)
    destFile
  }

  protected def getDestFile(destRoot: File): File
  protected def doGenerate(destFile: File): Unit
}