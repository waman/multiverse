import java.io.File

import sbt.io.IO

class JsonResourceFactory(unitdefs: File, srcManaged: File, destPath: File){
  // unitdefs: src/main/resources/unitdefs
  // srcManaged: src/main/src_managed

  private val unitDir = "unit"+File.separator
  private val unitsystemDir = "unitsystem"+File.separator

  def apply(json: File): JsonResource = {
    val destRoot = IO.resolve(srcManaged, destPath)  // src/main/src_managed/org/waman/multiverse
    IO.relativize(unitdefs, json.getParentFile) match {
      case Some("") =>
        json.getName match {
          case "ScalePrefixes.json" => new ScalePrefixJson(json, destRoot)
          case "Properties.json" => new PropertiesJson(json, destRoot)
          case x => throw new RuntimeException(s"Unknown json file appears: $x")
        }

      case Some("unit") =>
        json.getName match {
          case "Constants.json" => new ConstantsJson(json, IO.resolve(destRoot, new File("unit")))
          case x => throw new RuntimeException(s"Unknown json file appears: $x")
        }

      case Some(s) if s.contains(unitDir) =>
        val subpackage = s.replace(unitDir, "") // basic
        val destDir = IO.resolve(destRoot, new File(s)) // src/main/src_managed/org/waman/multiverse/unit/basic

        json.getName match {
          case "TemperatureUnits.json" => new HomogeneousUnitDefinitionJson(json, destDir, subpackage)
          case "LengthUnits.json" => new LengthUnitDefinitionJson(json, destDir, subpackage)
          case "AreaUnits.json" | "VolumeUnits.json" =>
            val s = json.getName.replace("Units.json", "")
            new LengthPoweredUnitDefinitionJson(s, json, destDir, subpackage)
          case "TimeUnits.json" => new TimeUnitDefinitionJson(json, destDir, subpackage)
          case _ => new LinearUnitDefinitionJson(json, destDir, subpackage)
        }

      case Some(s) if s.contains(unitsystemDir) =>
        throw new RuntimeException(s"""Unknown json file appears: $s""")

      case x => throw new RuntimeException(s"""Unknown json file appears: $x""")
    }
  }
}

class JsonResources(val jsons: Seq[JsonResource]){

  def extractResources[U <: JsonResource](cls: Class[U]): Seq[U] =
    jsons.filter(cls.isInstance(_)).map(cls.cast(_))

  def searchUnitDefinition(id: String): UnitDefinitionJson =
    unitDefs.find(_.id == id) match {
      case Some(ud) => ud
      case _ => throw new RuntimeException(s"""Unknown unit appears: $id""")
    }

  val scalePrefixJson: ScalePrefixJson = jsons.find(_.isInstanceOf[ScalePrefixJson]).get.asInstanceOf[ScalePrefixJson]
  val unitDefs: Seq[UnitDefinitionJson] = extractResources(classOf[UnitDefinitionJson])
  val linearUnitDefs: Seq[LinearUnitDefinitionJson] = extractResources(classOf[LinearUnitDefinitionJson])

  def generate(): Seq[File] = extractResources(classOf[SourceGeneratorJson]).map(_.generate(this))
}

abstract class JsonResource(val jsonFile: File)

abstract class SourceGeneratorJson(jsonFile: File, destDir: File)
    extends JsonResource (jsonFile){

  def destFilename: String

  lazy val destFile: File = IO.resolve(destDir, new File(destFilename))
  def packageName: String

  def generate(jsons: JsonResources): File = {
    doGenerate(jsons)
    println("[GENERATE] " + this.destFilename)
    this.destFile
  }

  protected def doGenerate(jsons: JsonResources): Unit
}