import java.io.File

import sbt.io.IO

class JsonResourceFactory(info: File, srcManaged: File, destPath: File){
  // info: src/main/resources/physical-units
  // srcManaged: src/main/src_managed
  // src: src/main/scala

  def apply(json: File): JsonResource = {
    val subpackageDir = IO.relativizeFile(info, json.getParentFile).get // basic
    val destDir = IO.resolve(IO.resolve(srcManaged, destPath), subpackageDir) // src/main/src_managed/org/waman/multiverse/unit/basic
    val subpackage = subpackageDir.toString

    json.getName match {
      case "Properties.json" => new PropertiesJson(json, destDir)
      case "Constants.json" => new ConstantsJson(json, destDir)
      case "ScalePrefixes.json" => new ScalePrefixJson(json, destDir.getParentFile)  // org.waman.multiverse
      case "TemperatureUnits.json" => new HomogeneousUnitDefinitionJson(json, destDir, subpackage)
      case "LengthUnits.json" => new LengthUnitDefinitionJson(json, destDir, subpackage)
      case "AreaUnits.json" | "VolumeUnits.json" =>
        val s = json.getName.replace("Units.json", "")
        new LengthPoweredUnitDefinitionJson(s, json, destDir, subpackage)
      case "TimeUnits.json" => new TimeUnitDefinitionJson(json, destDir, subpackage)
      case _ => new LinearUnitDefinitionJson(json, destDir, subpackage)
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