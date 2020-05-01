import java.io.File

import sbt.io.IO

class JsonResourceFactory(unitdefs: File, srcManaged: File, destPath: File){
  // unitdefs: src/main/resources/unitdefs
  // srcManaged: src/main/src_managed

  private val unitDir = "unit"+File.separator

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
          case _ => new UnitsJson(json, IO.resolve(destRoot, new File("unit")))
        }

      case Some(s) if s.contains(unitDir) =>
        val subpackage = s.replace(unitDir, "") // basic
        val destDir = IO.resolve(destRoot, new File(s)) // src/main/src_managed/org/waman/multiverse/unit/basic

        json.getName match {
//          case "aliases.json" => new AliasesJson(json, destDir, subpackage)
          case "Temperature.json" => new HomogeneousUnitDefinitionJson(json, destDir, subpackage)
          case "Length.json" => new LengthUnitDefinitionDefinitionJson(json, destDir, subpackage)
          case "Area.json" | "Volume.json" =>
            val s = json.getName.replace(".json", "")
            new LengthPoweredUnitDefinitionDefinitionJson(s, json, destDir, subpackage)
          case "Time.json" => new TimeUnitDefinitionJson(json, destDir, subpackage)
          case _ => new LinearUnitDefinitionJson(json, destDir, subpackage)
        }

      case Some("unitsystem") =>
        new UnitSystemJson(json, IO.resolve(destRoot, new File("unitsystem")))

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
  val unitsystems: Seq[UnitSystemJson] = extractResources(classOf[UnitSystemJson])

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
    println("[GENERATE] " + this.destFile)
    this.destFile
  }

  protected def doGenerate(jsons: JsonResources): Unit
}