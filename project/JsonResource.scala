import java.io.{BufferedWriter => BW, File}

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
      case "ScalePrefixes.json" => new ScalePrefixJson(json, destDir.getParentFile, mainDir)  // org.waman.multiverse
      case "TemperatureUnits.json" => new HomogeneousUnitDefinitionJson(json, destDir, mainDir, subpackage.toString)
      case "LengthUnits.json" => new LengthUnitDefinitionJson(json, destDir, mainDir, subpackage.toString)
      case "TimeUnits.json" => new TimeUnitDefinitionJson(json, destDir, mainDir, subpackage.toString)
      case _ => new LinearUnitDefinitionJson(json, destDir, mainDir, subpackage.toString)
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

  def generate(): Seq[File] = extractResources(classOf[SourceGeneratorJson]).flatMap(_.generate(this))
}

abstract class JsonResource(val jsonFile: File)

abstract class SourceGeneratorJson(jsonFile: File, destDir: File, mainDir: File)
    extends JsonResource (jsonFile){

  def destFilename: String

  lazy val destFile: File = IO.resolve(destDir, new File(destFilename))
  def packageName: String

  def generate(jsons: JsonResources): Seq[File] = {
    if (!IO.resolve(mainDir, new File(destFilename)).exists()){
      doGenerate(jsons)
      println("[GENERATE] " + this.destFilename)
      Seq(this.destFile)
    } else {
      Nil
    }
  }

  protected def doGenerate(jsons: JsonResources): Unit
}