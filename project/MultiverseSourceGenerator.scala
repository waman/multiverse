import sbt.io.IO

import java.io.File

object MultiverseSourceGenerator {

  import GenerationUtil._

  // jsonRoot: src/main/resources/unitdefs/json_simplified
  // srcManaged: src/main/src_managed
  def generate(jsonRoot: File, srcManaged: File): Seq[File] = {

    val jsons = JsonResources(jsonRoot)

    // destRoot: src/main/src_managed/org/waman/multiverse
    val destRoot = IO.resolve(srcManaged, new File(rootPackage.replace('.', '/')))
    IO.createDirectory(destRoot)

    val generated = jsons.generate(destRoot)
    val properties = PropertiesGenerator.generate(jsonRoot, destRoot, jsons)
    val metricAttributes = MetricAttributesGenerator.generate(destRoot, jsons)
    val implicits = ImplicitsGenerator.generate(destRoot, jsons)
    properties +: metricAttributes +: implicits +: generated
  }
}