import java.io.File
import sbt.io.IO

object MultiverseSourceGenerator {

  import GenerationUtil._

  private val destPath = new File(rootPackage.replace('.', File.separatorChar))

  // unitdefs: src/main/resources/unitdefs
  // srcManaged: src/main/src_managed
  def generate(unitdefs: File, srcManaged: File): Seq[File] = {
    IO.createDirectory(srcManaged)
    if (lastModifiedIn(unitdefs) > lastModifiedIn(srcManaged))
      doGenerate(unitdefs, srcManaged)
    else
      GenerationUtil.allFiles(srcManaged)
  }

  private def doGenerate(unitdefs: File, srcManaged: File): Seq[File] = {

    val factory = new JsonResourceFactory(unitdefs, srcManaged, destPath)

    def walk(f: File, acc: Seq[JsonResource]): Seq[JsonResource] =
      if (f.isFile) {
        // ex) src/main/resources/unitdefs/unit/basic/LengthUnits.json
        //         -> src/main/src_managed/org/waman/multiverse/unit/basic/Length.scala
        factory(f) +: acc

      } else if (f.isDirectory) {
        IO.listFiles(f).toList.flatMap(walk(_, acc))
      } else {
        acc
      }

    val jsons = new JsonResources(walk(unitdefs, Nil))
    UnitdefsConsistencyChecker.test(jsons)

    val generated = jsons.generate()
    val implicits = ImplicitsGenerator.generate(jsons, srcManaged)
    implicits +: generated
  }
}