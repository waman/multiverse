import java.io.File
import sbt.io.IO

object MultiverseSourceGenerator {

  private val destPath = new File("org/waman/multiverse")

  // unitdefs: src/main/resources/unitdefs
  // srcManaged: src/main/src_managed
  def generate(unitdefs: File, srcManaged: File): Seq[File] =
    if (!srcManaged.exists() || unitdefs.lastModified() > srcManaged.lastModified())
      doGenerate(unitdefs, srcManaged)
    else
      allFiles(srcManaged)

  private def doGenerate(unitdefs: File, srcManaged: File): Seq[File] = {
    IO.createDirectory(srcManaged)
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
    val generated = jsons.generate()

    val implicits = ImplicitsGenerator.generate(srcManaged, jsons)

    implicits +: generated
  }

  private def allFiles(srcManaged: File): Seq[File] = {
    def allFiles(f: File, acc: Seq[File]): Seq[File] =
      if (f.isFile)
        f +: acc
      else if (f.isDirectory)
        IO.listFiles(f).toList.flatMap(allFiles(_, acc))
      else
        acc

    allFiles(srcManaged, Nil)
  }
}