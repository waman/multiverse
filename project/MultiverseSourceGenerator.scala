import java.io.File

import sbt.io.IO
import sbt.util.Tracked
import sbt.util.Tracked

object MultiverseSourceGenerator {

  private val destPath = new File("org/waman/multiverse/unit")

  // info: src/main/resources/physical-units
  // srcManaged: src/main/src_managed
  def generate(info: File, srcManaged: File): Seq[File] =
    if (!srcManaged.exists() || info.lastModified() > srcManaged.lastModified())
      doGenerate(info, srcManaged)
    else
      allFiles(srcManaged)

  private def doGenerate(info: File, srcManaged: File): Seq[File] = {
    IO.createDirectory(srcManaged)
    val factory = new JsonResourceFactory(info, srcManaged, destPath)

    def walk(f: File, acc: Seq[JsonResource]): Seq[JsonResource] =
      if (f.isFile) {
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