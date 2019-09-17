import java.io.File

import sbt.io.IO

object MultiverseSourceGenerator {

  private val destPath = new File("org/waman/multiverse/unit")

  def generate(info: File, srcManaged: File, src: File): Seq[File] = {
    IO.createDirectory(srcManaged)
    val factory = new JsonResourceFactory(info, srcManaged, src, destPath)
    // info: src/main/resources/physical-units
    // srcManaged: src/main/src_managed
    // src: src/main/scala

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

    val jsons = walk(info, Nil)
    jsons.flatMap(_.generate(jsons))
  }
}