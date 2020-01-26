import java.io.File

import sbt.io.IO

object UnitSystemTestsGenerator {
  import GenerationUtil._

  def generate(jsons: JsonResources, srcManagedTest: File): Unit = {
    val destDir = IO.resolve(srcManagedTest, new File("org/waman/multiverse/unitsystem"))
    if (!destDir.exists()) IO.createDirectory(destDir)

    val destFile = IO.resolve(destDir, new File("MKSSpec.scala"))
    IO.writer(destFile, "", utf8, append=false) { writer =>
      writer.write(
        s"""package org.waman.multiverse.unitsystem
           |
           |import org.waman.multiverse.MultiverseCustomSpec
           |
           |class MKSSpec extends MultiverseCustomSpec{
           |  import org.waman.multiverse.implicits._
           |  import org.waman.multiverse.unitsystem.MKS._
           |
           |  "Length object is implicitly converted to the Double value in metre" in {
           |    // SetUp
           |    import org.waman.multiverse.unit.basic.LengthUnits._
           |    val expected = 7.0(m)(m)
           |    // Exercise
           |    val sut: Double = 7.0(m)
           |    // Verify
           |    sut should equal (%%%%(expected))
           |  }
           |}
           |""".stripMargin)

    }

    println("[GENERATE] " + destFile)
  }
}
