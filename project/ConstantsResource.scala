import java.io.File

import com.google.gson.reflect.TypeToken
import sbt.io.IO

class ConstantsResource(json: File, destDir: File, mainDir: File)
    extends JsonResource(json, destDir, mainDir) {

  val constantsType: Class[_ >: Array[Constant]] = new TypeToken[Array[Constant]]() {}.getRawType

  val destFilename: String = "Constants.scala"
  val packageName: String = GenerationUtil.rootPackage + ".unit"

  override def isLinearUnit: Boolean = false

  override protected def doGenerate(jsons: Seq[JsonResource]): Unit =
    IO.reader(jsonFile, UTF8) { reader =>
      val consts = gson.fromJson(reader, constantsType).asInstanceOf[Array[Constant]]

      IO.writer(destFile, "", UTF8, append = false) { writer =>
        writer.write(
          s"""package $packageName
             |
             |import spire.math.Real
             |import spire.implicits._
             |
             |object Constants{
             |""".stripMargin)

        consts.foreach { c =>
          if (c.name == "Pi")
            writer.write(s"""  val Pi: Real = Real.pi\n""")
          else
            writer.write(s"""  val ${c.name}: Real = r"${c.value}"\n""")
        }

        writer.write("}")
      }
    }
}

case class Constant(name: String, value: String)