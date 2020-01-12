import java.io.File

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class ScalePrefix(name: String, prefix: String, aliases: Array[String], scale: String){
  lazy val _aliases: Seq[String] =
    if (aliases != null) aliases else Nil
}

class ScalePrefixJson(jsonFile: File, destDir: File)
    extends SourceGeneratorJson(jsonFile, destDir){

  import GenerationUtil._

  override def destFilename: String = "ScalePrefixes.scala"
  override def packageName: String = rootPackage

  private val scalePrefixType: Class[_ >: Array[ScalePrefix]] = new TypeToken[Array[ScalePrefix]]() {}.getRawType

    val scalePrefixes: Seq[ScalePrefix] = IO.reader(jsonFile, utf8) { reader =>
      gson.fromJson(reader, scalePrefixType).asInstanceOf[Array[ScalePrefix]].toSeq
  }

  override protected def doGenerate(jsons: JsonResources): Unit =
    IO.writer(destFile, "", utf8, append = false) { writer =>
      writer.write(
        s"""package $rootPackage
           |
           |import spire.math._
           |import spire.implicits._
           |
           |object ScalePrefixes {
           |
           |""".stripMargin)

      scalePrefixes.foreach{ sp =>
        writer.write(s"""  private val ${sp.name}Real: Real = r"${sp.scale}"\n""")
      }

      writer.write(
        s"""
           |  private def fromReal[A: Fractional](value: Real): A = implicitly[Fractional[A]].fromReal(value)
           |
           |""".stripMargin)

      scalePrefixes.foreach{ sp =>
        writer.write(s"""  def ${sp.name}[A: Fractional]: A = fromReal(${sp.name}Real)\n""")
      }

      writer.write("\n")

      scalePrefixes.foreach{ sp =>
        writer.write(s"""  def ${sp.name}[A: Fractional](value: A): A = ${sp.name} * value\n""")
      }

      writer.write("}\n")
    }
}