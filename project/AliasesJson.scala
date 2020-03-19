import java.io.File

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class Aliase(name: String, is: String)

class AliasesJson(jsonFile: File, destDir: File, subpackage: String)
    extends SourceGeneratorJson(jsonFile, destDir){

  import GenerationUtil._

  override def destFilename: String = "package.scala"
  val packageName: String = GenerationUtil.rootPackage + ".unit." + subpackage

  private val aliasesType: Class[_ >: Array[Aliase]] = new TypeToken[Array[Aliase]]() {}.getRawType

  val aliases: Seq[Aliase] = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, aliasesType).asInstanceOf[Array[Aliase]].toSeq
  }

  override protected def doGenerate(jsons: JsonResources): Unit =
    IO.writer(destFile, "", utf8, append = false) { writer =>
      writer.write(s"""package $rootPackage.unit\n\n""")

      foreachUnitDefinition(this.aliases.map(_.is), jsons){ ud =>
        if (this.subpackage != ud.subpackage) {
          writer.write(
            s"""import ${ud.packageName}.${ud.id}
               |import ${ud.packageName}.${ud.id}Unit
               |
               |""".stripMargin)
        }
      }

      writer.write(s"""package object $subpackage {\n""")

      this.aliases.foreach{ a =>
        writer.write(
          s"""
             |  type ${a.name}[A] = ${a.is}[A]
             |  type ${a.name}Unit = ${a.is}Unit
             |""".stripMargin)
      }

      writer.write("}\n")
    }
}