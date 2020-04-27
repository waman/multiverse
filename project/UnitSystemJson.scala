import java.io.{File, BufferedWriter => BW}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class UnitSystemInfo(description: String, parent: String, evalEntries: Array[Entry]){
  def _parent: String = if (this.parent != null) this.parent else "UnitSystem"
  lazy val _evalEntries: Seq[Entry] = GenerationUtil.toSeq(evalEntries)
}

case class Entry(quantity: String, unit: String)

class UnitSystemJson(jsonFile: File, destDir: File)
  extends SourceGeneratorJson(jsonFile, destDir){

  import GenerationUtil._

  val id: String = jsonFile.getName.replace(".json", "")  // MKS
  val destFilename: String =  id + ".scala"// MKS.scala
  val packageName: String = GenerationUtil.rootPackage + ".unitsystem"

  val unitsystemInfoType: Class[_ >: UnitSystemInfo] = new TypeToken[UnitSystemInfo]() {}.getRawType

  val unitsystemInfo: UnitSystemInfo = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, unitsystemInfoType).asInstanceOf[UnitSystemInfo]
  }

  override protected def doGenerate(jsons: JsonResources): Unit = {
    IO.writer(this.destFile, "", utf8, append = false) { writer: BW =>
      writer.write(
        s"""package $packageName
           |
           |import scala.language.implicitConversions
           |
           |""".stripMargin)

      this.unitsystemInfo._evalEntries.foreach{ e =>
        val ud = jsons.searchUnitDefinition(e.quantity)
        writer.write(
          s"""import $rootPackage.unit.${ud.subpackage}.${e.quantity}\n""")
      }

      writer.write("\n")

      this.unitsystemInfo._evalEntries.flatMap {
        case e if isCompositeUnit(e.unit) => extractUnitTypes(e.unit)
        case e => Seq((e.quantity, e.unit))
      }.distinct.foreach{ e =>
        val ud = jsons.searchUnitDefinition(e._1)
        writer.write(s"""import $rootPackage.unit.${ud.subpackage}.${e._1}UnitObjects.${e._2}\n""")
      }

      writer.write("\n")

      if (this.unitsystemInfo.description != null) {
        writer.write(
          s"""/**
             | * ${this.unitsystemInfo.description}
             | */
             |""".stripMargin)
      }

      writer.write(
        s"""trait $id extends ${this.unitsystemInfo._parent}{
           |""".stripMargin)

      this.unitsystemInfo._evalEntries.foreach{ e =>
        val evalUnit =
          if (isCompositeUnit(e.unit)) refineUnitNamesInUnitSystem(e.unit)
          else e.unit

        writer.write(s"""  implicit def evaluate${e.quantity}[A: Fractional](q: ${e.quantity}[A]): A = q($evalUnit)\n""")
      }

      writer.write(
        s"""}
           |
           |object $id extends $id
           |""".stripMargin)
    }
  }
}