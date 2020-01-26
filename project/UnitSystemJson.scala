import java.io.{File, BufferedWriter => BW}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class UnitSystemInfo(extend: String, entries: Array[Entry]){
  lazy val _entries: Seq[Entry] = GenerationUtil.toSeq(entries)
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

      this.unitsystemInfo._entries.foreach{ e =>
        val ud = jsons.searchUnitDefinition(e.quantity)
        writer.write(
          s"""import $rootPackage.unit.${ud.subpackage}.${e.quantity}\n""")
      }

      writer.write("\n")

      this.unitsystemInfo._entries.flatMap {
        case e if isCompositeUnit(e.unit) => extractUnitTypes(e.unit)
        case e => Seq((e.quantity, e.unit))
      }.distinct.foreach{ e =>
        val ud = jsons.searchUnitDefinition(e._1)
        writer.write(s"""import $rootPackage.unit.${ud.subpackage}.${e._1}UnitObjects.${e._2}\n""")
      }

      writer.write(
        s"""
           |trait $id {
           |""".stripMargin)

      this.unitsystemInfo._entries.foreach{ e =>
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