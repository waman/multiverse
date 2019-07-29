import java.io.File
import java.nio.charset.Charset

import com.google.gson._
import com.google.gson.reflect.TypeToken
import sbt.Tracked.{inputChanged, outputChanged}
import sbt.io.IO

import scala.collection.mutable

object MultiverseSourceGenerator{

  val gson = new Gson

  private val predefPath = "org/waman/multiverse/predef"
  private val utf8 = Charset.forName("UTF-8")

  def generate(info: File, srcManaged: File): Seq[File] = {
    IO.createDirectory(srcManaged)
    // info: src/main/resources/physical-units
    // srcManaged: src/main/src_managed

    def walk(f: File): Seq[File] =
      if(f.isFile){
        // json: src/main/resources/physical-units/basic/LengthUnits.json
        // info: src/main/resources/physical-units
        // srcManaged: src/main/src_managed

        // ex) src/main/resources/physical-units/basic/LengthUnits.json
        //         -> src/main/src_managed/org/waman/multiverse/predef/basic/LengthUnits.scala
        val destFilename = f.getName.replace("json", "scala")  // LengthUnits.scala
        val rel = IO.relativizeFile(info, f.getParentFile).get  // basic
        val destDir = IO.resolve(IO.resolve(srcManaged, new File(predefPath)), rel)  // src/main/src_managed/org/waman/multiverse/predef/basic
        val destFile = IO.resolve(destDir, new File(destFilename))

        val packageName = IO.relativize(srcManaged, destFile.getParentFile).get.toString.replaceAll("[/\\\\]", ".")  // org.waman.multiverse.predef.basic

        if(!destFile.exists()){
          generateSource(f, destFile, packageName)

        }else{
          import sbt.util.CacheImplicits._
          inputChanged(f){ (inChanged, _: File) =>
            outputChanged(destFile){ (outChanged, _: File) =>
              if(inChanged || outChanged)
                generateSource(f, destFile, packageName)
            }
          }
        }
        Seq(destFile)

      }else if(f.isDirectory){
        IO.listFiles(f).toList.flatMap(walk)
      }else{
        Nil
      }

    walk(info)
  }

//  def generateContexts(rsrc: Path, srcManaged: Path): (List[Path], Map[String, Array[ContextJson]]) = {
//
//    val seq = rsrc.filesMatchRecurse(FileType.Files, fileExtensionFilter("Context.json")).map { json =>
//      val id = json.getFileName.toString.replaceAll("\\.json", "")
//      val rel = rsrc.relativize(json.getParent)
//      val packageName = rel.toString.replaceAll("[/\\\\]+", ".")
//      val generated = srcManaged / rel / (id + ".scala")
//
//      val nsArray = json.withReader(gson.fromJson(_, contextType)).asInstanceOf[Array[ContextJson]]
//
//      if(!generated.exists || generated.isOlderThan(json) ) {
//        if (generated.exists) generated.delete()
//        generated.getParent.createDirectories()
//
//        generateContext(id, packageName, generated, nsArray)
//        println("[GENERATE] " + generated)
//      }
//
//      (generated, packageName + "." + id, nsArray)
//      // (generated, "org.waman.multiverse.metric.MetricContext", Array(NameSymbol("UnitedStates", "US"), ...))
//    }
//
//    (seq.map(_._1).toList, Map(seq.map(t => (t._2, t._3)):_*))
//  }

//  def generateContext(id: String, packageName: String, generated: Path, contextArray: Array[ContextJson]): Unit = {
//    generated.withWriter{ writer =>
//      // Context trait
//      writer <<
//        s"""package $packageName
//           |
//           |import org.waman.multiverse.ConstantsDefined
//           |
//           |sealed abstract class $id(val name: String, val symbol: String)""".stripMargin
//
//      // Context object
//      writer <<
//        s"""
//           |
//           |object $id extends ConstantsDefined[$id]{
//           |""".stripMargin
//
//      contextArray.foreach { ns =>
//        writer <<
//          // case object UnitedStates extends Context("UnitedStates", "US")
//          s"""
//             |  case object ${ns.name} extends $id("${ns.name}", "${ns.symbol}")""".stripMargin
//      }
//
//      writer <<
//        s"""
//           |
//           |  lazy val values = Seq(${contextArray.map(_.name).mkString(", ")})
//           |}""".stripMargin
//
//      // ContextDefined trait
//      writer <<
//        s"""
//           |
//           |trait ${id}Defined{
//           |  import $id._
//           |""".stripMargin
//
//      contextArray.foreach{ ns =>
//        writer <<
//          // val US = Context.UnitedStates
//          s"""
//             |  val ${ns.symbol} = ${ns.name}""".stripMargin
//      }
//
//      writer <<
//        s"""
//           |}""".stripMargin
//    }
//  }

//  def generateUnitSystem(rsrc: Path, srcManaged: Path, contextLists: Map[String, Array[ContextJson]]):
//  (Path, Array[UnitSource]) = {
//
//    val generated = srcManaged / "org/waman/multiverse/UnitSystem.scala"
//
//    val unitSystemInfo = jsonPropertyFilesOfUnits(rsrc).map { json: Path =>
//      val rel = rsrc.relativize(json.getParent)
//      val packageName = rel.toString.replaceAll("[/\\\\]+", ".")
//      val className = json.getFileName.toString.replace("Unit.json", "")
//      val fullClassName = s"$packageName.$className"
//
//      val prop = json.withReader(gson.fromJson(_, unitType)).asInstanceOf[UnitJson]
//      val us = UnitSource(json, packageName, className, prop)
//
//      if (prop.hasConstants)
//        List(us, fullClassName, s"$packageName.Predefined${className}Unit")
//      else
//        List(us, fullClassName)
//    }
//
//    if(!generated.exists || jsonPropertyFilesOfUnits(rsrc).forall(generated.isOlderThan)) {
//      if (generated.exists) generated.delete()
//
//      generated.withWriter { writer =>
//        writer <<
//          s"""package org.waman.multiverse
//             |
//             |trait UnitSystem extends UnitSystemImplicits""".stripMargin
//
//        contextLists.foreach{ case (contextClass, _) =>
//          writer <<
//            s"""
//               |  with ${contextClass}Defined""".stripMargin
//        }
//
//        unitSystemInfo.flatMap {
//          case list if list.length == 3 => List(list(2))
//          case _ => Nil
//        }.foreach{ predef =>
//          writer <<
//            s"""
//               |  with $predef""".stripMargin
//        }
//
//        writer <<
//          s"""
//             |
//             |object UnitSystem extends UnitSystem{
//             |
//             |  lazy val supportedQuantities: Set[Class[_]] = Set(
//             |    """.stripMargin
//
//        writer << unitSystemInfo.map(_(1)).map(
//          "classOf[" + _ + "[_]]").mkString(
//          s""",
//             |    """.stripMargin)
//        writer <<
//          s"""
//             |  )
//             |}""".stripMargin
//      }
//      println("[GENERATE] " + generated)
//    }
//
//    val props = unitSystemInfo.map(_(0).asInstanceOf[UnitSource]).toArray
//    (generated, props)
//  }
//

  //  val contextType = new TypeToken[Array[ContextJson]](){}.getRawType

  def generateSource(json: File, destFile: File, packageName: String): Unit = {
    val filename = json.getName
    if(filename == "Constants.json"){
      generateConstants(json, destFile, packageName)
      println("[GENERATE] " + destFile)

    }else if(filename.endsWith("Units.json")){
      generatePredefUnit(json, destFile, packageName)
      println("[GENERATE] " + destFile)
    }
  }

  val constantsType: Class[_ >: Array[Constant]] = new TypeToken[Array[Constant]](){}.getRawType

  def generateConstants(json: File, destFile: File, packageName: String): Unit = {
    IO.reader(json, utf8){ reader =>
      val consts = gson.fromJson(reader, constantsType).asInstanceOf[Array[Constant]]

      IO.writer(destFile, "", utf8, append=false) { writer =>
        writer.write(s"package $packageName\n\n")
        writer.write("import spire.math.Real\n")
        writer.write("import spire.implicits._\n\n")
        writer.write("object Constants{\n")

        consts.foreach { c =>
          writer.write(s"""  val ${c.name}: Real = r"${c.value}"\n""")
        }

        writer.write("}")
      }
    }
  }

  val unitCategoryType: Class[_ >: UnitCategory] = new TypeToken[UnitCategory](){}.getRawType

  def generatePredefUnit(json: File, destFile: File, packageName: String): Unit = {
    val id = destFile.getName.replaceAll("Units.scala", "")
    IO.reader(json, utf8) { reader =>
      val categ = gson.fromJson(reader, unitCategoryType).asInstanceOf[UnitCategory]
      val units = categ.units.flatMap(_.canonicalizeAndExpandScalePrefixes())

      val objCode = new StringBuilder
      val unitsCode = new StringBuilder
      val getUnitsCode = new mutable.ListBuffer[String]

      objCode.append(s"object ${id}UnitObjects{\n")
      unitsCode.append(s"object ${id}Units{\n")

      units.foreach { u =>
        val objName = u.name.replaceAll("\\s", "_")

        //***** PredefLengthUnitObjects *****
        val aliases =
          if (u.aliases.isEmpty) "Nil"
          else u.aliases.mkString("Seq(\"", "\", \"", "\")")

        val notExact =
          if (!u.notExact) ""
          else " with NotExact"

        // final case object metre extends SimpleLengthUnit("metre", "m", Nil, r"1")
        objCode.append(
          s"""  final object $objName extends Simple${id}Unit(""" +
            s""""${u.name}", "${u.symbol}", $aliases, ${u.intervalInSIUnit})$notExact\n""")

        //***** getUnitsCode *****
        getUnitsCode.append(objName)

        //***** PredefLengthUnits *****
        // def m: LengthUnit = LengthUnitObjects.metre
        unitsCode.append(s"""  def ${u.symbol}: ${id}Unit = ${id}UnitObjects.$objName\n""")

        u.aliases.foreach { a =>
          if (!a.contains("(")) // TODO
            unitsCode.append(s"""  def $a: ${id}Unit = ${id}UnitObjects.$objName\n""")
        }
      }

      objCode.append(s"""\n  def getUnits: Seq[${id}Unit] = \n""")
      objCode.append(getUnitsCode.mkString("    Seq(", ", ", ")\n\n"))
      objCode.append("}\n\n")

      unitsCode.append(s"""\n  def getUnits: Seq[${id}Unit] = ${id}UnitObjects.getUnits\n""")
      unitsCode.append("}")

      IO.writer(destFile, "", utf8, append = false) { writer =>
        writer.write(s"package $packageName\n\n")
        writer.write("import spire.math.Real\n")
        writer.write("import spire.implicits._\n\n")

        // import org.waman.multiverse.units.basic.LengthUnit
        writer.write("import org.waman.multiverse._\n")
        writer.write("import org.waman.multiverse.predef._\n")
        writer.write(s"""import ${packageName.replaceAll("predef", "units")}.${id}Unit\n\n""")

        // class SimpleEnergyUnit(val name: String, val symbol: String, val aliases: Seq[String], val intervalInSIUnit: Real) extends EnergyUnit
        writer.write(
          s"""class Simple${id}Unit(val name: String, val symbol: String, val aliases: Seq[String], val intervalInSIUnit: Real) extends ${id}Unit\n\n""")

        writer.write(objCode.toString)
        writer.write(unitsCode.toString)
      }
    }
  }
}