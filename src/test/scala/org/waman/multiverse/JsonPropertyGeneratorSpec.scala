package org.waman.multiverse

import java.nio.charset.StandardCharsets

import org.scalatest.FlatSpec
import org.waman.gluino.nio.GluinoPath
import spire.math.Real

class JsonPropertyGeneratorSpec extends FlatSpec with GluinoPath{

  "Json property files" should "be generated" ignore {
    UnitSystem.supportedQuantities.foreach{ q =>
      generateJson(q)
    }
  }

  def generateJson(qClass: Class[_]): Unit = {
    val destDir = path(".") / ("src/main/resources/" + qClass.getPackage.getName.replaceAll("\\.+", "/"))

    val unitClassName = qClass.getSimpleName + "Unit"
    val dest = destDir / (unitClassName + ".json")
    if(dest.exists)return

    destDir.createDirectories()

    val unitClass = Class.forName(qClass.getPackage.getName + "." + unitClassName)
    val unitObjectClass = Class.forName(unitClass.getName + "$")
    val unitObject = unitObjectClass.getField("MODULE$").get(null)
    val cs = unitObject match {
      case c: ConstantsDefined[_] => c.values
      case _ => Set()
    }
    val constants = cs.map(_.asInstanceOf[PhysicalUnit[_]])

    dest.withWriter(StandardCharsets.UTF_8){ writer =>
      writer <<
        s"""{
           |  "imports":[
           |    "org.waman.multiverse.UnitSystem"
           |  ],""".stripMargin

      val baseUnit = constants.filter(_.valueInBaseUnit == Real.one) match {
        case c if c.isEmpty => ""
        case head::_ => head.name
      }
      val unitIn = unitClass.getMethods.filter(_.getName.startsWith("unitIn")).head

      writer <<
        s"""
           |
           |  "baseUnit":"$baseUnit",
           |  "baseUnitAccessor":"${unitIn.getName}",
           |""".stripMargin

      val ifs = unitClass.getInterfaces

      val muls = ifs.filter(_.getSimpleName.startsWith("MultiplicativeBy"))
      if(muls.nonEmpty) {
        val mulStr = muls.map(_.getSimpleName.replaceAll("MultiplicativeBy", "").replaceAll("Unit", ""))
                         .map(by => s"""["$by", ""]""").mkString(", ")

        writer <<
          s"""
             |  "multiplicative":[$mulStr],""".stripMargin
      }

      val divs = ifs.filter(_.getSimpleName.startsWith("DivisibleBy"))
      if(divs.nonEmpty) {
        val divStr = divs.map(_.getSimpleName.replaceAll("DivisibleBy", "").replaceAll("Unit", ""))
                         .map(by => s"""["$by", ""]""").mkString(", ")

        writer <<
          s"""
             |  "divisible":[$divStr],""".stripMargin
        }

      if(unitObjectClass.getMethods.exists(_.getName == "apply")){
        writer <<
          s"""
             |
             |  "product/quotient":["", ""],""".stripMargin
      }

      if(constants.nonEmpty){
        writer <<
          s"""
             |
             |  "constants":[""".stripMargin

        val scaledUnits = constants.map(_.name).filter(_.startsWith("Yocto")).map(_.replaceAll("Yocto", "")).toSet
        val reducedConstants = constants.filterNot { c =>
          scaledUnits.exists(s => c.name.contains(s) && c.name != s)
        }

        writer << reducedConstants.map{ c =>
          if(scaledUnits.contains(c.name)){
            s"""
               |    {"name":"${c.name}", "symbol":"${c.symbols.head}", "args":"", "scalePrefixes":true}""".stripMargin
          }else{
            s"""
               |    {"name":"${c.name}", "symbols":[${c.symbols.map("\"" + _ + "\"").mkString(", ")}], "args":""}""".stripMargin
          }
        }.mkString(",")

        writer <<
          s"""
             |  ]""".stripMargin
      }

      writer <<
        s"""
           |}""".stripMargin
    }
    println("[CREATE] " + dest.toAbsolutePath)
  }
}