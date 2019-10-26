import java.io
import java.io.{BufferedWriter, File}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class HomogeneousUnitCategory(SIUnit: String, composites: Array[String], units: Array[HomogeneousUnit]){
  def _units: Seq[HomogeneousUnit] = if (this.units != null) this.units else Nil
}

// ex)
//{"name":"electronvolt", "symbol":"eV", "interval":"Constants.ElementaryCharge",
//   scalePrefixes":true},
case class HomogeneousUnit(
                       name: String,
                       symbol: String,
                       aliases: Array[String],
                       zero: String,
                       interval: String,
                       scalePrefixes: Boolean
                     ){
  import GenerationUtil.toObjectName

  def canonicalizeAndExpandScalePrefixes(prefixes: Seq[ScalePrefix]): Seq[CanonicalizedHomogeneousUnit] = {

    val _zero = GenerationUtil.refineNumber(this.zero)
    val _interval = GenerationUtil.refineNumber(this.interval)
    val _aliases = if (this.aliases != null) this.aliases.toSeq else Nil

    if(scalePrefixes){
      val nm = this.symbol +: _aliases

      // In this case, always name == objectName
      CanonicalizedHomogeneousUnit(this.name, this.name, this.symbol, _aliases, _zero, _interval) +:
        prefixes.map{ p =>
          val al = (p.prefix +: p._aliases).flatMap(ps => nm.map(ns => ps + ns)).tail
          val name = p.name + this.name
          CanonicalizedHomogeneousUnit(name, name, p.prefix + this.symbol, al, _zero, s"""${_interval} * r"${p.scale}"""")
        }
    }else{
      Seq(
        CanonicalizedHomogeneousUnit(this.name, toObjectName(this.name), this.symbol, _aliases, _zero, _interval))
    }
  }
}
case class CanonicalizedHomogeneousUnit(
                                    name: String,
                                    objectName: String,
                                    symbol: String,
                                    aliases: Seq[String],
                                    zero: String,
                                    interval: String)

class HomogeneousUnitDefinitionJson(jsonFile: File, destDir: File, mainDir: File, subpackage: String) 
  extends UnitDefinitionJson(jsonFile, destDir, mainDir, subpackage){

  import GenerationUtil._

  val unitCategoryType: Class[_ >: HomogeneousUnitCategory] = new TypeToken[HomogeneousUnitCategory]() {}.getRawType

  val unitCategory: HomogeneousUnitCategory = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, unitCategoryType).asInstanceOf[HomogeneousUnitCategory]
  }

  val composites: Composites = Composites(Nil, Nil)

  override protected def doGenerate(jsons: Seq[JsonResource]): Unit = {

    IO.writer(this.destFile, "", utf8, append = false) { writer: io.BufferedWriter =>
      val spj = jsons.find(_.isInstanceOf[ScalePrefixJson]).get.asInstanceOf[ScalePrefixJson]
      val units = this.unitCategory._units.flatMap(_.canonicalizeAndExpandScalePrefixes(spj.scalePrefixes))

      writer.write(
        s"""package $packageName
           |
           |import spire.math.Real
           |import spire.math.Fractional
           |import spire.implicits._
           |import ${GenerationUtil.rootPackage}._
           |
           |class $id[A: Fractional](val value: A, val unit: ${id}Unit)
           |    extends HomogeneousQuantity[A, ${id}Unit]
           |
           |trait ${id}Unit extends HomogeneousUnit[${id}Unit]{
           |  override def getSIUnit: ${id}Unit = ${id}UnitObjects.getSIUnit
           |}
           |
           |class Default${id}Unit(val name: String, val symbol: String, val aliases: Seq[String], val zero: Real, val interval: Real)
           |  extends ${id}Unit
           |
           |""".stripMargin)

      generateUnitObjectCode(writer, id, this.unitCategory.SIUnit, units)
      writer.write("\n")
      generateUnitsCode(writer, id, units)
    }
  }

  private def generateUnitObjectCode(
                                      writer: BufferedWriter, id: String, siUnit: String, units: Seq[CanonicalizedHomogeneousUnit]): Unit = {

    writer.write(s"object ${id}UnitObjects{\n")

    if(units.exists(u => u.interval.contains("Constants")))
      writer.write(s"""  import ${GenerationUtil.rootPackage}.unit.Constants\n\n""")

    //***** SI Unit *****
    writer.write(
      s"""
         |  def getSIUnit: ${id}Unit = $siUnit
         |
         |""".stripMargin)

    //***** Unit Objects *****
    units.foreach{ u =>
      val aliases =
        if (u.aliases.isEmpty) "Nil"
        else u.aliases.mkString("Seq(\"", "\", \"", "\")")

      // final case object kelvin extends DefaultTemperatureUnit("kelvin", "K", Nil, r"0", r"1")
      writer.write(
        s"""  final object ${u.objectName} extends Default${id}Unit""" +
          s"""("${u.name}", "${u.symbol}", $aliases, ${u.zero}, ${u.interval})\n""")
    }

    writer.write(
      s"""
         |  def getUnits: Seq[${id}Unit] =
         |    ${units.map(_.objectName).mkString("Seq(", ", ", ")")}
         |}
         |
         |""".stripMargin)
  }

  private def generateUnitsCode(
                                 writer: BufferedWriter, id: String, units: Seq[CanonicalizedHomogeneousUnit]): Unit = {

    writer.write(s"""object ${id}Units{\n""")

    units.foreach { u =>
      val sym = escapeSymbol(u.symbol)

      // def m: LengthUnit = LengthUnitObjects.metre
      writer.write(s"""  def $sym: ${id}Unit = ${id}UnitObjects.${u.objectName}\n""")

      u.aliases.foreach { al =>
        val als = escapeSymbol(al)
        writer.write(s"""  def $als: ${id}Unit = ${id}UnitObjects.${u.objectName}\n""")
      }
    }

    writer.write(
      s"""
         |  def getSIUnit: ${id}Unit = ${id}UnitObjects.getSIUnit
         |  def getUnits: Seq[${id}Unit] = ${id}UnitObjects.getUnits
         |}
         |""".stripMargin)
  }

  def escapeSymbol(s: String): String =
    if (s.matches("""\w+""")) s
    else s"""`$s`"""
}