import java.io.{BufferedWriter => BW, File}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class HomogeneousUnitCategory(SIUnit: String, dimension: Dimension, composites: Array[String],
                                                     convertibles: Array[Convertible], units: Array[RawHomogeneousUnit])
    extends UnitCategory[RawHomogeneousUnit, HomogeneousUnit]

case class RawHomogeneousUnit(name: String, symbol: String, aliases: Array[String],
                              zero: String, interval: String, scalePrefixes: Boolean, excludePrefixes: Array[String])
  extends RawUnitInfo[HomogeneousUnit]{

  import GenerationUtil._

  override def expandScalePrefixes(jsons: JsonResources): Seq[HomogeneousUnit] = {
    val prefixes = jsons.scalePrefixJson.scalePrefixes

    val _zero = if (this.zero != null) refineNumbers(this.zero) else "0"
    val _interval = if (this.interval != null) refineNumbers(this.interval) else "1"

    if(scalePrefixes){
      val nm = this.symbol +: _aliases

      // In this case, always name == objectName
      HomogeneousUnit(this.name, this.name, this.symbol, _aliases, _zero, _interval) +:
        prefixes.map{ p =>
          val al = (p.prefix +: p._aliases).flatMap(ps => nm.map(ns => ps + ns)).tail
          val name = p.name + this.name
          HomogeneousUnit(name, name, p.prefix + this.symbol, al, _zero, s"""${_interval} * r"${p.scale}"""")
        }
    }else{
      Seq(
        HomogeneousUnit(this.name, toObjectName(this.name), this.symbol, _aliases, _zero, _interval))
    }
  }
}

case class HomogeneousUnit(name: String, objectName: String, symbol: String, aliases: Seq[String],
                           zero: String, interval: String) extends UnitInfo{
  override def attributes: Seq[Attribute] = Nil
  override def baseUnit: String = null
}

class HomogeneousUnitDefinitionJson(jsonFile: File, destDir: File, subpackage: String)
  extends UnitDefinitionJsonAdapter[HomogeneousUnitCategory, RawHomogeneousUnit, HomogeneousUnit, Unit](
    "Homogeneous", jsonFile, destDir, subpackage){

  import GenerationUtil._

  val unitCategoryType: Class[_ >: HomogeneousUnitCategory] = new TypeToken[HomogeneousUnitCategory]() {}.getRawType

  val unitCategory: HomogeneousUnitCategory = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, unitCategoryType).asInstanceOf[HomogeneousUnitCategory]
  }

  override protected def getUnits(jsons: JsonResources): Seq[HomogeneousUnit] = {
    this.unitCategory._units.flatMap(_.expandScalePrefixes(jsons))
  }

  override protected def createOptions(jsons: JsonResources): Unit = ()

  override protected def parentQuantityDecl: String = s"""HomogeneousQuantity[A, ${id}Unit]"""

  override protected def generateImplsOfUnitTrait(writer: BW): Unit = {
    writer.write(
      s"""/** For user defined units */
         |class Simple${id}Unit(val name: String, val symbol: String, val zero: Real, val interval: Real) extends ${id}Unit {
         |  override def aliases: Seq[String] = Nil
         |}
         |
         |class Default${id}Unit(val name: String, val symbol: String, val aliases: Seq[String], val zero: Real, val interval: Real)
         |  extends ${id}Unit
         |
         |""".stripMargin)
  }

  override protected def generateUnitCaseObject(writer: BW, unit: HomogeneousUnit): Unit = {
    val aliases =
      if (unit.aliases.isEmpty) "Nil"
      else unit.aliases.mkString("Seq(\"", "\", \"", "\")")

    // final case object kelvin extends DefaultTemperatureUnit("kelvin", "K", Nil, r"0", r"1")
    writer.write(
      s"""  final case object ${unit.objectName} extends Default${id}Unit""" +
        s"""("${unit.name}", "${unit.symbol}", $aliases, ${unit.zero}, ${unit.interval})\n""")
  }
}