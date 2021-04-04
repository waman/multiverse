import java.io.{BufferedWriter => BW, File}

import com.google.gson.reflect.TypeToken
import sbt.io.IO

case class HomogeneousUnitCategory(description: String,
                                   SIUnit: String,
                                   dimension: Dimension,
                                   composites: Array[String],
                                   convertibles: Array[Convertible],
                                   units: Array[HomogeneousUnit],
                                   use: Use) extends UnitCategory[HomogeneousUnit]

case class HomogeneousUnit(name: String,
                           symbol: String,
                           aliases: Array[String],
                           zero: String,
                           interval: String,
                           description: String) extends UnitInfo{

  lazy val objectName: String = GenerationUtil.toObjectName(this.name)
  def _zero: String = if (this.zero != null) this.zero else "0"
  def _interval: String = if (this.interval != null) this.interval else "1"
}

class HomogeneousUnitdefJson(jsonFile: File, subpackage: String)
  extends UnitdefJsonAdapter[HomogeneousUnitCategory, HomogeneousUnit](
    jsonFile, subpackage, UnitType.Homogeneous){

  import GenerationUtil._

  val unitCategoryType: Class[_ >: HomogeneousUnitCategory] = new TypeToken[HomogeneousUnitCategory]() {}.getRawType

  val unitCategory: HomogeneousUnitCategory = IO.reader(jsonFile, utf8) { reader =>
    gson.fromJson(reader, unitCategoryType).asInstanceOf[HomogeneousUnitCategory]
  }

  override protected def parentQuantityDeclaration: String = s"""HomogeneousQuantity[A, ${id}Unit]"""

  override protected def generateImplsOfUnitTrait(writer: BW): Unit = {
    writer.write(
      s"""
         |/** For no aliase or user defined units */
         |class Simple${id}Unit(val name: String, val symbol: String, val zero: Real, val interval: Real) extends ${id}Unit {
         |  override def aliases: Seq[String] = Nil
         |}
         |
         |/** For units which has aliases */
         |class Default${id}Unit(val name: String, val symbol: String, val aliases: Seq[String], val zero: Real, val interval: Real)
         |  extends ${id}Unit
         |""".stripMargin)
  }

  override protected def generateUnitCaseObject(writer: BW, unit: HomogeneousUnit): Unit = {
    val sZero = refineNumbers(unit._zero)
    val sInterval = refineNumbers(unit._interval)
    if (unit._aliases.isEmpty){
      // final case object kelvin extends SimpleTemperatureUnit("kelvin", "K", r"0", r"1")
      writer.write(
        s"""  final case object ${unit.objectName} extends Simple${id}Unit""" +
          s"""("${unit.name}", "${unit.symbol}", $sZero, $sInterval)\n""")

    } else {
      val aliases = unit._aliases.mkString("Seq(\"", "\", \"", "\")")
      // final case object kelvin extends DefaultTemperatureUnit("celsius", "°C", Seq("degC", "℃"), r"273.15", r"1")
      writer.write(
        s"""  final case object ${unit.objectName} extends Default${id}Unit""" +
          s"""("${unit.name}", "${unit.symbol}", ${unit.aliasesStr}, $sZero, $sInterval)\n""")
    }
  }

  override protected def generateUnits(writer: BW): Unit = {
    val units = this.unitCategory._units

    writer.write(
      s"""
         |object ${id}Units{
         |""".stripMargin)

    units.foreach { u =>
      val sym = escape(u.symbol)
      val rType = s"${id}Unit"

      // def m: LengthUnit = LengthUnitObjects.metre
      writer.write(s"""  def $sym: $rType = ${id}UnitObjects.${u.objectName}\n""")

      u._aliases.foreach { al =>
        val als = escape(al)
        writer.write(s"""  def $als: $rType = ${id}UnitObjects.${u.objectName}\n""")
      }
    }

    writer.write("}")
  }
}