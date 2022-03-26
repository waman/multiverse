import java.io.{File, BufferedWriter => BW}
import play.api.libs.json.{Json, Reads}

case class HomogeneousUnitdef(description: Option[String],
                              si_unit: String,
                              dimension: Dimension,
                              convertibles: Option[Seq[Convertible]],
                              units: Option[Seq[HomogeneousUnit]],
                              use: Option[Use]) extends Unitdef[HomogeneousUnit]

case class HomogeneousUnit(name: String,
                           symbol: String,
                           aliases: Option[Seq[String]],
                           zero: Option[String],
                           interval: Option[String],
                           description: Option[String]) extends UnitInfo{

  lazy val objectName: String = GenerationUtil.toSnakeCase(this.name)

  def _zero: String = this.zero match {
    case Some(_zero) => _zero
    case _ => "0"
  }

  def _interval: String = this.interval match {
    case Some(_interval) => _interval
    case _ => "1"
  }
}

class HomogeneousUnitdefJson(jsonFile: File, subpackage: String)
  extends UnitdefJsonAdapter[HomogeneousUnitdef, HomogeneousUnit](
    jsonFile, subpackage, UnitType.Homogeneous){

  import GenerationUtil._

  implicit val homogeneousUnitReads: Reads[HomogeneousUnit] = Json.reads[HomogeneousUnit]
  implicit val homogeneousUnitdefReads: Reads[HomogeneousUnitdef] = Json.reads[HomogeneousUnitdef]
  val unitdef: HomogeneousUnitdef = readJson(jsonFile, _.validate[HomogeneousUnitdef])

  override protected def parentQuantityDeclaration: String = s"""HomogeneousQuantity[A, ${id}Unit]"""

  override protected def generateImplementationsOfUnitTrait(writer: BW): Unit = {
    writer.write(
      s"""
         |/** For no alias or user defined units */
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
    val sZero = refineFactor(unit._zero)
    val sInterval = refineFactor(unit._interval)
    unit.aliases match{
      case Some(_) =>
        // final case object kelvin extends DefaultTemperatureUnit("celsius", "°C", Seq("degC", "℃"), r"273.15", r"1")
        writer.write(
          s"""  final case object ${unit.objectName} extends Default${id}Unit""" +
            s"""("${unit.name}", "${unit.symbol}", ${unit.aliasesStr}, $sZero, $sInterval)\n""")

      case _ =>
        // final case object kelvin extends SimpleTemperatureUnit("kelvin", "K", r"0", r"1")
        writer.write(
          s"""  final case object ${unit.objectName} extends Simple${id}Unit""" +
            s"""("${unit.name}", "${unit.symbol}", $sZero, $sInterval)\n""")
    }
  }

  override protected def generateUnits(writer: BW): Unit =
    this.unitdef.units.foreach{ units =>
      writer.write(
        s"""
           |object ${id}Units{
           |""".stripMargin)

      units.foreach { u =>
        val sym = escape(u.symbol)
        val rType = s"${id}Unit"

        // def m: LengthUnit = LengthUnitObjects.metre
        writer.write(s"""  def $sym: $rType = ${id}UnitObjects.${u.objectName}\n""")

        u.aliases.foreach{ aliases =>
          aliases.foreach { al =>
            val als = escape(al)
            writer.write(s"""  def $als: $rType = ${id}UnitObjects.${u.objectName}\n""")
          }
        }
      }

      writer.write("}")
    }
}