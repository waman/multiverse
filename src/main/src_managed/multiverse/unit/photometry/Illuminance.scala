package multiverse.unit.photometry

import spire.math.Real
import spire.math.Fractional

import multiverse._


class Illuminance[A: Fractional](val value: A, val unit: IlluminanceUnit)
    extends LinearQuantity[Illuminance[A], A, IlluminanceUnit] {

  override protected def newQuantity(value: A, unit: IlluminanceUnit): Illuminance[A] = new Illuminance(value, unit)
}

trait IlluminanceUnit extends LinearUnit[IlluminanceUnit]{

  override def getSIUnit: IlluminanceUnit = IlluminanceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = IlluminanceUnit.dimension
}

object IlluminanceUnit extends UnitInfo[IlluminanceUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](J -> 1, L -> -2).withDefaultValue(0)

  def getSIUnit: IlluminanceUnit = IlluminanceUnitObjects.lux

  import IlluminanceUnitObjects._
  def getUnits: Seq[IlluminanceUnit] =
    Seq(lux, yoctolux, zeptolux, attolux, femtolux, picolux, nanolux, microlux, millilux, centilux, decilux, decalux, hectolux, kilolux, megalux, gigalux, teralux, petalux, exalux, zettalux, yottalux, phot, foot_candle)
}

/** For no aliase or user defined units */
class SimpleIlluminanceUnit(val name: String, val symbol: String, val interval: Real) extends IlluminanceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultIlluminanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends IlluminanceUnit

object IlluminanceUnitObjects{

  import spire.implicits._

  import multiverse.unit.photometry.LuminousIntensityUnitObjects._
  import multiverse.unit.basic.AreaUnitObjects._

  final case object lux extends SimpleIlluminanceUnit("lux", "lx", 1)
  final case object yoctolux extends SimpleIlluminanceUnit("yoctolux", "ylx", r"1e-24")
  final case object zeptolux extends SimpleIlluminanceUnit("zeptolux", "zlx", r"1e-21")
  final case object attolux extends SimpleIlluminanceUnit("attolux", "alx", r"1e-18")
  final case object femtolux extends SimpleIlluminanceUnit("femtolux", "flx", r"1e-15")
  final case object picolux extends SimpleIlluminanceUnit("picolux", "plx", r"1e-12")
  final case object nanolux extends SimpleIlluminanceUnit("nanolux", "nlx", r"1e-9")
  final case object microlux extends DefaultIlluminanceUnit("microlux", "μlx", Seq("mclx"), r"1e-6")
  final case object millilux extends SimpleIlluminanceUnit("millilux", "mlx", r"1e-3")
  final case object centilux extends SimpleIlluminanceUnit("centilux", "clx", r"1e-2")
  final case object decilux extends SimpleIlluminanceUnit("decilux", "dlx", r"1e-1")
  final case object decalux extends SimpleIlluminanceUnit("decalux", "dalx", r"1e1")
  final case object hectolux extends SimpleIlluminanceUnit("hectolux", "hlx", r"1e2")
  final case object kilolux extends DefaultIlluminanceUnit("kilolux", "klx", Seq("Klx"), r"1e3")
  final case object megalux extends SimpleIlluminanceUnit("megalux", "Mlx", r"1e6")
  final case object gigalux extends SimpleIlluminanceUnit("gigalux", "Glx", r"1e9")
  final case object teralux extends SimpleIlluminanceUnit("teralux", "Tlx", r"1e12")
  final case object petalux extends SimpleIlluminanceUnit("petalux", "Plx", r"1e15")
  final case object exalux extends SimpleIlluminanceUnit("exalux", "Elx", r"1e18")
  final case object zettalux extends SimpleIlluminanceUnit("zettalux", "Zlx", r"1e21")
  final case object yottalux extends SimpleIlluminanceUnit("yottalux", "Ylx", r"1e24")
  final case object phot extends SimpleIlluminanceUnit("phot", "ph", r"1e4")
  final case object foot_candle extends SimpleIlluminanceUnit("foot candle", "fc", r"1" * candela.interval / square_foot.interval)
}

object IlluminanceUnits{

  def lx: IlluminanceUnit = IlluminanceUnitObjects.lux
  def ylx: IlluminanceUnit = IlluminanceUnitObjects.yoctolux
  def zlx: IlluminanceUnit = IlluminanceUnitObjects.zeptolux
  def alx: IlluminanceUnit = IlluminanceUnitObjects.attolux
  def flx: IlluminanceUnit = IlluminanceUnitObjects.femtolux
  def plx: IlluminanceUnit = IlluminanceUnitObjects.picolux
  def nlx: IlluminanceUnit = IlluminanceUnitObjects.nanolux
  def `μlx`: IlluminanceUnit = IlluminanceUnitObjects.microlux
  def mclx: IlluminanceUnit = IlluminanceUnitObjects.microlux
  def mlx: IlluminanceUnit = IlluminanceUnitObjects.millilux
  def clx: IlluminanceUnit = IlluminanceUnitObjects.centilux
  def dlx: IlluminanceUnit = IlluminanceUnitObjects.decilux
  def dalx: IlluminanceUnit = IlluminanceUnitObjects.decalux
  def hlx: IlluminanceUnit = IlluminanceUnitObjects.hectolux
  def klx: IlluminanceUnit = IlluminanceUnitObjects.kilolux
  def Klx: IlluminanceUnit = IlluminanceUnitObjects.kilolux
  def Mlx: IlluminanceUnit = IlluminanceUnitObjects.megalux
  def Glx: IlluminanceUnit = IlluminanceUnitObjects.gigalux
  def Tlx: IlluminanceUnit = IlluminanceUnitObjects.teralux
  def Plx: IlluminanceUnit = IlluminanceUnitObjects.petalux
  def Elx: IlluminanceUnit = IlluminanceUnitObjects.exalux
  def Zlx: IlluminanceUnit = IlluminanceUnitObjects.zettalux
  def Ylx: IlluminanceUnit = IlluminanceUnitObjects.yottalux
  def ph: IlluminanceUnit = IlluminanceUnitObjects.phot
  def fc: IlluminanceUnit = IlluminanceUnitObjects.foot_candle
}