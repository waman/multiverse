package org.waman.multiverse.unit.defs.photo

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._

class Illuminance[A: Fractional](val value: A, val unit: IlluminanceUnit)
    extends LinearQuantity[Illuminance[A], A, IlluminanceUnit] {

  override protected def newQuantity(value: A, unit: IlluminanceUnit): Illuminance[A] = new Illuminance(value, unit)
}

/** None */
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


/** For no alias or user defined units */
class SimpleIlluminanceUnit(val name: String, val symbol: String, val interval: Real) extends IlluminanceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultIlluminanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends IlluminanceUnit
  
object IlluminanceUnitObjects{

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
  final case object foot_candle extends SimpleIlluminanceUnit("foot candle", "fc", r"1" * LuminousIntensityUnitObjects.candela.interval / AreaUnitObjects.square_foot.interval)
}


object IlluminanceUnits{

  /** lux */
  def lx: IlluminanceUnit = IlluminanceUnitObjects.lux
  /** yoctolux */
  def ylx: IlluminanceUnit = IlluminanceUnitObjects.yoctolux
  /** zeptolux */
  def zlx: IlluminanceUnit = IlluminanceUnitObjects.zeptolux
  /** attolux */
  def alx: IlluminanceUnit = IlluminanceUnitObjects.attolux
  /** femtolux */
  def flx: IlluminanceUnit = IlluminanceUnitObjects.femtolux
  /** picolux */
  def plx: IlluminanceUnit = IlluminanceUnitObjects.picolux
  /** nanolux */
  def nlx: IlluminanceUnit = IlluminanceUnitObjects.nanolux
  /** microlux */
  def μlx: IlluminanceUnit = IlluminanceUnitObjects.microlux
  /** microlux */
  def mclx: IlluminanceUnit = IlluminanceUnitObjects.microlux
  /** millilux */
  def mlx: IlluminanceUnit = IlluminanceUnitObjects.millilux
  /** centilux */
  def clx: IlluminanceUnit = IlluminanceUnitObjects.centilux
  /** decilux */
  def dlx: IlluminanceUnit = IlluminanceUnitObjects.decilux
  /** decalux */
  def dalx: IlluminanceUnit = IlluminanceUnitObjects.decalux
  /** hectolux */
  def hlx: IlluminanceUnit = IlluminanceUnitObjects.hectolux
  /** kilolux */
  def klx: IlluminanceUnit = IlluminanceUnitObjects.kilolux
  /** kilolux */
  def Klx: IlluminanceUnit = IlluminanceUnitObjects.kilolux
  /** megalux */
  def Mlx: IlluminanceUnit = IlluminanceUnitObjects.megalux
  /** gigalux */
  def Glx: IlluminanceUnit = IlluminanceUnitObjects.gigalux
  /** teralux */
  def Tlx: IlluminanceUnit = IlluminanceUnitObjects.teralux
  /** petalux */
  def Plx: IlluminanceUnit = IlluminanceUnitObjects.petalux
  /** exalux */
  def Elx: IlluminanceUnit = IlluminanceUnitObjects.exalux
  /** zettalux */
  def Zlx: IlluminanceUnit = IlluminanceUnitObjects.zettalux
  /** yottalux */
  def Ylx: IlluminanceUnit = IlluminanceUnitObjects.yottalux
  /** phot */
  def ph: IlluminanceUnit = IlluminanceUnitObjects.phot
  /** foot candle */
  def fc: IlluminanceUnit = IlluminanceUnitObjects.foot_candle
}