package org.waman.multiverse.unit.defs.photometry

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._

class LuminousFlux[A: Fractional](val value: A, val unit: LuminousFluxUnit)
    extends LinearQuantity[LuminousFlux[A], A, LuminousFluxUnit] {

  override protected def newQuantity(value: A, unit: LuminousFluxUnit): LuminousFlux[A] = new LuminousFlux(value, unit)

  def /(area: Area[A]): Illuminance[A] = new Illuminance(this.value / area.value, this.unit / area.unit)
}

/** None */
trait LuminousFluxUnit extends LinearUnit[LuminousFluxUnit]{

  override def getSIUnit: LuminousFluxUnit = LuminousFluxUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = LuminousFluxUnit.dimension

  def /(areaUnit: AreaUnit): IlluminanceUnit =
    new QuotientUnit[IlluminanceUnit, LuminousFluxUnit, AreaUnit](LuminousFluxUnit.this, areaUnit) with IlluminanceUnit
}

object LuminousFluxUnit extends UnitInfo[LuminousFluxUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](J -> 1).withDefaultValue(0)

  def getSIUnit: LuminousFluxUnit = LuminousFluxUnitObjects.lumen

  import LuminousFluxUnitObjects._

  def getUnits: Seq[LuminousFluxUnit] =
    Seq(lumen, yoctolumen, zeptolumen, attolumen, femtolumen, picolumen, nanolumen, microlumen, millilumen, centilumen, decilumen, decalumen, hectolumen, kilolumen, megalumen, gigalumen, teralumen, petalumen, exalumen, zettalumen, yottalumen)
}


/** For no aliase or user defined units */
class SimpleLuminousFluxUnit(val name: String, val symbol: String, val interval: Real) extends LuminousFluxUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultLuminousFluxUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LuminousFluxUnit
  
object LuminousFluxUnitObjects{

  final case object lumen extends SimpleLuminousFluxUnit("lumen", "lm", 1)
  final case object yoctolumen extends SimpleLuminousFluxUnit("yoctolumen", "ylm", r"1e-24")
  final case object zeptolumen extends SimpleLuminousFluxUnit("zeptolumen", "zlm", r"1e-21")
  final case object attolumen extends SimpleLuminousFluxUnit("attolumen", "alm", r"1e-18")
  final case object femtolumen extends SimpleLuminousFluxUnit("femtolumen", "flm", r"1e-15")
  final case object picolumen extends SimpleLuminousFluxUnit("picolumen", "plm", r"1e-12")
  final case object nanolumen extends SimpleLuminousFluxUnit("nanolumen", "nlm", r"1e-9")
  final case object microlumen extends DefaultLuminousFluxUnit("microlumen", "μlm", Seq("mclm"), r"1e-6")
  final case object millilumen extends SimpleLuminousFluxUnit("millilumen", "mlm", r"1e-3")
  final case object centilumen extends SimpleLuminousFluxUnit("centilumen", "clm", r"1e-2")
  final case object decilumen extends SimpleLuminousFluxUnit("decilumen", "dlm", r"1e-1")
  final case object decalumen extends SimpleLuminousFluxUnit("decalumen", "dalm", r"1e1")
  final case object hectolumen extends SimpleLuminousFluxUnit("hectolumen", "hlm", r"1e2")
  final case object kilolumen extends DefaultLuminousFluxUnit("kilolumen", "klm", Seq("Klm"), r"1e3")
  final case object megalumen extends SimpleLuminousFluxUnit("megalumen", "Mlm", r"1e6")
  final case object gigalumen extends SimpleLuminousFluxUnit("gigalumen", "Glm", r"1e9")
  final case object teralumen extends SimpleLuminousFluxUnit("teralumen", "Tlm", r"1e12")
  final case object petalumen extends SimpleLuminousFluxUnit("petalumen", "Plm", r"1e15")
  final case object exalumen extends SimpleLuminousFluxUnit("exalumen", "Elm", r"1e18")
  final case object zettalumen extends SimpleLuminousFluxUnit("zettalumen", "Zlm", r"1e21")
  final case object yottalumen extends SimpleLuminousFluxUnit("yottalumen", "Ylm", r"1e24")
}


object LuminousFluxUnits{

  /** lumen */
  def lm: LuminousFluxUnit = LuminousFluxUnitObjects.lumen
  /** yoctolumen */
  def ylm: LuminousFluxUnit = LuminousFluxUnitObjects.yoctolumen
  /** zeptolumen */
  def zlm: LuminousFluxUnit = LuminousFluxUnitObjects.zeptolumen
  /** attolumen */
  def alm: LuminousFluxUnit = LuminousFluxUnitObjects.attolumen
  /** femtolumen */
  def flm: LuminousFluxUnit = LuminousFluxUnitObjects.femtolumen
  /** picolumen */
  def plm: LuminousFluxUnit = LuminousFluxUnitObjects.picolumen
  /** nanolumen */
  def nlm: LuminousFluxUnit = LuminousFluxUnitObjects.nanolumen
  /** microlumen */
  def μlm: LuminousFluxUnit = LuminousFluxUnitObjects.microlumen
  /** microlumen */
  def mclm: LuminousFluxUnit = LuminousFluxUnitObjects.microlumen
  /** millilumen */
  def mlm: LuminousFluxUnit = LuminousFluxUnitObjects.millilumen
  /** centilumen */
  def clm: LuminousFluxUnit = LuminousFluxUnitObjects.centilumen
  /** decilumen */
  def dlm: LuminousFluxUnit = LuminousFluxUnitObjects.decilumen
  /** decalumen */
  def dalm: LuminousFluxUnit = LuminousFluxUnitObjects.decalumen
  /** hectolumen */
  def hlm: LuminousFluxUnit = LuminousFluxUnitObjects.hectolumen
  /** kilolumen */
  def klm: LuminousFluxUnit = LuminousFluxUnitObjects.kilolumen
  /** kilolumen */
  def Klm: LuminousFluxUnit = LuminousFluxUnitObjects.kilolumen
  /** megalumen */
  def Mlm: LuminousFluxUnit = LuminousFluxUnitObjects.megalumen
  /** gigalumen */
  def Glm: LuminousFluxUnit = LuminousFluxUnitObjects.gigalumen
  /** teralumen */
  def Tlm: LuminousFluxUnit = LuminousFluxUnitObjects.teralumen
  /** petalumen */
  def Plm: LuminousFluxUnit = LuminousFluxUnitObjects.petalumen
  /** exalumen */
  def Elm: LuminousFluxUnit = LuminousFluxUnitObjects.exalumen
  /** zettalumen */
  def Zlm: LuminousFluxUnit = LuminousFluxUnitObjects.zettalumen
  /** yottalumen */
  def Ylm: LuminousFluxUnit = LuminousFluxUnitObjects.yottalumen
}