package org.waman.multiverse.unit.optics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.basic.Area
import org.waman.multiverse.unit.basic.AreaUnit

class LuminousFlux[A: Fractional](val value: A, val unit: LuminousFluxUnit)
    extends LinearQuantity[LuminousFlux[A], A, LuminousFluxUnit] {

  override protected def newQuantity(value: A, unit: LuminousFluxUnit): LuminousFlux[A] = new LuminousFlux(value, unit)

  def /(area: Area[A]): Illuminance[A] = new Illuminance(this.value / area.value, this.unit / area.unit)

}

trait LuminousFluxUnit extends LinearUnit[LuminousFluxUnit]{

  override def getSIUnit: LuminousFluxUnit = LuminousFluxUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = LuminousFluxUnit.dimension

  def /(areaUnit: AreaUnit): IlluminanceUnit =
    new AbstractQuotientUnit[IlluminanceUnit, LuminousFluxUnit, AreaUnit](LuminousFluxUnit.this, areaUnit) with IlluminanceUnit

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

/** For user defined units */
class SimpleLuminousFluxUnit(val name: String, val symbol: String, val interval: Real) extends LuminousFluxUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultLuminousFluxUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LuminousFluxUnit

object LuminousFluxUnitObjects{

  final case object lumen extends DefaultLuminousFluxUnit("lumen", "lm", Nil, 1)
  final case object yoctolumen extends DefaultLuminousFluxUnit("yoctolumen", "ylm", Nil, r"1e-24")
  final case object zeptolumen extends DefaultLuminousFluxUnit("zeptolumen", "zlm", Nil, r"1e-21")
  final case object attolumen extends DefaultLuminousFluxUnit("attolumen", "alm", Nil, r"1e-18")
  final case object femtolumen extends DefaultLuminousFluxUnit("femtolumen", "flm", Nil, r"1e-15")
  final case object picolumen extends DefaultLuminousFluxUnit("picolumen", "plm", Nil, r"1e-12")
  final case object nanolumen extends DefaultLuminousFluxUnit("nanolumen", "nlm", Nil, r"1e-9")
  final case object microlumen extends DefaultLuminousFluxUnit("microlumen", "μlm", Seq("mclm"), r"1e-6")
  final case object millilumen extends DefaultLuminousFluxUnit("millilumen", "mlm", Nil, r"1e-3")
  final case object centilumen extends DefaultLuminousFluxUnit("centilumen", "clm", Nil, r"1e-2")
  final case object decilumen extends DefaultLuminousFluxUnit("decilumen", "dlm", Nil, r"1e-1")
  final case object decalumen extends DefaultLuminousFluxUnit("decalumen", "dalm", Nil, r"1e1")
  final case object hectolumen extends DefaultLuminousFluxUnit("hectolumen", "hlm", Nil, r"1e2")
  final case object kilolumen extends DefaultLuminousFluxUnit("kilolumen", "klm", Seq("Klm"), r"1e3")
  final case object megalumen extends DefaultLuminousFluxUnit("megalumen", "Mlm", Nil, r"1e6")
  final case object gigalumen extends DefaultLuminousFluxUnit("gigalumen", "Glm", Nil, r"1e9")
  final case object teralumen extends DefaultLuminousFluxUnit("teralumen", "Tlm", Nil, r"1e12")
  final case object petalumen extends DefaultLuminousFluxUnit("petalumen", "Plm", Nil, r"1e15")
  final case object exalumen extends DefaultLuminousFluxUnit("exalumen", "Elm", Nil, r"1e18")
  final case object zettalumen extends DefaultLuminousFluxUnit("zettalumen", "Zlm", Nil, r"1e21")
  final case object yottalumen extends DefaultLuminousFluxUnit("yottalumen", "Ylm", Nil, r"1e24")
}

object LuminousFluxUnits{
  def lm: LuminousFluxUnit = LuminousFluxUnitObjects.lumen
  def ylm: LuminousFluxUnit = LuminousFluxUnitObjects.yoctolumen
  def zlm: LuminousFluxUnit = LuminousFluxUnitObjects.zeptolumen
  def alm: LuminousFluxUnit = LuminousFluxUnitObjects.attolumen
  def flm: LuminousFluxUnit = LuminousFluxUnitObjects.femtolumen
  def plm: LuminousFluxUnit = LuminousFluxUnitObjects.picolumen
  def nlm: LuminousFluxUnit = LuminousFluxUnitObjects.nanolumen
  def `μlm`: LuminousFluxUnit = LuminousFluxUnitObjects.microlumen
  def mclm: LuminousFluxUnit = LuminousFluxUnitObjects.microlumen
  def mlm: LuminousFluxUnit = LuminousFluxUnitObjects.millilumen
  def clm: LuminousFluxUnit = LuminousFluxUnitObjects.centilumen
  def dlm: LuminousFluxUnit = LuminousFluxUnitObjects.decilumen
  def dalm: LuminousFluxUnit = LuminousFluxUnitObjects.decalumen
  def hlm: LuminousFluxUnit = LuminousFluxUnitObjects.hectolumen
  def klm: LuminousFluxUnit = LuminousFluxUnitObjects.kilolumen
  def Klm: LuminousFluxUnit = LuminousFluxUnitObjects.kilolumen
  def Mlm: LuminousFluxUnit = LuminousFluxUnitObjects.megalumen
  def Glm: LuminousFluxUnit = LuminousFluxUnitObjects.gigalumen
  def Tlm: LuminousFluxUnit = LuminousFluxUnitObjects.teralumen
  def Plm: LuminousFluxUnit = LuminousFluxUnitObjects.petalumen
  def Elm: LuminousFluxUnit = LuminousFluxUnitObjects.exalumen
  def Zlm: LuminousFluxUnit = LuminousFluxUnitObjects.zettalumen
  def Ylm: LuminousFluxUnit = LuminousFluxUnitObjects.yottalumen
}