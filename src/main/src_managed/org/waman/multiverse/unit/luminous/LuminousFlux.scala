package org.waman.multiverse.unit.luminous

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
    new QuotientUnit[IlluminanceUnit, LuminousFluxUnit, AreaUnit](LuminousFluxUnit.this, areaUnit) with IlluminanceUnit

}

object LuminousFluxUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](J -> 1).withDefaultValue(0)

  def getSIUnit: LuminousFluxUnit = LuminousFluxUnitObjects.lumen

import LuminousFluxUnitObjects._
  def getUnits: Seq[LuminousFluxUnit] =
    Seq(lumen, yoctolumen, zeptolumen, attolumen, femtolumen, picolumen, nanolumen, microlumen, millilumen, centilumen, decilumen, decalumen, hectolumen, kilolumen, megalumen, gigalumen, teralumen, petalumen, exalumen, zettalumen, yottalumen)
}



class DefaultLuminousFluxUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LuminousFluxUnit

object LuminousFluxUnitObjects{

  final object lumen extends DefaultLuminousFluxUnit("lumen", "lm", Nil, r"1")
  final object yoctolumen extends DefaultLuminousFluxUnit("yoctolumen", "ylm", Nil, r"1" * r"1e-24")
  final object zeptolumen extends DefaultLuminousFluxUnit("zeptolumen", "zlm", Nil, r"1" * r"1e-21")
  final object attolumen extends DefaultLuminousFluxUnit("attolumen", "alm", Nil, r"1" * r"1e-18")
  final object femtolumen extends DefaultLuminousFluxUnit("femtolumen", "flm", Nil, r"1" * r"1e-15")
  final object picolumen extends DefaultLuminousFluxUnit("picolumen", "plm", Nil, r"1" * r"1e-12")
  final object nanolumen extends DefaultLuminousFluxUnit("nanolumen", "nlm", Nil, r"1" * r"1e-9")
  final object microlumen extends DefaultLuminousFluxUnit("microlumen", "μlm", Seq("mclm"), r"1" * r"1e-6")
  final object millilumen extends DefaultLuminousFluxUnit("millilumen", "mlm", Nil, r"1" * r"1e-3")
  final object centilumen extends DefaultLuminousFluxUnit("centilumen", "clm", Nil, r"1" * r"1e-2")
  final object decilumen extends DefaultLuminousFluxUnit("decilumen", "dlm", Nil, r"1" * r"1e-1")
  final object decalumen extends DefaultLuminousFluxUnit("decalumen", "dalm", Nil, r"1" * r"1e1")
  final object hectolumen extends DefaultLuminousFluxUnit("hectolumen", "hlm", Nil, r"1" * r"1e2")
  final object kilolumen extends DefaultLuminousFluxUnit("kilolumen", "klm", Seq("Klm"), r"1" * r"1e3")
  final object megalumen extends DefaultLuminousFluxUnit("megalumen", "Mlm", Nil, r"1" * r"1e6")
  final object gigalumen extends DefaultLuminousFluxUnit("gigalumen", "Glm", Nil, r"1" * r"1e9")
  final object teralumen extends DefaultLuminousFluxUnit("teralumen", "Tlm", Nil, r"1" * r"1e12")
  final object petalumen extends DefaultLuminousFluxUnit("petalumen", "Plm", Nil, r"1" * r"1e15")
  final object exalumen extends DefaultLuminousFluxUnit("exalumen", "Elm", Nil, r"1" * r"1e18")
  final object zettalumen extends DefaultLuminousFluxUnit("zettalumen", "Zlm", Nil, r"1" * r"1e21")
  final object yottalumen extends DefaultLuminousFluxUnit("yottalumen", "Ylm", Nil, r"1" * r"1e24")
}

object LuminousFluxUnits{
  def lm: LuminousFluxUnit = LuminousFluxUnitObjects.lumen
  def ylm: LuminousFluxUnit = LuminousFluxUnitObjects.yoctolumen
  def zlm: LuminousFluxUnit = LuminousFluxUnitObjects.zeptolumen
  def alm: LuminousFluxUnit = LuminousFluxUnitObjects.attolumen
  def flm: LuminousFluxUnit = LuminousFluxUnitObjects.femtolumen
  def plm: LuminousFluxUnit = LuminousFluxUnitObjects.picolumen
  def nlm: LuminousFluxUnit = LuminousFluxUnitObjects.nanolumen
  def μlm: LuminousFluxUnit = LuminousFluxUnitObjects.microlumen
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