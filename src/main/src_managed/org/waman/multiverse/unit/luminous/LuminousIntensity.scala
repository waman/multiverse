package org.waman.multiverse.unit.luminous

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.basic.Area
import org.waman.multiverse.unit.basic.AreaUnit

class LuminousIntensity[A: Fractional](val value: A, val unit: LuminousIntensityUnit)
    extends LinearQuantity[LuminousIntensity[A], A, LuminousIntensityUnit] {

  override protected def newQuantity(value: A, unit: LuminousIntensityUnit): LuminousIntensity[A] = new LuminousIntensity(value, unit)

  def /(area: Area[A]): Luminance[A] = new Luminance(this.value / area.value, this.unit / area.unit)

}

trait LuminousIntensityUnit extends LinearUnit[LuminousIntensityUnit]{

  override def getSIUnit: LuminousIntensityUnit = LuminousIntensityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = LuminousIntensityUnit.dimension

  def /(areaUnit: AreaUnit): LuminanceUnit =
    new AbstractQuotientUnit[LuminanceUnit, LuminousIntensityUnit, AreaUnit](LuminousIntensityUnit.this, areaUnit) with LuminanceUnit

}

/** For user defined units */
class SimpleLuminousIntensityUnit(val name: String, val symbol: String, val interval: Real) extends LuminousIntensityUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultLuminousIntensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LuminousIntensityUnit

object LuminousIntensityUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](J -> 1).withDefaultValue(0)

  def getSIUnit: LuminousIntensityUnit = LuminousIntensityUnitObjects.candela

  import LuminousIntensityUnitObjects._
  def getUnits: Seq[LuminousIntensityUnit] =
    Seq(candela, yoctocandela, zeptocandela, attocandela, femtocandela, picocandela, nanocandela, microcandela, millicandela, centicandela, decicandela, decacandela, hectocandela, kilocandela, megacandela, gigacandela, teracandela, petacandela, exacandela, zettacandela, yottacandela)
}

object LuminousIntensityUnitObjects{

  final case object candela extends DefaultLuminousIntensityUnit("candela", "cd", Nil, 1)
  final case object yoctocandela extends DefaultLuminousIntensityUnit("yoctocandela", "ycd", Nil, r"1e-24")
  final case object zeptocandela extends DefaultLuminousIntensityUnit("zeptocandela", "zcd", Nil, r"1e-21")
  final case object attocandela extends DefaultLuminousIntensityUnit("attocandela", "acd", Nil, r"1e-18")
  final case object femtocandela extends DefaultLuminousIntensityUnit("femtocandela", "fcd", Nil, r"1e-15")
  final case object picocandela extends DefaultLuminousIntensityUnit("picocandela", "pcd", Nil, r"1e-12")
  final case object nanocandela extends DefaultLuminousIntensityUnit("nanocandela", "ncd", Nil, r"1e-9")
  final case object microcandela extends DefaultLuminousIntensityUnit("microcandela", "μcd", Seq("mccd"), r"1e-6")
  final case object millicandela extends DefaultLuminousIntensityUnit("millicandela", "mcd", Nil, r"1e-3")
  final case object centicandela extends DefaultLuminousIntensityUnit("centicandela", "ccd", Nil, r"1e-2")
  final case object decicandela extends DefaultLuminousIntensityUnit("decicandela", "dcd", Nil, r"1e-1")
  final case object decacandela extends DefaultLuminousIntensityUnit("decacandela", "dacd", Nil, r"1e1")
  final case object hectocandela extends DefaultLuminousIntensityUnit("hectocandela", "hcd", Nil, r"1e2")
  final case object kilocandela extends DefaultLuminousIntensityUnit("kilocandela", "kcd", Seq("Kcd"), r"1e3")
  final case object megacandela extends DefaultLuminousIntensityUnit("megacandela", "Mcd", Nil, r"1e6")
  final case object gigacandela extends DefaultLuminousIntensityUnit("gigacandela", "Gcd", Nil, r"1e9")
  final case object teracandela extends DefaultLuminousIntensityUnit("teracandela", "Tcd", Nil, r"1e12")
  final case object petacandela extends DefaultLuminousIntensityUnit("petacandela", "Pcd", Nil, r"1e15")
  final case object exacandela extends DefaultLuminousIntensityUnit("exacandela", "Ecd", Nil, r"1e18")
  final case object zettacandela extends DefaultLuminousIntensityUnit("zettacandela", "Zcd", Nil, r"1e21")
  final case object yottacandela extends DefaultLuminousIntensityUnit("yottacandela", "Ycd", Nil, r"1e24")
}

object LuminousIntensityUnits{
  def cd: LuminousIntensityUnit = LuminousIntensityUnitObjects.candela
  def ycd: LuminousIntensityUnit = LuminousIntensityUnitObjects.yoctocandela
  def zcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.zeptocandela
  def acd: LuminousIntensityUnit = LuminousIntensityUnitObjects.attocandela
  def fcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.femtocandela
  def pcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.picocandela
  def ncd: LuminousIntensityUnit = LuminousIntensityUnitObjects.nanocandela
  def `μcd`: LuminousIntensityUnit = LuminousIntensityUnitObjects.microcandela
  def mccd: LuminousIntensityUnit = LuminousIntensityUnitObjects.microcandela
  def mcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.millicandela
  def ccd: LuminousIntensityUnit = LuminousIntensityUnitObjects.centicandela
  def dcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.decicandela
  def dacd: LuminousIntensityUnit = LuminousIntensityUnitObjects.decacandela
  def hcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.hectocandela
  def kcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.kilocandela
  def Kcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.kilocandela
  def Mcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.megacandela
  def Gcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.gigacandela
  def Tcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.teracandela
  def Pcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.petacandela
  def Ecd: LuminousIntensityUnit = LuminousIntensityUnitObjects.exacandela
  def Zcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.zettacandela
  def Ycd: LuminousIntensityUnit = LuminousIntensityUnitObjects.yottacandela
}