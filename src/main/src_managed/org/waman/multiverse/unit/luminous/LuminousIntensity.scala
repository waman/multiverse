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

object LuminousIntensityUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](J -> 1).withDefaultValue(0)

  def getSIUnit: LuminousIntensityUnit = LuminousIntensityUnitObjects.candela

  import LuminousIntensityUnitObjects._
  def getUnits: Seq[LuminousIntensityUnit] =
    Seq(candela, yoctocandela, zeptocandela, attocandela, femtocandela, picocandela, nanocandela, microcandela, millicandela, centicandela, decicandela, decacandela, hectocandela, kilocandela, megacandela, gigacandela, teracandela, petacandela, exacandela, zettacandela, yottacandela)
}

class DefaultLuminousIntensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LuminousIntensityUnit

object LuminousIntensityUnitObjects{

  final object candela extends DefaultLuminousIntensityUnit("candela", "cd", Nil, 1)
  final object yoctocandela extends DefaultLuminousIntensityUnit("yoctocandela", "ycd", Nil, 1 * r"1e-24")
  final object zeptocandela extends DefaultLuminousIntensityUnit("zeptocandela", "zcd", Nil, 1 * r"1e-21")
  final object attocandela extends DefaultLuminousIntensityUnit("attocandela", "acd", Nil, 1 * r"1e-18")
  final object femtocandela extends DefaultLuminousIntensityUnit("femtocandela", "fcd", Nil, 1 * r"1e-15")
  final object picocandela extends DefaultLuminousIntensityUnit("picocandela", "pcd", Nil, 1 * r"1e-12")
  final object nanocandela extends DefaultLuminousIntensityUnit("nanocandela", "ncd", Nil, 1 * r"1e-9")
  final object microcandela extends DefaultLuminousIntensityUnit("microcandela", "μcd", Seq("mccd"), 1 * r"1e-6")
  final object millicandela extends DefaultLuminousIntensityUnit("millicandela", "mcd", Nil, 1 * r"1e-3")
  final object centicandela extends DefaultLuminousIntensityUnit("centicandela", "ccd", Nil, 1 * r"1e-2")
  final object decicandela extends DefaultLuminousIntensityUnit("decicandela", "dcd", Nil, 1 * r"1e-1")
  final object decacandela extends DefaultLuminousIntensityUnit("decacandela", "dacd", Nil, 1 * r"1e1")
  final object hectocandela extends DefaultLuminousIntensityUnit("hectocandela", "hcd", Nil, 1 * r"1e2")
  final object kilocandela extends DefaultLuminousIntensityUnit("kilocandela", "kcd", Seq("Kcd"), 1 * r"1e3")
  final object megacandela extends DefaultLuminousIntensityUnit("megacandela", "Mcd", Nil, 1 * r"1e6")
  final object gigacandela extends DefaultLuminousIntensityUnit("gigacandela", "Gcd", Nil, 1 * r"1e9")
  final object teracandela extends DefaultLuminousIntensityUnit("teracandela", "Tcd", Nil, 1 * r"1e12")
  final object petacandela extends DefaultLuminousIntensityUnit("petacandela", "Pcd", Nil, 1 * r"1e15")
  final object exacandela extends DefaultLuminousIntensityUnit("exacandela", "Ecd", Nil, 1 * r"1e18")
  final object zettacandela extends DefaultLuminousIntensityUnit("zettacandela", "Zcd", Nil, 1 * r"1e21")
  final object yottacandela extends DefaultLuminousIntensityUnit("yottacandela", "Ycd", Nil, 1 * r"1e24")
}

object LuminousIntensityUnits{
  def cd: LuminousIntensityUnit = LuminousIntensityUnitObjects.candela
  def ycd: LuminousIntensityUnit = LuminousIntensityUnitObjects.yoctocandela
  def zcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.zeptocandela
  def acd: LuminousIntensityUnit = LuminousIntensityUnitObjects.attocandela
  def fcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.femtocandela
  def pcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.picocandela
  def ncd: LuminousIntensityUnit = LuminousIntensityUnitObjects.nanocandela
  def μcd: LuminousIntensityUnit = LuminousIntensityUnitObjects.microcandela
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