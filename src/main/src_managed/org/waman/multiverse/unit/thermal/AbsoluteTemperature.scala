package org.waman.multiverse.unit.thermal

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._
class AbsoluteTemperature[A: Fractional](val value: A, val unit: AbsoluteTemperatureUnit)
    extends LinearQuantity[AbsoluteTemperature[A], A, AbsoluteTemperatureUnit] {

  override protected def newQuantity(value: A, unit: AbsoluteTemperatureUnit): AbsoluteTemperature[A] = new AbsoluteTemperature(value, unit)
           }

trait AbsoluteTemperatureUnit extends LinearUnit[AbsoluteTemperatureUnit]{
  override def getSIUnit: AbsoluteTemperatureUnit = AbsoluteTemperatureUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AbsoluteTemperatureUnit.dimension

}

object AbsoluteTemperatureUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](Θ -> 1).withDefaultValue(0)

  def getSIUnit: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.kelvin

import AbsoluteTemperatureUnitObjects._
  def getUnits: Seq[AbsoluteTemperatureUnit] =
    Seq(kelvin, yoctokelvin, zeptokelvin, attokelvin, femtokelvin, picokelvin, nanokelvin, microkelvin, millikelvin, centikelvin, decikelvin, decakelvin, hectokelvin, kilokelvin, megakelvin, gigakelvin, terakelvin, petakelvin, exakelvin, zettakelvin, yottakelvin)
}



class DefaultAbsoluteTemperatureUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AbsoluteTemperatureUnit

object AbsoluteTemperatureUnitObjects{

  final object kelvin extends DefaultAbsoluteTemperatureUnit("kelvin", "K", Nil, r"1")
  final object yoctokelvin extends DefaultAbsoluteTemperatureUnit("yoctokelvin", "yK", Nil, r"1" * r"1e-24")
  final object zeptokelvin extends DefaultAbsoluteTemperatureUnit("zeptokelvin", "zK", Nil, r"1" * r"1e-21")
  final object attokelvin extends DefaultAbsoluteTemperatureUnit("attokelvin", "aK", Nil, r"1" * r"1e-18")
  final object femtokelvin extends DefaultAbsoluteTemperatureUnit("femtokelvin", "fK", Nil, r"1" * r"1e-15")
  final object picokelvin extends DefaultAbsoluteTemperatureUnit("picokelvin", "pK", Nil, r"1" * r"1e-12")
  final object nanokelvin extends DefaultAbsoluteTemperatureUnit("nanokelvin", "nK", Nil, r"1" * r"1e-9")
  final object microkelvin extends DefaultAbsoluteTemperatureUnit("microkelvin", "μK", Seq("mcK"), r"1" * r"1e-6")
  final object millikelvin extends DefaultAbsoluteTemperatureUnit("millikelvin", "mK", Nil, r"1" * r"1e-3")
  final object centikelvin extends DefaultAbsoluteTemperatureUnit("centikelvin", "cK", Nil, r"1" * r"1e-2")
  final object decikelvin extends DefaultAbsoluteTemperatureUnit("decikelvin", "dK", Nil, r"1" * r"1e-1")
  final object decakelvin extends DefaultAbsoluteTemperatureUnit("decakelvin", "daK", Nil, r"1" * r"1e1")
  final object hectokelvin extends DefaultAbsoluteTemperatureUnit("hectokelvin", "hK", Nil, r"1" * r"1e2")
  final object kilokelvin extends DefaultAbsoluteTemperatureUnit("kilokelvin", "kK", Seq("KK"), r"1" * r"1e3")
  final object megakelvin extends DefaultAbsoluteTemperatureUnit("megakelvin", "MK", Nil, r"1" * r"1e6")
  final object gigakelvin extends DefaultAbsoluteTemperatureUnit("gigakelvin", "GK", Nil, r"1" * r"1e9")
  final object terakelvin extends DefaultAbsoluteTemperatureUnit("terakelvin", "TK", Nil, r"1" * r"1e12")
  final object petakelvin extends DefaultAbsoluteTemperatureUnit("petakelvin", "PK", Nil, r"1" * r"1e15")
  final object exakelvin extends DefaultAbsoluteTemperatureUnit("exakelvin", "EK", Nil, r"1" * r"1e18")
  final object zettakelvin extends DefaultAbsoluteTemperatureUnit("zettakelvin", "ZK", Nil, r"1" * r"1e21")
  final object yottakelvin extends DefaultAbsoluteTemperatureUnit("yottakelvin", "YK", Nil, r"1" * r"1e24")
}

object AbsoluteTemperatureUnits{
  def K: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.kelvin
  def yK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.yoctokelvin
  def zK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.zeptokelvin
  def aK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.attokelvin
  def fK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.femtokelvin
  def pK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.picokelvin
  def nK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.nanokelvin
  def μK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.microkelvin
  def mcK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.microkelvin
  def mK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.millikelvin
  def cK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.centikelvin
  def dK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.decikelvin
  def daK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.decakelvin
  def hK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.hectokelvin
  def kK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.kilokelvin
  def KK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.kilokelvin
  def MK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.megakelvin
  def GK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.gigakelvin
  def TK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.terakelvin
  def PK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.petakelvin
  def EK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.exakelvin
  def ZK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.zettakelvin
  def YK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.yottakelvin
}