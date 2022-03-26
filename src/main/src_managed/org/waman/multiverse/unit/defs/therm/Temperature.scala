package org.waman.multiverse.unit.defs.therm

import spire.math._
import spire.implicits._

import org.waman.multiverse._

class Temperature[A: Fractional](val value: A, val unit: TemperatureUnit)
    extends HomogeneousQuantity[A, TemperatureUnit] {


  def toAbsoluteTemperature: AbsoluteTemperature[A] = new AbsoluteTemperature(
      apply(TemperatureUnitObjects.kelvin) * implicitly[Fractional[A]].fromReal(r"1"),
      AbsoluteTemperatureUnitObjects.kelvin)

}

/** None */
trait TemperatureUnit extends HomogeneousUnit[TemperatureUnit]{

  override def getSIUnit: TemperatureUnit = TemperatureUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = TemperatureUnit.dimension
}

object TemperatureUnit extends UnitInfo[TemperatureUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](Θ -> 1).withDefaultValue(0)

  def getSIUnit: TemperatureUnit = TemperatureUnitObjects.kelvin

  import TemperatureUnitObjects._

  def getUnits: Seq[TemperatureUnit] =
    Seq(kelvin, yoctokelvin, zeptokelvin, attokelvin, femtokelvin, picokelvin, nanokelvin, microkelvin, millikelvin, centikelvin, decikelvin, decakelvin, hectokelvin, kilokelvin, megakelvin, gigakelvin, terakelvin, petakelvin, exakelvin, zettakelvin, yottakelvin, celsius, fahrenheit, rankine, delisle, newton, réaumur, rømer, regulo_gas_mark)
}

/** For no alias or user defined units */
class SimpleTemperatureUnit(val name: String, val symbol: String, val zero: Real, val interval: Real) extends TemperatureUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultTemperatureUnit(val name: String, val symbol: String, val aliases: Seq[String], val zero: Real, val interval: Real)
  extends TemperatureUnit

object TemperatureUnitObjects{

  final case object kelvin extends SimpleTemperatureUnit("kelvin", "K", r"0", r"1")
  final case object yoctokelvin extends SimpleTemperatureUnit("yoctokelvin", "yK", r"0", r"1e-24")
  final case object zeptokelvin extends SimpleTemperatureUnit("zeptokelvin", "zK", r"0", r"1e-21")
  final case object attokelvin extends SimpleTemperatureUnit("attokelvin", "aK", r"0", r"1e-18")
  final case object femtokelvin extends SimpleTemperatureUnit("femtokelvin", "fK", r"0", r"1e-15")
  final case object picokelvin extends SimpleTemperatureUnit("picokelvin", "pK", r"0", r"1e-12")
  final case object nanokelvin extends SimpleTemperatureUnit("nanokelvin", "nK", r"0", r"1e-9")
  final case object microkelvin extends DefaultTemperatureUnit("microkelvin", "μK", Seq("mcK"), r"0", r"1e-6")
  final case object millikelvin extends SimpleTemperatureUnit("millikelvin", "mK", r"0", r"1e-3")
  final case object centikelvin extends SimpleTemperatureUnit("centikelvin", "cK", r"0", r"1e-2")
  final case object decikelvin extends SimpleTemperatureUnit("decikelvin", "dK", r"0", r"1e-1")
  final case object decakelvin extends SimpleTemperatureUnit("decakelvin", "daK", r"0", r"1e1")
  final case object hectokelvin extends SimpleTemperatureUnit("hectokelvin", "hK", r"0", r"1e2")
  final case object kilokelvin extends DefaultTemperatureUnit("kilokelvin", "kK", Seq("KK"), r"0", r"1e3")
  final case object megakelvin extends SimpleTemperatureUnit("megakelvin", "MK", r"0", r"1e6")
  final case object gigakelvin extends SimpleTemperatureUnit("gigakelvin", "GK", r"0", r"1e9")
  final case object terakelvin extends SimpleTemperatureUnit("terakelvin", "TK", r"0", r"1e12")
  final case object petakelvin extends SimpleTemperatureUnit("petakelvin", "PK", r"0", r"1e15")
  final case object exakelvin extends SimpleTemperatureUnit("exakelvin", "EK", r"0", r"1e18")
  final case object zettakelvin extends SimpleTemperatureUnit("zettakelvin", "ZK", r"0", r"1e21")
  final case object yottakelvin extends SimpleTemperatureUnit("yottakelvin", "YK", r"0", r"1e24")
  final case object celsius extends DefaultTemperatureUnit("celsius", "℃", Seq("°C", "degC"), r"273.15", r"1")
  final case object fahrenheit extends DefaultTemperatureUnit("fahrenheit", "℉", Seq("°F", "degF"), r"273.15" - r"5/9" * r"32", r"5/9")
  final case object rankine extends DefaultTemperatureUnit("rankine", "°R", Seq("degR"), r"0", r"5/9")
  final case object delisle extends DefaultTemperatureUnit("delisle", "°De", Seq("degDe"), r"373.15", r"-2/3")
  final case object newton extends DefaultTemperatureUnit("newton", "°N", Seq("degN"), r"273.15", r"100/33")
  final case object réaumur extends DefaultTemperatureUnit("réaumur", "°Ré", Seq("degRe"), r"273.15", r"5/4")
  final case object rømer extends DefaultTemperatureUnit("rømer", "°Rø", Seq("degRo"), r"273.15" - r"7.5" * r"40/21", r"40/21")
  final case object regulo_gas_mark extends SimpleTemperatureUnit("regulo gas mark", "GM", r"422.038", r"125/9")
}


object TemperatureUnits{
  def K: TemperatureUnit = TemperatureUnitObjects.kelvin
  def yK: TemperatureUnit = TemperatureUnitObjects.yoctokelvin
  def zK: TemperatureUnit = TemperatureUnitObjects.zeptokelvin
  def aK: TemperatureUnit = TemperatureUnitObjects.attokelvin
  def fK: TemperatureUnit = TemperatureUnitObjects.femtokelvin
  def pK: TemperatureUnit = TemperatureUnitObjects.picokelvin
  def nK: TemperatureUnit = TemperatureUnitObjects.nanokelvin
  def μK: TemperatureUnit = TemperatureUnitObjects.microkelvin
  def mcK: TemperatureUnit = TemperatureUnitObjects.microkelvin
  def mK: TemperatureUnit = TemperatureUnitObjects.millikelvin
  def cK: TemperatureUnit = TemperatureUnitObjects.centikelvin
  def dK: TemperatureUnit = TemperatureUnitObjects.decikelvin
  def daK: TemperatureUnit = TemperatureUnitObjects.decakelvin
  def hK: TemperatureUnit = TemperatureUnitObjects.hectokelvin
  def kK: TemperatureUnit = TemperatureUnitObjects.kilokelvin
  def KK: TemperatureUnit = TemperatureUnitObjects.kilokelvin
  def MK: TemperatureUnit = TemperatureUnitObjects.megakelvin
  def GK: TemperatureUnit = TemperatureUnitObjects.gigakelvin
  def TK: TemperatureUnit = TemperatureUnitObjects.terakelvin
  def PK: TemperatureUnit = TemperatureUnitObjects.petakelvin
  def EK: TemperatureUnit = TemperatureUnitObjects.exakelvin
  def ZK: TemperatureUnit = TemperatureUnitObjects.zettakelvin
  def YK: TemperatureUnit = TemperatureUnitObjects.yottakelvin
  def `℃`: TemperatureUnit = TemperatureUnitObjects.celsius
  def `°C`: TemperatureUnit = TemperatureUnitObjects.celsius
  def degC: TemperatureUnit = TemperatureUnitObjects.celsius
  def `℉`: TemperatureUnit = TemperatureUnitObjects.fahrenheit
  def `°F`: TemperatureUnit = TemperatureUnitObjects.fahrenheit
  def degF: TemperatureUnit = TemperatureUnitObjects.fahrenheit
  def `°R`: TemperatureUnit = TemperatureUnitObjects.rankine
  def degR: TemperatureUnit = TemperatureUnitObjects.rankine
  def `°De`: TemperatureUnit = TemperatureUnitObjects.delisle
  def degDe: TemperatureUnit = TemperatureUnitObjects.delisle
  def `°N`: TemperatureUnit = TemperatureUnitObjects.newton
  def degN: TemperatureUnit = TemperatureUnitObjects.newton
  def `°Ré`: TemperatureUnit = TemperatureUnitObjects.réaumur
  def degRe: TemperatureUnit = TemperatureUnitObjects.réaumur
  def `°Rø`: TemperatureUnit = TemperatureUnitObjects.rømer
  def degRo: TemperatureUnit = TemperatureUnitObjects.rømer
  def GM: TemperatureUnit = TemperatureUnitObjects.regulo_gas_mark
}