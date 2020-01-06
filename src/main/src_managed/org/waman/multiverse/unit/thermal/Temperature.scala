package org.waman.multiverse.unit.thermal

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class Temperature[A: Fractional](val value: A, val unit: TemperatureUnit)
    extends HomogeneousQuantity[A, TemperatureUnit] {


  def toAbsoluteTemperature: AbsoluteTemperature[A] = new AbsoluteTemperature(
      apply(TemperatureUnitObjects.kelvin),
      AbsoluteTemperatureUnitObjects.kelvin)

}

trait TemperatureUnit extends HomogeneousUnit[TemperatureUnit]{

  override def getSIUnit: TemperatureUnit = TemperatureUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = TemperatureUnit.dimension

}

/** For user defined units */
class SimpleTemperatureUnit(val name: String, val symbol: String, val zero: Real, val interval: Real) extends TemperatureUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultTemperatureUnit(val name: String, val symbol: String, val aliases: Seq[String], val zero: Real, val interval: Real)
  extends TemperatureUnit

object TemperatureUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](Θ -> 1).withDefaultValue(0)

  def getSIUnit: TemperatureUnit = TemperatureUnitObjects.kelvin

  import TemperatureUnitObjects._
  def getUnits: Seq[TemperatureUnit] =
    Seq(kelvin, yoctokelvin, zeptokelvin, attokelvin, femtokelvin, picokelvin, nanokelvin, microkelvin, millikelvin, centikelvin, decikelvin, decakelvin, hectokelvin, kilokelvin, megakelvin, gigakelvin, terakelvin, petakelvin, exakelvin, zettakelvin, yottakelvin, celsius, fahrenheit, rankine, delisle, newton, réaumur, rømer, regulo_gas_mark)
}

object TemperatureUnitObjects{

  final case object kelvin extends DefaultTemperatureUnit("kelvin", "K", Nil, 0, 1)
  final case object yoctokelvin extends DefaultTemperatureUnit("yoctokelvin", "yK", Nil, 0, 1 * r"1e-24")
  final case object zeptokelvin extends DefaultTemperatureUnit("zeptokelvin", "zK", Nil, 0, 1 * r"1e-21")
  final case object attokelvin extends DefaultTemperatureUnit("attokelvin", "aK", Nil, 0, 1 * r"1e-18")
  final case object femtokelvin extends DefaultTemperatureUnit("femtokelvin", "fK", Nil, 0, 1 * r"1e-15")
  final case object picokelvin extends DefaultTemperatureUnit("picokelvin", "pK", Nil, 0, 1 * r"1e-12")
  final case object nanokelvin extends DefaultTemperatureUnit("nanokelvin", "nK", Nil, 0, 1 * r"1e-9")
  final case object microkelvin extends DefaultTemperatureUnit("microkelvin", "μK", Seq("mcK"), 0, 1 * r"1e-6")
  final case object millikelvin extends DefaultTemperatureUnit("millikelvin", "mK", Nil, 0, 1 * r"1e-3")
  final case object centikelvin extends DefaultTemperatureUnit("centikelvin", "cK", Nil, 0, 1 * r"1e-2")
  final case object decikelvin extends DefaultTemperatureUnit("decikelvin", "dK", Nil, 0, 1 * r"1e-1")
  final case object decakelvin extends DefaultTemperatureUnit("decakelvin", "daK", Nil, 0, 1 * r"1e1")
  final case object hectokelvin extends DefaultTemperatureUnit("hectokelvin", "hK", Nil, 0, 1 * r"1e2")
  final case object kilokelvin extends DefaultTemperatureUnit("kilokelvin", "kK", Seq("KK"), 0, 1 * r"1e3")
  final case object megakelvin extends DefaultTemperatureUnit("megakelvin", "MK", Nil, 0, 1 * r"1e6")
  final case object gigakelvin extends DefaultTemperatureUnit("gigakelvin", "GK", Nil, 0, 1 * r"1e9")
  final case object terakelvin extends DefaultTemperatureUnit("terakelvin", "TK", Nil, 0, 1 * r"1e12")
  final case object petakelvin extends DefaultTemperatureUnit("petakelvin", "PK", Nil, 0, 1 * r"1e15")
  final case object exakelvin extends DefaultTemperatureUnit("exakelvin", "EK", Nil, 0, 1 * r"1e18")
  final case object zettakelvin extends DefaultTemperatureUnit("zettakelvin", "ZK", Nil, 0, 1 * r"1e21")
  final case object yottakelvin extends DefaultTemperatureUnit("yottakelvin", "YK", Nil, 0, 1 * r"1e24")
  final case object celsius extends DefaultTemperatureUnit("celsius", "°C", Seq("degC", "℃"), r"273.15", 1)
  final case object fahrenheit extends DefaultTemperatureUnit("fahrenheit", "°F", Seq("degF", "℉"), r"273.15" - r"5"/r"9" * r"32", r"5"/r"9")
  final case object rankine extends DefaultTemperatureUnit("rankine", "°R", Seq("degR"), 0, r"5"/r"9")
  final case object delisle extends DefaultTemperatureUnit("delisle", "°De", Seq("degDe"), r"373.15", r"-2"/r"3")
  final case object newton extends DefaultTemperatureUnit("newton", "°N", Seq("degN"), r"273.15", r"100"/r"33")
  final case object réaumur extends DefaultTemperatureUnit("réaumur", "°Ré", Seq("degRe"), r"273.15", r"5"/r"4")
  final case object rømer extends DefaultTemperatureUnit("rømer", "°Rø", Seq("degRo"), r"273.15" - r"7.5" * r"40"/r"21", r"40"/r"21")
  final case object regulo_gas_mark extends DefaultTemperatureUnit("regulo gas mark", "GM", Nil, r"422.038", r"125"/r"9")
}

object TemperatureUnits{
  def K: TemperatureUnit = TemperatureUnitObjects.kelvin
  def yK: TemperatureUnit = TemperatureUnitObjects.yoctokelvin
  def zK: TemperatureUnit = TemperatureUnitObjects.zeptokelvin
  def aK: TemperatureUnit = TemperatureUnitObjects.attokelvin
  def fK: TemperatureUnit = TemperatureUnitObjects.femtokelvin
  def pK: TemperatureUnit = TemperatureUnitObjects.picokelvin
  def nK: TemperatureUnit = TemperatureUnitObjects.nanokelvin
  def `μK`: TemperatureUnit = TemperatureUnitObjects.microkelvin
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
  def `°C`: TemperatureUnit = TemperatureUnitObjects.celsius
  def degC: TemperatureUnit = TemperatureUnitObjects.celsius
  def `℃`: TemperatureUnit = TemperatureUnitObjects.celsius
  def `°F`: TemperatureUnit = TemperatureUnitObjects.fahrenheit
  def degF: TemperatureUnit = TemperatureUnitObjects.fahrenheit
  def `℉`: TemperatureUnit = TemperatureUnitObjects.fahrenheit
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