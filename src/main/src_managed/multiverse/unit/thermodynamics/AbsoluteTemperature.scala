package multiverse.unit.thermodynamics

import spire.math.Real
import spire.math.Fractional

import multiverse._


class AbsoluteTemperature[A: Fractional](val value: A, val unit: AbsoluteTemperatureUnit)
    extends LinearQuantity[AbsoluteTemperature[A], A, AbsoluteTemperatureUnit] {

  import spire.implicits._

  import multiverse.unit.Constants
  import multiverse.unit.mechanics.Energy
  import multiverse.unit.mechanics.EnergyUnitObjects

  def toEnergy: Energy[A] = new Energy(
      apply(AbsoluteTemperatureUnitObjects.kelvin) * implicitly[Fractional[A]].fromReal(Constants.BoltzmannConstant),
      EnergyUnitObjects.joule)


  def toTemperature: Temperature[A] = new Temperature(
      apply(AbsoluteTemperatureUnitObjects.kelvin),
      TemperatureUnitObjects.kelvin)

  override protected def newQuantity(value: A, unit: AbsoluteTemperatureUnit): AbsoluteTemperature[A] = new AbsoluteTemperature(value, unit)
}

trait AbsoluteTemperatureUnit extends LinearUnit[AbsoluteTemperatureUnit]{

  override def getSIUnit: AbsoluteTemperatureUnit = AbsoluteTemperatureUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AbsoluteTemperatureUnit.dimension
}

object AbsoluteTemperatureUnit extends UnitInfo[AbsoluteTemperatureUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](Θ -> 1).withDefaultValue(0)

  def getSIUnit: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.kelvin

  import AbsoluteTemperatureUnitObjects._
  def getUnits: Seq[AbsoluteTemperatureUnit] =
    Seq(kelvin, yoctokelvin, zeptokelvin, attokelvin, femtokelvin, picokelvin, nanokelvin, microkelvin, millikelvin, centikelvin, decikelvin, decakelvin, hectokelvin, kilokelvin, megakelvin, gigakelvin, terakelvin, petakelvin, exakelvin, zettakelvin, yottakelvin)
}

/** For no aliase or user defined units */
class SimpleAbsoluteTemperatureUnit(val name: String, val symbol: String, val interval: Real) extends AbsoluteTemperatureUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAbsoluteTemperatureUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AbsoluteTemperatureUnit

object AbsoluteTemperatureUnitObjects{

  import spire.implicits._


  final case object kelvin extends SimpleAbsoluteTemperatureUnit("kelvin", "K", 1)
  final case object yoctokelvin extends SimpleAbsoluteTemperatureUnit("yoctokelvin", "yK", r"1e-24")
  final case object zeptokelvin extends SimpleAbsoluteTemperatureUnit("zeptokelvin", "zK", r"1e-21")
  final case object attokelvin extends SimpleAbsoluteTemperatureUnit("attokelvin", "aK", r"1e-18")
  final case object femtokelvin extends SimpleAbsoluteTemperatureUnit("femtokelvin", "fK", r"1e-15")
  final case object picokelvin extends SimpleAbsoluteTemperatureUnit("picokelvin", "pK", r"1e-12")
  final case object nanokelvin extends SimpleAbsoluteTemperatureUnit("nanokelvin", "nK", r"1e-9")
  final case object microkelvin extends DefaultAbsoluteTemperatureUnit("microkelvin", "μK", Seq("mcK"), r"1e-6")
  final case object millikelvin extends SimpleAbsoluteTemperatureUnit("millikelvin", "mK", r"1e-3")
  final case object centikelvin extends SimpleAbsoluteTemperatureUnit("centikelvin", "cK", r"1e-2")
  final case object decikelvin extends SimpleAbsoluteTemperatureUnit("decikelvin", "dK", r"1e-1")
  final case object decakelvin extends SimpleAbsoluteTemperatureUnit("decakelvin", "daK", r"1e1")
  final case object hectokelvin extends SimpleAbsoluteTemperatureUnit("hectokelvin", "hK", r"1e2")
  final case object kilokelvin extends DefaultAbsoluteTemperatureUnit("kilokelvin", "kK", Seq("KK"), r"1e3")
  final case object megakelvin extends SimpleAbsoluteTemperatureUnit("megakelvin", "MK", r"1e6")
  final case object gigakelvin extends SimpleAbsoluteTemperatureUnit("gigakelvin", "GK", r"1e9")
  final case object terakelvin extends SimpleAbsoluteTemperatureUnit("terakelvin", "TK", r"1e12")
  final case object petakelvin extends SimpleAbsoluteTemperatureUnit("petakelvin", "PK", r"1e15")
  final case object exakelvin extends SimpleAbsoluteTemperatureUnit("exakelvin", "EK", r"1e18")
  final case object zettakelvin extends SimpleAbsoluteTemperatureUnit("zettakelvin", "ZK", r"1e21")
  final case object yottakelvin extends SimpleAbsoluteTemperatureUnit("yottakelvin", "YK", r"1e24")
}

object AbsoluteTemperatureUnits{

  def K: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.kelvin
  def yK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.yoctokelvin
  def zK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.zeptokelvin
  def aK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.attokelvin
  def fK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.femtokelvin
  def pK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.picokelvin
  def nK: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.nanokelvin
  def `μK`: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.microkelvin
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