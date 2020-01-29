package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.electrics.Current
import org.waman.multiverse.unit.electrics.CurrentUnit

import org.waman.multiverse.unit.electrics.Voltage
import org.waman.multiverse.unit.electrics.VoltageUnit


class Power[A: Fractional](val value: A, val unit: PowerUnit)
    extends LinearQuantity[Power[A], A, PowerUnit] {

  override protected def newQuantity(value: A, unit: PowerUnit): Power[A] = new Power(value, unit)

  def /(current: Current[A]): Voltage[A] = new Voltage(this.value / current.value, this.unit / current.unit)
}

/** null */
trait PowerUnit extends LinearUnit[PowerUnit]{

  override def getSIUnit: PowerUnit = PowerUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = PowerUnit.dimension

  def /(currentUnit: CurrentUnit): VoltageUnit =
    new AbstractQuotientUnit[VoltageUnit, PowerUnit, CurrentUnit](PowerUnit.this, currentUnit) with VoltageUnit
}

object PowerUnit extends UnitInfo[PowerUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, M -> 1, L -> 2).withDefaultValue(0)

  def getSIUnit: PowerUnit = PowerUnitObjects.watt

  import PowerUnitObjects._
  def getUnits: Seq[PowerUnit] =
    Seq(watt, yoctowatt, zeptowatt, attowatt, femtowatt, picowatt, nanowatt, microwatt, milliwatt, centiwatt, deciwatt, decawatt, hectowatt, kilowatt, megawatt, gigawatt, terawatt, petawatt, exawatt, zettawatt, yottawatt)
}

/** For no aliase or user defined units */
class SimplePowerUnit(val name: String, val symbol: String, val interval: Real) extends PowerUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultPowerUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends PowerUnit

object PowerUnitObjects{

  final case object watt extends SimplePowerUnit("watt", "W", 1)
  final case object yoctowatt extends SimplePowerUnit("yoctowatt", "yW", r"1e-24")
  final case object zeptowatt extends SimplePowerUnit("zeptowatt", "zW", r"1e-21")
  final case object attowatt extends SimplePowerUnit("attowatt", "aW", r"1e-18")
  final case object femtowatt extends SimplePowerUnit("femtowatt", "fW", r"1e-15")
  final case object picowatt extends SimplePowerUnit("picowatt", "pW", r"1e-12")
  final case object nanowatt extends SimplePowerUnit("nanowatt", "nW", r"1e-9")
  final case object microwatt extends DefaultPowerUnit("microwatt", "μW", Seq("mcW"), r"1e-6")
  final case object milliwatt extends SimplePowerUnit("milliwatt", "mW", r"1e-3")
  final case object centiwatt extends SimplePowerUnit("centiwatt", "cW", r"1e-2")
  final case object deciwatt extends SimplePowerUnit("deciwatt", "dW", r"1e-1")
  final case object decawatt extends SimplePowerUnit("decawatt", "daW", r"1e1")
  final case object hectowatt extends SimplePowerUnit("hectowatt", "hW", r"1e2")
  final case object kilowatt extends DefaultPowerUnit("kilowatt", "kW", Seq("KW"), r"1e3")
  final case object megawatt extends SimplePowerUnit("megawatt", "MW", r"1e6")
  final case object gigawatt extends SimplePowerUnit("gigawatt", "GW", r"1e9")
  final case object terawatt extends SimplePowerUnit("terawatt", "TW", r"1e12")
  final case object petawatt extends SimplePowerUnit("petawatt", "PW", r"1e15")
  final case object exawatt extends SimplePowerUnit("exawatt", "EW", r"1e18")
  final case object zettawatt extends SimplePowerUnit("zettawatt", "ZW", r"1e21")
  final case object yottawatt extends SimplePowerUnit("yottawatt", "YW", r"1e24")
}

object PowerUnits{
  def W: PowerUnit = PowerUnitObjects.watt
  def yW: PowerUnit = PowerUnitObjects.yoctowatt
  def zW: PowerUnit = PowerUnitObjects.zeptowatt
  def aW: PowerUnit = PowerUnitObjects.attowatt
  def fW: PowerUnit = PowerUnitObjects.femtowatt
  def pW: PowerUnit = PowerUnitObjects.picowatt
  def nW: PowerUnit = PowerUnitObjects.nanowatt
  def `μW`: PowerUnit = PowerUnitObjects.microwatt
  def mcW: PowerUnit = PowerUnitObjects.microwatt
  def mW: PowerUnit = PowerUnitObjects.milliwatt
  def cW: PowerUnit = PowerUnitObjects.centiwatt
  def dW: PowerUnit = PowerUnitObjects.deciwatt
  def daW: PowerUnit = PowerUnitObjects.decawatt
  def hW: PowerUnit = PowerUnitObjects.hectowatt
  def kW: PowerUnit = PowerUnitObjects.kilowatt
  def KW: PowerUnit = PowerUnitObjects.kilowatt
  def MW: PowerUnit = PowerUnitObjects.megawatt
  def GW: PowerUnit = PowerUnitObjects.gigawatt
  def TW: PowerUnit = PowerUnitObjects.terawatt
  def PW: PowerUnit = PowerUnitObjects.petawatt
  def EW: PowerUnit = PowerUnitObjects.exawatt
  def ZW: PowerUnit = PowerUnitObjects.zettawatt
  def YW: PowerUnit = PowerUnitObjects.yottawatt
}