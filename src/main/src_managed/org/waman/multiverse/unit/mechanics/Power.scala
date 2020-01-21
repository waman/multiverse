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

/** For user defined units */
class SimplePowerUnit(val name: String, val symbol: String, val interval: Real) extends PowerUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultPowerUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends PowerUnit

object PowerUnitObjects{

  final case object watt extends DefaultPowerUnit("watt", "W", Nil, 1)
  final case object yoctowatt extends DefaultPowerUnit("yoctowatt", "yW", Nil, r"1e-24")
  final case object zeptowatt extends DefaultPowerUnit("zeptowatt", "zW", Nil, r"1e-21")
  final case object attowatt extends DefaultPowerUnit("attowatt", "aW", Nil, r"1e-18")
  final case object femtowatt extends DefaultPowerUnit("femtowatt", "fW", Nil, r"1e-15")
  final case object picowatt extends DefaultPowerUnit("picowatt", "pW", Nil, r"1e-12")
  final case object nanowatt extends DefaultPowerUnit("nanowatt", "nW", Nil, r"1e-9")
  final case object microwatt extends DefaultPowerUnit("microwatt", "μW", Seq("mcW"), r"1e-6")
  final case object milliwatt extends DefaultPowerUnit("milliwatt", "mW", Nil, r"1e-3")
  final case object centiwatt extends DefaultPowerUnit("centiwatt", "cW", Nil, r"1e-2")
  final case object deciwatt extends DefaultPowerUnit("deciwatt", "dW", Nil, r"1e-1")
  final case object decawatt extends DefaultPowerUnit("decawatt", "daW", Nil, r"1e1")
  final case object hectowatt extends DefaultPowerUnit("hectowatt", "hW", Nil, r"1e2")
  final case object kilowatt extends DefaultPowerUnit("kilowatt", "kW", Seq("KW"), r"1e3")
  final case object megawatt extends DefaultPowerUnit("megawatt", "MW", Nil, r"1e6")
  final case object gigawatt extends DefaultPowerUnit("gigawatt", "GW", Nil, r"1e9")
  final case object terawatt extends DefaultPowerUnit("terawatt", "TW", Nil, r"1e12")
  final case object petawatt extends DefaultPowerUnit("petawatt", "PW", Nil, r"1e15")
  final case object exawatt extends DefaultPowerUnit("exawatt", "EW", Nil, r"1e18")
  final case object zettawatt extends DefaultPowerUnit("zettawatt", "ZW", Nil, r"1e21")
  final case object yottawatt extends DefaultPowerUnit("yottawatt", "YW", Nil, r"1e24")
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