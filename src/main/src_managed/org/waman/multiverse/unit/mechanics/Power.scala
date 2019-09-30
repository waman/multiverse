package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

import org.waman.multiverse._

class Power[A: Fractional](val value: A, val unit: PowerUnit)
    extends LinearQuantity[Power[A], A, PowerUnit] {

  override protected def newQuantity(value: A, unit: PowerUnit): Power[A] = new Power(value, unit)
}

trait PowerUnit extends LinearUnit[PowerUnit]{
  override def getSIUnit: PowerUnit = PowerUnitObjects.getSIUnit

  import org.waman.multiverse.unit.electric.CurrentUnit
  import org.waman.multiverse.unit.electric.VoltageUnit

  def /(currentUnit: CurrentUnit): VoltageUnit =
    new QuotientUnit[VoltageUnit, PowerUnit, CurrentUnit](PowerUnit.this, currentUnit) with VoltageUnit

}

class DefaultPowerUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends PowerUnit


object PowerUnitObjects{

  def getSIUnit: PowerUnit = watt

  final object watt extends DefaultPowerUnit("watt", "W", Nil, r"1")
  final object yoctowatt extends DefaultPowerUnit("yoctowatt", "yW", Nil, r"1" * r"1e-24")
  final object zeptowatt extends DefaultPowerUnit("zeptowatt", "zW", Nil, r"1" * r"1e-21")
  final object attowatt extends DefaultPowerUnit("attowatt", "aW", Nil, r"1" * r"1e-18")
  final object femtowatt extends DefaultPowerUnit("femtowatt", "fW", Nil, r"1" * r"1e-15")
  final object picowatt extends DefaultPowerUnit("picowatt", "pW", Nil, r"1" * r"1e-12")
  final object nanowatt extends DefaultPowerUnit("nanowatt", "nW", Nil, r"1" * r"1e-9")
  final object microwatt extends DefaultPowerUnit("microwatt", "μW", Seq("mcW"), r"1" * r"1e-6")
  final object milliwatt extends DefaultPowerUnit("milliwatt", "mW", Nil, r"1" * r"1e-3")
  final object centiwatt extends DefaultPowerUnit("centiwatt", "cW", Nil, r"1" * r"1e-2")
  final object deciwatt extends DefaultPowerUnit("deciwatt", "dW", Nil, r"1" * r"1e-1")
  final object decawatt extends DefaultPowerUnit("decawatt", "daW", Nil, r"1" * r"1e1")
  final object hectowatt extends DefaultPowerUnit("hectowatt", "hW", Nil, r"1" * r"1e2")
  final object kilowatt extends DefaultPowerUnit("kilowatt", "kW", Seq("KW"), r"1" * r"1e3")
  final object megawatt extends DefaultPowerUnit("megawatt", "MW", Nil, r"1" * r"1e6")
  final object gigawatt extends DefaultPowerUnit("gigawatt", "GW", Nil, r"1" * r"1e9")
  final object terawatt extends DefaultPowerUnit("terawatt", "TW", Nil, r"1" * r"1e12")
  final object petawatt extends DefaultPowerUnit("petawatt", "PW", Nil, r"1" * r"1e15")
  final object exawatt extends DefaultPowerUnit("exawatt", "EW", Nil, r"1" * r"1e18")
  final object zettawatt extends DefaultPowerUnit("zettawatt", "ZW", Nil, r"1" * r"1e21")
  final object yottawatt extends DefaultPowerUnit("yottawatt", "YW", Nil, r"1" * r"1e24")

  def getUnits: Seq[PowerUnit] =
    Seq(watt, yoctowatt, zeptowatt, attowatt, femtowatt, picowatt, nanowatt, microwatt, milliwatt, centiwatt, deciwatt, decawatt, hectowatt, kilowatt, megawatt, gigawatt, terawatt, petawatt, exawatt, zettawatt, yottawatt)
}


object PowerUnits{
  def W: PowerUnit = PowerUnitObjects.watt
  def yW: PowerUnit = PowerUnitObjects.yoctowatt
  def zW: PowerUnit = PowerUnitObjects.zeptowatt
  def aW: PowerUnit = PowerUnitObjects.attowatt
  def fW: PowerUnit = PowerUnitObjects.femtowatt
  def pW: PowerUnit = PowerUnitObjects.picowatt
  def nW: PowerUnit = PowerUnitObjects.nanowatt
  def μW: PowerUnit = PowerUnitObjects.microwatt
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

  def getSIUnit: PowerUnit = PowerUnitObjects.getSIUnit
  def getUnits: Seq[PowerUnit] = PowerUnitObjects.getUnits
}
