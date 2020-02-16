package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.electrics.Current
import org.waman.multiverse.unit.electrics.CurrentUnit

import org.waman.multiverse.unit.electrics.Voltage
import org.waman.multiverse.unit.electrics.VoltageUnit

import org.waman.multiverse.unit.angle.SolidAngle
import org.waman.multiverse.unit.angle.SolidAngleUnit

import org.waman.multiverse.unit.radiation.RadiantIntensity
import org.waman.multiverse.unit.radiation.RadiantIntensityUnit


class Power[A: Fractional](val value: A, val unit: PowerUnit)
    extends LinearQuantity[Power[A], A, PowerUnit] {

  override protected def newQuantity(value: A, unit: PowerUnit): Power[A] = new Power(value, unit)

  def /(current: Current[A]): Voltage[A] = new Voltage(this.value / current.value, this.unit / current.unit)

  def /(solidAngle: SolidAngle[A]): RadiantIntensity[A] = new RadiantIntensity(this.value / solidAngle.value, this.unit / solidAngle.unit)
}

/** null */
trait PowerUnit extends LinearUnit[PowerUnit]{

  override def getSIUnit: PowerUnit = PowerUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = PowerUnit.dimension

  def /(currentUnit: CurrentUnit): VoltageUnit =
    new AbstractQuotientUnit[VoltageUnit, PowerUnit, CurrentUnit](PowerUnit.this, currentUnit) with VoltageUnit

  def /(solidAngleUnit: SolidAngleUnit): RadiantIntensityUnit =
    new AbstractQuotientUnit[RadiantIntensityUnit, PowerUnit, SolidAngleUnit](PowerUnit.this, solidAngleUnit) with RadiantIntensityUnit
}

object PowerUnit extends UnitInfo[PowerUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, M -> 1, L -> 2).withDefaultValue(0)

  def getSIUnit: PowerUnit = PowerUnitObjects.watt

  import PowerUnitObjects._
  def getUnits: Seq[PowerUnit] =
    Seq(watt, yoctowatt, zeptowatt, attowatt, femtowatt, picowatt, nanowatt, microwatt, milliwatt, centiwatt, deciwatt, decawatt, hectowatt, kilowatt, megawatt, gigawatt, terawatt, petawatt, exawatt, zettawatt, yottawatt, horsepower, `horsepower(mechanical)`, `horsepower(metric)`, `horsepower(electrical)`, `horsepower(boiler)`, lusec, poncelet, square_foot_equivalent_direct_radiation)
}

/** For no aliase or user defined units */
class SimplePowerUnit(val name: String, val symbol: String, val interval: Real) extends PowerUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultPowerUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends PowerUnit

sealed trait horsepowerAttribute

object PowerAttributes{
  final object boiler extends horsepowerAttribute
  final object mechanical extends horsepowerAttribute
  final object metric extends horsepowerAttribute
  final object electrical extends horsepowerAttribute
}

object PowerUnitObjects{
  import org.waman.multiverse.unit.basic.LengthUnitObjects._
  import org.waman.multiverse.unit.mechanics.ForceUnitObjects._
  import org.waman.multiverse.unit.basic.TimeUnitObjects._
  import org.waman.multiverse.unit.basic.VolumeUnitObjects._
  import org.waman.multiverse.unit.fluid.PressureUnitObjects._
  import org.waman.multiverse.unit.mechanics.EnergyUnitObjects._

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
  final case object horsepower extends DefaultPowerUnit("horsepower", "hp", Seq("HP"), `horsepower(mechanical)`.interval)
  final case object `horsepower(mechanical)` extends DefaultPowerUnit("horsepower(mechanical)", "hp(mechanical)", Seq("HP(mechanical)"), r"33000" * foot.interval * pound_force.interval / minute.interval)
  final case object `horsepower(metric)` extends DefaultPowerUnit("horsepower(metric)", "hp(metric)", Seq("HP(metric)", "PS"), r"75" * kilogram_force.interval * metre.interval / second.interval)
  final case object `horsepower(electrical)` extends DefaultPowerUnit("horsepower(electrical)", "hp(electrical)", Seq("HP(electrical)"), r"746")
  final case object `horsepower(boiler)` extends DefaultPowerUnit("horsepower(boiler)", "hp(boiler)", Seq("HP(boiler)"), r"9812.5")
  final case object lusec extends SimplePowerUnit("lusec", "lusec", litre.interval * micrometre_of_mercury.interval / second.interval)
  final case object poncelet extends SimplePowerUnit("poncelet", "p", r"100" * metre.interval * kilogram_force.interval / second.interval)
  final case object square_foot_equivalent_direct_radiation extends SimplePowerUnit("square foot equivalent direct radiation", "sq_ft_EDR", r"240" * `british_thermal_unit(IT)`.interval / hour.interval)
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
  def hp: PowerUnit = PowerUnitObjects.horsepower
  def hp(a: horsepowerAttribute): PowerUnit = a match { 
    case PowerAttributes.mechanical => PowerUnitObjects.`horsepower(mechanical)`
    case PowerAttributes.metric => PowerUnitObjects.`horsepower(metric)`
    case PowerAttributes.electrical => PowerUnitObjects.`horsepower(electrical)`
    case PowerAttributes.boiler => PowerUnitObjects.`horsepower(boiler)`
  }
  def HP: PowerUnit = PowerUnitObjects.horsepower
  def HP(a: horsepowerAttribute): PowerUnit = hp(a)

  def lusec: PowerUnit = PowerUnitObjects.lusec
  def p: PowerUnit = PowerUnitObjects.poncelet
  def sq_ft_EDR: PowerUnit = PowerUnitObjects.square_foot_equivalent_direct_radiation
}