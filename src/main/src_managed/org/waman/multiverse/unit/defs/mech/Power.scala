package org.waman.multiverse.unit.defs.mech

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs.em._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.radio.freq._
import org.waman.multiverse.unit.defs.radio._
import org.waman.multiverse.unit.defs.fluid._

class Power[A: Fractional](val value: A, val unit: PowerUnit)
    extends LinearQuantity[Power[A], A, PowerUnit] {

  override protected def newQuantity(value: A, unit: PowerUnit): Power[A] = new Power(value, unit)

  def /(electricCurrent: ElectricCurrent[A]): Voltage[A] = new Voltage(this.value / electricCurrent.value, this.unit / electricCurrent.unit)

  def *(time: Time[A]): Energy[A] = new Energy(this.value * time.value, this.unit * time.unit)

  def /(areaFrequency: AreaFrequency[A]): freq.SpectralIrradiance[A] = new freq.SpectralIrradiance(this.value / areaFrequency.value, this.unit / areaFrequency.unit)

  def /(area: Area[A]): Irradiance[A] = new Irradiance(this.value / area.value, this.unit / area.unit)
}

/** Some(This can be used as a unit of radiant flux (radiation).) */
trait PowerUnit extends LinearUnit[PowerUnit]{

  override def getSIUnit: PowerUnit = PowerUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = PowerUnit.dimension

  def /(electricCurrentUnit: ElectricCurrentUnit): VoltageUnit =
    new QuotientUnit[VoltageUnit, PowerUnit, ElectricCurrentUnit](PowerUnit.this, electricCurrentUnit) with VoltageUnit

  def *(timeUnit: TimeUnit): EnergyUnit =
    new ProductUnit[EnergyUnit, PowerUnit, TimeUnit](PowerUnit.this, timeUnit) with EnergyUnit

  def /(areaFrequencyUnit: AreaFrequencyUnit): freq.SpectralIrradianceUnit =
    new QuotientUnit[freq.SpectralIrradianceUnit, PowerUnit, AreaFrequencyUnit](PowerUnit.this, areaFrequencyUnit) with freq.SpectralIrradianceUnit

  def /(areaUnit: AreaUnit): IrradianceUnit =
    new QuotientUnit[IrradianceUnit, PowerUnit, AreaUnit](PowerUnit.this, areaUnit) with IrradianceUnit
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


/** For no alias or user defined units */
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
  final case object horsepower extends DefaultPowerUnit("horsepower", "hp", Seq("HP"), `horsepower(mechanical)`.interval)
  final case object `horsepower(mechanical)` extends DefaultPowerUnit("horsepower(mechanical)", "hp(mechanical)", Seq("HP(mechanical)"), r"33000" * LengthUnitObjects.foot.interval * ForceUnitObjects.pound_force.interval / TimeUnitObjects.minute.interval)
  final case object `horsepower(metric)` extends DefaultPowerUnit("horsepower(metric)", "hp(metric)", Seq("PS"), r"75" * ForceUnitObjects.kilogram_force.interval * LengthUnitObjects.metre.interval / TimeUnitObjects.second.interval)
  final case object `horsepower(electrical)` extends DefaultPowerUnit("horsepower(electrical)", "hp(electrical)", Seq("HP(electrical)"), r"746")
  final case object `horsepower(boiler)` extends DefaultPowerUnit("horsepower(boiler)", "hp(boiler)", Seq("HP(boiler)"), r"9812.5")
  final case object lusec extends SimplePowerUnit("lusec", "lusec", VolumeUnitObjects.litre.interval * PressureUnitObjects.micrometre_of_mercury.interval / TimeUnitObjects.second.interval)
  final case object poncelet extends SimplePowerUnit("poncelet", "p", r"100" * LengthUnitObjects.metre.interval * ForceUnitObjects.kilogram_force.interval / TimeUnitObjects.second.interval)
  final case object square_foot_equivalent_direct_radiation extends SimplePowerUnit("square foot equivalent direct radiation", "sq_ft_EDR", r"240" * EnergyUnitObjects.`british_thermal_unit(IT)`.interval / TimeUnitObjects.hour.interval)
}

sealed trait horsepowerAttribute

object PowerUnits{
  final object mechanical extends horsepowerAttribute
  final object metric extends horsepowerAttribute
  final object electrical extends horsepowerAttribute
  final object boiler extends horsepowerAttribute

  /** watt */
  def W: PowerUnit = PowerUnitObjects.watt
  /** yoctowatt */
  def yW: PowerUnit = PowerUnitObjects.yoctowatt
  /** zeptowatt */
  def zW: PowerUnit = PowerUnitObjects.zeptowatt
  /** attowatt */
  def aW: PowerUnit = PowerUnitObjects.attowatt
  /** femtowatt */
  def fW: PowerUnit = PowerUnitObjects.femtowatt
  /** picowatt */
  def pW: PowerUnit = PowerUnitObjects.picowatt
  /** nanowatt */
  def nW: PowerUnit = PowerUnitObjects.nanowatt
  /** microwatt */
  def μW: PowerUnit = PowerUnitObjects.microwatt
  /** microwatt */
  def mcW: PowerUnit = PowerUnitObjects.microwatt
  /** milliwatt */
  def mW: PowerUnit = PowerUnitObjects.milliwatt
  /** centiwatt */
  def cW: PowerUnit = PowerUnitObjects.centiwatt
  /** deciwatt */
  def dW: PowerUnit = PowerUnitObjects.deciwatt
  /** decawatt */
  def daW: PowerUnit = PowerUnitObjects.decawatt
  /** hectowatt */
  def hW: PowerUnit = PowerUnitObjects.hectowatt
  /** kilowatt */
  def kW: PowerUnit = PowerUnitObjects.kilowatt
  /** kilowatt */
  def KW: PowerUnit = PowerUnitObjects.kilowatt
  /** megawatt */
  def MW: PowerUnit = PowerUnitObjects.megawatt
  /** gigawatt */
  def GW: PowerUnit = PowerUnitObjects.gigawatt
  /** terawatt */
  def TW: PowerUnit = PowerUnitObjects.terawatt
  /** petawatt */
  def PW: PowerUnit = PowerUnitObjects.petawatt
  /** exawatt */
  def EW: PowerUnit = PowerUnitObjects.exawatt
  /** zettawatt */
  def ZW: PowerUnit = PowerUnitObjects.zettawatt
  /** yottawatt */
  def YW: PowerUnit = PowerUnitObjects.yottawatt
  /** horsepower */
  def hp: PowerUnit = PowerUnitObjects.horsepower
  /** horsepower(mechanical)<br/>horsepower(metric)<br/>horsepower(electrical)<br/>horsepower(boiler) */
  def hp(a: horsepowerAttribute): PowerUnit = a match {
    case PowerUnits.mechanical => PowerUnitObjects.`horsepower(mechanical)`
    case PowerUnits.metric => PowerUnitObjects.`horsepower(metric)`
    case PowerUnits.electrical => PowerUnitObjects.`horsepower(electrical)`
    case PowerUnits.boiler => PowerUnitObjects.`horsepower(boiler)`
  }
  /** horsepower */
  def HP: PowerUnit = PowerUnitObjects.horsepower
  /**   horsepower(mechanical)<br/>  horsepower(metric)<br/>  horsepower(electrical)<br/>  horsepower(boiler) */
  def HP(a: horsepowerAttribute): PowerUnit = hp(a)
  /** horsepower(mechanical) */
  def `hp(mechanical)`: PowerUnit = PowerUnitObjects.`horsepower(mechanical)`
  /** horsepower(mechanical) */
  def `HP(mechanical)`: PowerUnit = PowerUnitObjects.`horsepower(mechanical)`
  /** horsepower(metric) */
  def `hp(metric)`: PowerUnit = PowerUnitObjects.`horsepower(metric)`
  /** horsepower(metric) */
  def PS: PowerUnit = PowerUnitObjects.`horsepower(metric)`
  /** horsepower(electrical) */
  def `hp(electrical)`: PowerUnit = PowerUnitObjects.`horsepower(electrical)`
  /** horsepower(electrical) */
  def `HP(electrical)`: PowerUnit = PowerUnitObjects.`horsepower(electrical)`
  /** horsepower(boiler) */
  def `hp(boiler)`: PowerUnit = PowerUnitObjects.`horsepower(boiler)`
  /** horsepower(boiler) */
  def `HP(boiler)`: PowerUnit = PowerUnitObjects.`horsepower(boiler)`
  /** lusec */
  def lusec: PowerUnit = PowerUnitObjects.lusec
  /** poncelet */
  def p: PowerUnit = PowerUnitObjects.poncelet
  /** square foot equivalent direct radiation */
  def sq_ft_EDR: PowerUnit = PowerUnitObjects.square_foot_equivalent_direct_radiation
}