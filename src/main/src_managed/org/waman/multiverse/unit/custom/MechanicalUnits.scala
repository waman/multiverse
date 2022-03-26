package org.waman.multiverse.unit.custom

import org.waman.multiverse.unit.defs.mech._
import org.waman.multiverse.unit.defs.fluid._
/** Usually used units of mechanics and fluid mechanics */
object MechanicalUnits{
  /** second_squared */
  def s2: TimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  /** standard_gravity */
  def g_0: AccelerationUnit = AccelerationUnitObjects.standard_gravity
  /** gal */
  def Gal: AccelerationUnit = AccelerationUnitObjects.gal
  /** newton */
  def N: ForceUnit = ForceUnitObjects.newton
  /** dyne */
  def dyn: ForceUnit = ForceUnitObjects.dyne
  /** kilogram_force */
  def kgf: ForceUnit = ForceUnitObjects.kilogram_force
  /** joule */
  def J: EnergyUnit = EnergyUnitObjects.joule
  /** kilojoule */
  def kJ: EnergyUnit = EnergyUnitObjects.kilojoule
  /** megajoule */
  def MJ: EnergyUnit = EnergyUnitObjects.megajoule
  /** erg */
  def erg: EnergyUnit = EnergyUnitObjects.erg
  /** calorie */
  def cal: EnergyUnit = EnergyUnitObjects.calorie
  /** kilocalorie */
  def kcal: EnergyUnit = EnergyUnitObjects.kilocalorie
  /** kilowatt_hour */
  def Wh: EnergyUnit = EnergyUnitObjects.kilowatt_hour
  /** kilowatt_hour */
  def kWh: EnergyUnit = EnergyUnitObjects.kilowatt_hour
  /** ton_of_TNT */
  def tTNT: EnergyUnit = EnergyUnitObjects.ton_of_TNT
  /** electronvolt */
  def eV: EnergyUnit = EnergyUnitObjects.electronvolt
  /** kiloelectronvolt */
  def keV: EnergyUnit = EnergyUnitObjects.kiloelectronvolt
  /** megaelectronvolt */
  def MeV: EnergyUnit = EnergyUnitObjects.megaelectronvolt
  /** gigaelectronvolt */
  def GeV: EnergyUnit = EnergyUnitObjects.gigaelectronvolt
  /** teraelectronvolt */
  def TeV: EnergyUnit = EnergyUnitObjects.teraelectronvolt
  /** watt */
  def W: PowerUnit = PowerUnitObjects.watt
  /** milliwatt */
  def mW: PowerUnit = PowerUnitObjects.milliwatt
  /** microwatt */
  def μW: PowerUnit = PowerUnitObjects.microwatt
  /** microwatt */
  def mcW: PowerUnit = PowerUnitObjects.microwatt
  /** nanowatt */
  def nW: PowerUnit = PowerUnitObjects.nanowatt
  /** picowatt */
  def pW: PowerUnit = PowerUnitObjects.picowatt
  /** kilowatt */
  def kW: PowerUnit = PowerUnitObjects.kilowatt
  /** megawatt */
  def MW: PowerUnit = PowerUnitObjects.megawatt
  /** gigawatt */
  def GW: PowerUnit = PowerUnitObjects.gigawatt
  /** terawatt */
  def TW: PowerUnit = PowerUnitObjects.terawatt
  /** poise */
  def P: DynamicViscosityUnit = DynamicViscosityUnitObjects.poise
  /** stokes */
  def St: KinematicViscosityUnit = KinematicViscosityUnitObjects.stokes
  /** pascal */
  def Pa: PressureUnit = PressureUnitObjects.pascal
  /** hectopascal */
  def hPa: PressureUnit = PressureUnitObjects.hectopascal
  /** kilopascal */
  def kPa: PressureUnit = PressureUnitObjects.kilopascal
  /** megapascal */
  def MPa: PressureUnit = PressureUnitObjects.megapascal
  /** gigapascal */
  def GPa: PressureUnit = PressureUnitObjects.gigapascal
  /** barye */
  def Ba: PressureUnit = PressureUnitObjects.barye
  /** atmosphere */
  def atm: PressureUnit = PressureUnitObjects.atmosphere
}