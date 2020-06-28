package waman.multiverse.unit

import waman.multiverse.unit.mechanics._
import waman.multiverse.unit.mechanics._
import waman.multiverse.unit.mechanics._
import waman.multiverse.unit.mechanics._
import waman.multiverse.unit.mechanics._
import waman.multiverse.unit.fluid._
import waman.multiverse.unit.fluid._
import waman.multiverse.unit.fluid._

/**
 * Usually used units of mechanics and fluid mechanics
 */

object MechanicalUnits{

  def s2: TimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def g_0: AccelerationUnit = AccelerationUnitObjects.standard_gravity
  def Gal: AccelerationUnit = AccelerationUnitObjects.gal
  def N: ForceUnit = ForceUnitObjects.newton
  def dyn: ForceUnit = ForceUnitObjects.dyne
  def kgf: ForceUnit = ForceUnitObjects.kilogram_force
  def J: EnergyUnit = EnergyUnitObjects.joule
  def kJ: EnergyUnit = EnergyUnitObjects.kilojoule
  def MJ: EnergyUnit = EnergyUnitObjects.megajoule
  def erg: EnergyUnit = EnergyUnitObjects.erg
  def cal: EnergyUnit = EnergyUnitObjects.calorie
  def kcal: EnergyUnit = EnergyUnitObjects.kilocalorie
  def Wh: EnergyUnit = EnergyUnitObjects.kilowatt_hour
  def kWh: EnergyUnit = EnergyUnitObjects.kilowatt_hour
  def tTNT: EnergyUnit = EnergyUnitObjects.ton_of_TNT
  def eV: EnergyUnit = EnergyUnitObjects.electronvolt
  def keV: EnergyUnit = EnergyUnitObjects.kiloelectronvolt
  def MeV: EnergyUnit = EnergyUnitObjects.megaelectronvolt
  def GeV: EnergyUnit = EnergyUnitObjects.gigaelectronvolt
  def TeV: EnergyUnit = EnergyUnitObjects.teraelectronvolt
  def W: PowerUnit = PowerUnitObjects.watt
  def mW: PowerUnit = PowerUnitObjects.milliwatt
  def μW: PowerUnit = PowerUnitObjects.microwatt
  def mcW: PowerUnit = PowerUnitObjects.microwatt
  def nW: PowerUnit = PowerUnitObjects.nanowatt
  def pW: PowerUnit = PowerUnitObjects.picowatt
  def kW: PowerUnit = PowerUnitObjects.kilowatt
  def MW: PowerUnit = PowerUnitObjects.megawatt
  def GW: PowerUnit = PowerUnitObjects.gigawatt
  def TW: PowerUnit = PowerUnitObjects.terawatt
  def P: DynamicViscosityUnit = DynamicViscosityUnitObjects.poise
  def St: KinematicViscosityUnit = KinematicViscosityUnitObjects.stokes
  def Pa: PressureUnit = PressureUnitObjects.pascal
  def hPa: PressureUnit = PressureUnitObjects.hectopascal
  def kPa: PressureUnit = PressureUnitObjects.kilopascal
  def MPa: PressureUnit = PressureUnitObjects.megapascal
  def GPa: PressureUnit = PressureUnitObjects.gigapascal
  def Ba: PressureUnit = PressureUnitObjects.barye
  def atm: PressureUnit = PressureUnitObjects.atmosphere
}