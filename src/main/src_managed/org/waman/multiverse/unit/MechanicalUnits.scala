package org.waman.multiverse.unit

import org.waman.multiverse.unit.fluid._
import org.waman.multiverse.unit.fluid._
import org.waman.multiverse.unit.fluid._
import org.waman.multiverse.unit.mechanics._
import org.waman.multiverse.unit.mechanics._
import org.waman.multiverse.unit.mechanics._
import org.waman.multiverse.unit.mechanics._
import org.waman.multiverse.unit.mechanics._

/**
 * Usually used units of mechanics and fluid mechanics
 */

object MechanicalUnits{

  def P: DynamicViscosityUnit = DynamicViscosityUnitObjects.poise
  def St: KinematicViscosityUnit = KinematicViscosityUnitObjects.stokes
  def Pa: PressureUnit = PressureUnitObjects.pascal
  def Ba: PressureUnit = PressureUnitObjects.barye
  def atm: PressureUnit = PressureUnitObjects.atmosphere
  def g_0: AccelerationUnit = AccelerationUnitObjects.standard_gravity
  def Gal: AccelerationUnit = AccelerationUnitObjects.gal
  def J: EnergyUnit = EnergyUnitObjects.joule
  def kJ: EnergyUnit = EnergyUnitObjects.kilojoule
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
  def N: ForceUnit = ForceUnitObjects.newton
  def dyn: ForceUnit = ForceUnitObjects.dyne
  def kgf: ForceUnit = ForceUnitObjects.kilogram_force
  def W: PowerUnit = PowerUnitObjects.watt
  def kW: PowerUnit = PowerUnitObjects.kilowatt
  def s2: TimeSquaredUnit = TimeSquaredUnitObjects.second_squared
}