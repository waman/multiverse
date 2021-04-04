package org.waman.multiverse.unit.custom

import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.angle._
import org.waman.multiverse.unit.defs.chem._
import org.waman.multiverse.unit.defs.thermo._

/**
 * Usually used units of length, area, volume, mass, time, angle, frequency, temperature, e.t.c.
 */
object BasicUnits{
  def nm: LengthUnit = LengthUnitObjects.nanometre
  def μm: LengthUnit = LengthUnitObjects.micrometre
  def micron: LengthUnit = LengthUnitObjects.micrometre
  def mm: LengthUnit = LengthUnitObjects.millimetre
  def cm: LengthUnit = LengthUnitObjects.centimetre
  def m : LengthUnit = LengthUnitObjects.metre
  def km: LengthUnit = LengthUnitObjects.kilometre
  def mm2: AreaUnit = AreaUnitObjects.square_millimetre
  def cm2: AreaUnit = AreaUnitObjects.square_centimetre
  def m2 : AreaUnit = AreaUnitObjects.square_metre
  def a: AreaUnit = AreaUnitObjects.are
  def ha: AreaUnit = AreaUnitObjects.hectare
  def km2: AreaUnit = AreaUnitObjects.square_kilometre
  def mm3: VolumeUnit = VolumeUnitObjects.cubic_millimetre
  def cm3: VolumeUnit = VolumeUnitObjects.cubic_centimetre
  def m3 : VolumeUnit = VolumeUnitObjects.cubic_metre
  def km3: VolumeUnit = VolumeUnitObjects.cubic_kilometre
  def mL: VolumeUnit = VolumeUnitObjects.millilitre
  def L: VolumeUnit = VolumeUnitObjects.litre
  def μg: MassUnit = MassUnitObjects.microgram
  def mcg: MassUnit = MassUnitObjects.microgram
  def mg: MassUnit = MassUnitObjects.milligram
  def g : MassUnit = MassUnitObjects.gram
  def kg: MassUnit = MassUnitObjects.kilogram
  def t : MassUnit = MassUnitObjects.tonne
  def ps: TimeUnit = TimeUnitObjects.picosecond
  def ns: TimeUnit = TimeUnitObjects.nanosecond
  def μs: TimeUnit = TimeUnitObjects.microsecond
  def ms: TimeUnit = TimeUnitObjects.millisecond
  def s : TimeUnit = TimeUnitObjects.second
  def min: TimeUnit = TimeUnitObjects.minute
  def h : TimeUnit = TimeUnitObjects.hour
  def M: VelocityUnit = VelocityUnitObjects.mach_number
  def c: VelocityUnit = VelocityUnitObjects.speed_of_light
  def kine: VelocityUnit = VelocityUnitObjects.kine
  def rad: AngleUnit = AngleUnitObjects.radian
  def deg: AngleUnit = AngleUnitObjects.degree
  def sr: SolidAngleUnit = SolidAngleUnitObjects.steradian
  def Hz: FrequencyUnit = FrequencyUnitObjects.heltz
  def kHz: FrequencyUnit = FrequencyUnitObjects.kiloheltz
  def MHz: FrequencyUnit = FrequencyUnitObjects.megaheltz
  def GHz: FrequencyUnit = FrequencyUnitObjects.gigaheltz
  def THz: FrequencyUnit = FrequencyUnitObjects.teraheltz
  def mol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.mole
  def degC: TemperatureUnit = TemperatureUnitObjects.celsius
  def K: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.kelvin
}