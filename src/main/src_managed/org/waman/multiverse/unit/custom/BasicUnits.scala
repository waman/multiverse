package org.waman.multiverse.unit.custom

import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.angle._
import org.waman.multiverse.unit.defs.chem._
import org.waman.multiverse.unit.defs.thermo._

/**
 * Usually used units of length, area, volume, mass, time, angle, frequency, temperature, e.t.c.
 */
object BasicUnits{
  /** nanometre */
  def nm: LengthUnit = LengthUnitObjects.nanometre
  /** micrometre */
  def μm: LengthUnit = LengthUnitObjects.micrometre
  /** micrometre */
  def micron: LengthUnit = LengthUnitObjects.micrometre
  /** millimetre */
  def mm: LengthUnit = LengthUnitObjects.millimetre
  /** centimetre */
  def cm: LengthUnit = LengthUnitObjects.centimetre
  /** metre */
  def m : LengthUnit = LengthUnitObjects.metre
  /** kilometre */
  def km: LengthUnit = LengthUnitObjects.kilometre
  /** square_millimetre */
  def mm2: AreaUnit = AreaUnitObjects.square_millimetre
  /** square_centimetre */
  def cm2: AreaUnit = AreaUnitObjects.square_centimetre
  /** square_metre */
  def m2 : AreaUnit = AreaUnitObjects.square_metre
  /** are */
  def a: AreaUnit = AreaUnitObjects.are
  /** hectare */
  def ha: AreaUnit = AreaUnitObjects.hectare
  /** square_kilometre */
  def km2: AreaUnit = AreaUnitObjects.square_kilometre
  /** cubic_millimetre */
  def mm3: VolumeUnit = VolumeUnitObjects.cubic_millimetre
  /** cubic_centimetre */
  def cm3: VolumeUnit = VolumeUnitObjects.cubic_centimetre
  /** cubic_metre */
  def m3 : VolumeUnit = VolumeUnitObjects.cubic_metre
  /** cubic_kilometre */
  def km3: VolumeUnit = VolumeUnitObjects.cubic_kilometre
  /** millilitre */
  def mL: VolumeUnit = VolumeUnitObjects.millilitre
  /** litre */
  def L: VolumeUnit = VolumeUnitObjects.litre
  /** microgram */
  def μg: MassUnit = MassUnitObjects.microgram
  /** microgram */
  def mcg: MassUnit = MassUnitObjects.microgram
  /** milligram */
  def mg: MassUnit = MassUnitObjects.milligram
  /** gram */
  def g : MassUnit = MassUnitObjects.gram
  /** kilogram */
  def kg: MassUnit = MassUnitObjects.kilogram
  /** tonne */
  def t : MassUnit = MassUnitObjects.tonne
  /** picosecond */
  def ps: TimeUnit = TimeUnitObjects.picosecond
  /** nanosecond */
  def ns: TimeUnit = TimeUnitObjects.nanosecond
  /** microsecond */
  def μs: TimeUnit = TimeUnitObjects.microsecond
  /** millisecond */
  def ms: TimeUnit = TimeUnitObjects.millisecond
  /** second */
  def s : TimeUnit = TimeUnitObjects.second
  /** minute */
  def min: TimeUnit = TimeUnitObjects.minute
  /** hour */
  def h : TimeUnit = TimeUnitObjects.hour
  /** mach_number */
  def M: VelocityUnit = VelocityUnitObjects.mach_number
  /** speed_of_light */
  def c: VelocityUnit = VelocityUnitObjects.speed_of_light
  /** kine */
  def kine: VelocityUnit = VelocityUnitObjects.kine
  /** radian */
  def rad: AngleUnit = AngleUnitObjects.radian
  /** degree */
  def deg: AngleUnit = AngleUnitObjects.degree
  /** steradian */
  def sr: SolidAngleUnit = SolidAngleUnitObjects.steradian
  /** heltz */
  def Hz: FrequencyUnit = FrequencyUnitObjects.heltz
  /** kiloheltz */
  def kHz: FrequencyUnit = FrequencyUnitObjects.kiloheltz
  /** megaheltz */
  def MHz: FrequencyUnit = FrequencyUnitObjects.megaheltz
  /** gigaheltz */
  def GHz: FrequencyUnit = FrequencyUnitObjects.gigaheltz
  /** teraheltz */
  def THz: FrequencyUnit = FrequencyUnitObjects.teraheltz
  /** mole */
  def mol: AmountOfSubstanceUnit = AmountOfSubstanceUnitObjects.mole
  /** celsius */
  def degC: TemperatureUnit = TemperatureUnitObjects.celsius
  /** kelvin */
  def K: AbsoluteTemperatureUnit = AbsoluteTemperatureUnitObjects.kelvin
}