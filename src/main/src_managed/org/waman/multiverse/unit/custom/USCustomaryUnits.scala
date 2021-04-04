package org.waman.multiverse.unit.custom

import org.waman.multiverse.unit.defs._

/**
 * Units of US customary measurement systems.
 */
object USCustomaryUnits{
  def pt: LengthUnit = LengthUnitObjects.point
  def pc: LengthUnit = LengthUnitObjects.pica
  def in: LengthUnit = LengthUnitObjects.inch
  def ft: LengthUnit = LengthUnitObjects.foot
  def yd: LengthUnit = LengthUnitObjects.yard
  def mi: LengthUnit = LengthUnitObjects.mile
  def ftm: LengthUnit = LengthUnitObjects.fathom
  def cb: LengthUnit = LengthUnitObjects.cable
  def NM: LengthUnit = LengthUnitObjects.nautical_mile
  def nmi: LengthUnit = LengthUnitObjects.nautical_mile
  def ft2: AreaUnit = AreaUnitObjects.square_foot
  def sq_ft: AreaUnit = AreaUnitObjects.square_foot
  def ch2: AreaUnit = AreaUnitObjects.square_chain
  def sq_ch: AreaUnit = AreaUnitObjects.square_chain
  def ac: AreaUnit = AreaUnitObjects.acre
  def section: AreaUnit = AreaUnitObjects.section
  def in3: VolumeUnit = VolumeUnitObjects.cubic_inch
  def cu_in: VolumeUnit = VolumeUnitObjects.cubic_inch
  def ft3: VolumeUnit = VolumeUnitObjects.cubic_foot
  def cu_ft: VolumeUnit = VolumeUnitObjects.cubic_foot
  def yd3: VolumeUnit = VolumeUnitObjects.cubic_yard
  def cu_yd: VolumeUnit = VolumeUnitObjects.cubic_yard
  def acre_ft: VolumeUnit = VolumeUnitObjects.acre_foot
  def gr: MassUnit = MassUnitObjects.grain
  def dr: MassUnit = MassUnitObjects.dram
  def oz: MassUnit = MassUnitObjects.ounce
  def lb: MassUnit = MassUnitObjects.pound
  def cwt: MassUnit = MassUnitObjects.short_hundredweight
  def long_cwt: MassUnit = MassUnitObjects.long_hundredweight
  def sh_tn: MassUnit = MassUnitObjects.short_ton
  def long_tn: MassUnit = MassUnitObjects.long_ton
  def dwt: MassUnit = MassUnitObjects.pennyweight
  def oz_t: MassUnit = MassUnitObjects.`ounce(troy)`
  def lb_t: MassUnit = MassUnitObjects.`pound(troy)`
}