package org.waman.multiverse.unit.custom

import org.waman.multiverse.unit.defs._
/** Units of US customary measurement systems. */
object USCustomaryUnits{
  /** point */
  def pt: LengthUnit = LengthUnitObjects.point
  /** pica */
  def pc: LengthUnit = LengthUnitObjects.pica
  /** inch */
  def in: LengthUnit = LengthUnitObjects.inch
  /** foot */
  def ft: LengthUnit = LengthUnitObjects.foot
  /** yard */
  def yd: LengthUnit = LengthUnitObjects.yard
  /** mile */
  def mi: LengthUnit = LengthUnitObjects.mile
  /** fathom */
  def ftm: LengthUnit = LengthUnitObjects.fathom
  /** cable */
  def cb: LengthUnit = LengthUnitObjects.cable
  /** nautical_mile */
  def NM: LengthUnit = LengthUnitObjects.nautical_mile
  /** nautical_mile */
  def nmi: LengthUnit = LengthUnitObjects.nautical_mile
  /** square_foot */
  def ft2: AreaUnit = AreaUnitObjects.square_foot
  /** square_foot */
  def sq_ft: AreaUnit = AreaUnitObjects.square_foot
  /** square_chain */
  def ch2: AreaUnit = AreaUnitObjects.square_chain
  /** square_chain */
  def sq_ch: AreaUnit = AreaUnitObjects.square_chain
  /** acre */
  def ac: AreaUnit = AreaUnitObjects.acre
  /** section */
  def section: AreaUnit = AreaUnitObjects.section
  /** cubic_inch */
  def in3: VolumeUnit = VolumeUnitObjects.cubic_inch
  /** cubic_inch */
  def cu_in: VolumeUnit = VolumeUnitObjects.cubic_inch
  /** cubic_foot */
  def ft3: VolumeUnit = VolumeUnitObjects.cubic_foot
  /** cubic_foot */
  def cu_ft: VolumeUnit = VolumeUnitObjects.cubic_foot
  /** cubic_yard */
  def yd3: VolumeUnit = VolumeUnitObjects.cubic_yard
  /** cubic_yard */
  def cu_yd: VolumeUnit = VolumeUnitObjects.cubic_yard
  /** acre_foot */
  def acre_ft: VolumeUnit = VolumeUnitObjects.acre_foot
  /** grain */
  def gr: MassUnit = MassUnitObjects.grain
  /** dram */
  def dr: MassUnit = MassUnitObjects.dram
  /** ounce */
  def oz: MassUnit = MassUnitObjects.ounce
  /** pound */
  def lb: MassUnit = MassUnitObjects.pound
  /** short_hundredweight */
  def cwt: MassUnit = MassUnitObjects.short_hundredweight
  /** long_hundredweight */
  def long_cwt: MassUnit = MassUnitObjects.long_hundredweight
  /** short_ton */
  def sh_tn: MassUnit = MassUnitObjects.short_ton
  /** long_ton */
  def long_tn: MassUnit = MassUnitObjects.long_ton
  /** pennyweight */
  def dwt: MassUnit = MassUnitObjects.pennyweight
  /** ounce(troy) */
  def oz_t: MassUnit = MassUnitObjects.`ounce(troy)`
  /** pound(troy) */
  def lb_t: MassUnit = MassUnitObjects.`pound(troy)`
}