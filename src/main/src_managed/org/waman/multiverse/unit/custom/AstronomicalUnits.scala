package org.waman.multiverse.unit.custom

import org.waman.multiverse.unit.defs._

/**
 * Usually used units of astronomics
 */
object AstronomicalUnits{
  def AU: LengthUnit = LengthUnitObjects.astronomical_unit
  def ly: LengthUnit = LengthUnitObjects.light_year
  def pc: LengthUnit = LengthUnitObjects.parsec
}