package org.waman.multiverse.unit.custom

import org.waman.multiverse.unit.defs._

/**
 * Units of US customary measurement systems (Dry)
 */
object USCustomaryDryUnits{
  /** pint(US_dry) */
  def pt: VolumeUnit = VolumeUnitObjects.`pint(US_dry)`
  /** quart(US_dry) */
  def qt: VolumeUnit = VolumeUnitObjects.`quart(US_dry)`
  /** gallon(US_dry) */
  def gal: VolumeUnit = VolumeUnitObjects.`gallon(US_dry)`
  /** peck */
  def pk: VolumeUnit = VolumeUnitObjects.peck
  /** bushel(US_lvl) */
  def bu: VolumeUnit = VolumeUnitObjects.`bushel(US_lvl)`
  /** barrel(US_dry) */
  def bbl: VolumeUnit = VolumeUnitObjects.`barrel(US_dry)`
}