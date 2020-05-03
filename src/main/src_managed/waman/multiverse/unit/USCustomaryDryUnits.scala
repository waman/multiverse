package waman.multiverse.unit

import waman.multiverse.unit.basic._

/**
 * Units of US customary measurement systems (Dry)
 */

object USCustomaryDryUnits{

  def pt: VolumeUnit = VolumeUnitObjects.`pint(US_dry)`
  def qt: VolumeUnit = VolumeUnitObjects.`quart(US_dry)`
  def gal: VolumeUnit = VolumeUnitObjects.`gallon(US_dry)`
  def pk: VolumeUnit = VolumeUnitObjects.peck
  def bu: VolumeUnit = VolumeUnitObjects.`bushel(US_lvl)`
  def bbl: VolumeUnit = VolumeUnitObjects.`barrel(US_dry)`
}