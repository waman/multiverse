package org.waman.multiverse.unit.custom

import org.waman.multiverse.unit.defs._

/**
 * Units of US customary measurement systems (Liquid)
 */
object USCustomaryFluidUnits{
  def min: VolumeUnit = VolumeUnitObjects.minim
  def fl_dr: VolumeUnit = VolumeUnitObjects.`fluid_dram(US)`
  def tsp: VolumeUnit = VolumeUnitObjects.teaspoon
  def Tbsp: VolumeUnit = VolumeUnitObjects.tablespoon
  def fl_oz: VolumeUnit = VolumeUnitObjects.`fluid_ounce(US)`
  def jig: VolumeUnit = VolumeUnitObjects.shot
  def gi: VolumeUnit = VolumeUnitObjects.`gill(US)`
  def cp: VolumeUnit = VolumeUnitObjects.`cup(US)`
  def pt: VolumeUnit = VolumeUnitObjects.`pint(US_fl)`
  def qt: VolumeUnit = VolumeUnitObjects.`quart(US_fl)`
  def pot: VolumeUnit = VolumeUnitObjects.`pottle(US)`
  def gal: VolumeUnit = VolumeUnitObjects.`gallon(US_fl)`
  def fl_bl: VolumeUnit = VolumeUnitObjects.`barrel(US_fl)`
  def bbl: VolumeUnit = VolumeUnitObjects.barrel
  def hhd: VolumeUnit = VolumeUnitObjects.`hogshead(US)`
}