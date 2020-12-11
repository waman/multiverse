package multiverse.unit

import multiverse.unit.basic._
import multiverse.unit.basic._

/**
 * Units of US customary measurement systems (US Survey)
 */

object USCustomarySurveyUnits{

  def li: LengthUnit = LengthUnitObjects.`link(US)`
  def ft: LengthUnit = LengthUnitObjects.`foot(US)`
  def rd: LengthUnit = LengthUnitObjects.`rod(US)`
  def ch: LengthUnit = LengthUnitObjects.`chain(US)`
  def fur: LengthUnit = LengthUnitObjects.`furlong(US)`
  def mi: LengthUnit = LengthUnitObjects.`mile(US)`
  def lea: LengthUnit = LengthUnitObjects.`league(US)`
  def ft2: AreaUnit = AreaUnitObjects.`square_foot(US)`
  def sq_ft: AreaUnit = AreaUnitObjects.`square_foot(US)`
  def ch2: AreaUnit = AreaUnitObjects.`square_chain(US)`
  def sq_ch: AreaUnit = AreaUnitObjects.`square_chain(US)`
  def ac: AreaUnit = AreaUnitObjects.`acre(US)`
  def section: AreaUnit = AreaUnitObjects.`section(US)`
  def twp: AreaUnit = AreaUnitObjects.township
}