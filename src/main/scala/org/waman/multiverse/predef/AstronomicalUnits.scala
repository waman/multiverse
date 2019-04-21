package org.waman.multiverse.predef

import org.waman.multiverse.predef.basic.LengthUnits
import org.waman.multiverse.units.basic.LengthUnit

import scala.reflect.runtime.{universe => ru}

object AstronomicalUnits extends PhysicalUnitPredefProxy {

  def au: LengthUnit = LengthUnits.au
  def ly: LengthUnit = LengthUnits.ly
  def pc: LengthUnit = LengthUnits.pc

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}
