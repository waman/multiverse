package org.waman.multiverse.unit

import org.waman.multiverse.unit.basic._
import scala.reflect.runtime.{universe => ru}

object AstronomicalUnits extends PhysicalUnitPredefProxy {

  def AU: LengthUnit = LengthUnits.AU
  def ly: LengthUnit = LengthUnits.ly
  def pc: LengthUnit = LengthUnits.pc

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}
