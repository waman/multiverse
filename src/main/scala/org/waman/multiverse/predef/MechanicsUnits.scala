package org.waman.multiverse.predef

import org.waman.multiverse.predef.mechanics.{AccelerationUnits, TimeSquaredUnits}
import org.waman.multiverse.units.mechanics.{AccelerationUnit, TimeSquaredUnit}

import scala.reflect.runtime.{universe => ru}

object MechanicsUnits extends PhysicalUnitPredefProxy {

  // Time Squared Units
  def s2: TimeSquaredUnit = TimeSquaredUnits.`s²`

  // Acceleration Units
  def g_0: AccelerationUnit = AccelerationUnits.g_0

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}

