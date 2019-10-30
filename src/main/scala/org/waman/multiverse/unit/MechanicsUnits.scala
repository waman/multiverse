package org.waman.multiverse.unit

import org.waman.multiverse.unit.mechanics._
import scala.reflect.runtime.{universe => ru}

object MechanicsUnits extends PhysicalUnitPredefProxy {

  // Time Squared Units
  def s2: TimeSquaredUnit = TimeSquaredUnits.s2

  // Acceleration Units
  def g_0: AccelerationUnit = AccelerationUnits.g_0

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}
