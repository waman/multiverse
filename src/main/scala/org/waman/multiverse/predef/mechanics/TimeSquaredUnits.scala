package org.waman.multiverse.predef.mechanics

import org.waman.multiverse.predef.PhysicalUnitPredef
import org.waman.multiverse.predef.basic.TimeUnits
import org.waman.multiverse.units.mechanics.{TimeSquared_TimeSquaredUnit, TimeSquaredUnit}

import scala.reflect.runtime.{universe => ru}

object TimeSquaredUnits extends PhysicalUnitPredef[TimeSquaredUnit]{

  final case object s2 extends TimeSquared_TimeSquaredUnit(TimeUnits.s)

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}
