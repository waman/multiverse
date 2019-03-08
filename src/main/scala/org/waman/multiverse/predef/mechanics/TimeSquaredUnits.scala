package org.waman.multiverse.predef.mechanics

import org.waman.multiverse.predef.PhysicalUnitPredef
import org.waman.multiverse.units.mechanics.{SimpleTimeSquaredUnit, TimeSquaredUnit}
import spire.implicits._

import scala.reflect.runtime.{universe => ru}

object TimeSquaredUnits extends PhysicalUnitPredef[TimeSquaredUnit]{

  final case object s2 extends SimpleTimeSquaredUnit("second squared", 1)

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}
