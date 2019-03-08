package org.waman.multiverse.predef.mechanics

import org.waman.multiverse.predef.PhysicalUnitPredef
import org.waman.multiverse.units.mechanics.{AccelerationUnit, SimpleAccelerationUnit}
import spire.implicits._

import scala.reflect.runtime.{universe => ru}

object AccelerationUnits extends PhysicalUnitPredef[AccelerationUnit]{

  final case object g0 extends SimpleAccelerationUnit("standard gravity", r"9.80665")

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}
