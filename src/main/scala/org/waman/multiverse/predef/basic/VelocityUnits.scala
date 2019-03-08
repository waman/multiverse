package org.waman.multiverse.predef.basic

import org.waman.multiverse.NotExact
import org.waman.multiverse.predef.PhysicalUnitPredef
import org.waman.multiverse.units.basic.{SimpleVelocityUnit, VelocityUnit}
import spire.implicits._

import scala.reflect.runtime.{universe => ru}

object VelocityUnits extends PhysicalUnitPredef[VelocityUnit]{

  final case object c extends SimpleVelocityUnit("speed of light", r"299792458")
  final case object M extends SimpleVelocityUnit("Mach number", r"340") with NotExact

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}

