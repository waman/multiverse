package org.waman.multiverse.predef.basic

import org.waman.multiverse.NotExact
import org.waman.multiverse.units.basic.SimpleVelocityUnit
import spire.implicits._

trait VelocityUnits{

  final case object M extends SimpleVelocityUnit("Mach number", r"340") with NotExact
  final case object c extends SimpleVelocityUnit("speed of light", r"299792458")
}

