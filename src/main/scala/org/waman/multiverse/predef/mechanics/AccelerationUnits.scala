package org.waman.multiverse.predef.mechanics

import spire.implicits._
import org.waman.multiverse.units.basic.SimpleVelocityUnit

trait AccelerationUnits{

  final case object g0 extends SimpleVelocityUnit("standard gravity", r"9.80665")
}
