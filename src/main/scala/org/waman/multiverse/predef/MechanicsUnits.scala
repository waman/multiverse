package org.waman.multiverse.predef

import org.waman.multiverse.predef.mechanics.{AccelerationUnits, TimeSquaredUnits}

trait MechanicsUnits
  extends TimeSquaredUnits
  with AccelerationUnits

object MechanicsUnits extends MechanicsUnits

