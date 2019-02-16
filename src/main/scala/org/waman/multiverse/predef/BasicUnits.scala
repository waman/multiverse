package org.waman.multiverse.predef

import org.waman.multiverse.predef.basic._

trait BasicUnits
  extends LengthUnits
    with MassUnits
    with TimeUnits
    with VelocityUnits

object BasicUnits extends BasicUnits
