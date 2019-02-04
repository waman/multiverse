package org.waman.multiverse.predef

import org.waman.multiverse.units.SimpleLengthUnit
import spire.implicits._

trait LengthUnits{

  case object mm extends SimpleLengthUnit("millimetre", r"1e-3")
  case object m extends SimpleLengthUnit("metre", r"1")
  case object km extends SimpleLengthUnit("kilometre", r"1e3")
}

object LengthUnits extends LengthUnits