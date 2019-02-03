package org.waman.multiverse.predef

import org.waman.multiverse.metric.SimpleLengthUnit
import spire.implicits._

trait LengthUnits{

  case object mm extends SimpleLengthUnit(r"1e-3")
  case object m extends SimpleLengthUnit(r"1")
  case object km extends SimpleLengthUnit(r"1e3")
}

object LengthUnits extends LengthUnits