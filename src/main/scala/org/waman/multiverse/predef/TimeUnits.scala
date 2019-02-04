package org.waman.multiverse.predef

import org.waman.multiverse.units.SimpleLengthUnit
import spire.implicits._

trait TimeUnits{

  case object ms extends SimpleLengthUnit("millisecond", r"1e-3")
  case object s extends SimpleLengthUnit("second", r"1")
  case object min extends SimpleLengthUnit("minite", r"60")
  case object h extends SimpleLengthUnit("hour", r"3600")
}

object TimeUnits extends TimeUnits

