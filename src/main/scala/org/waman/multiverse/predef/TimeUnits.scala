package org.waman.multiverse.predef

import org.waman.multiverse.units.SimpleTimeUnit
import spire.implicits._

trait TimeUnits{

  case object ms extends SimpleTimeUnit("millisecond", r"1e-3")
  case object s extends SimpleTimeUnit("second", r"1")
  case object min extends SimpleTimeUnit("minite", r"60")
  case object h extends SimpleTimeUnit("hour", r"3600")
}

object TimeUnits extends TimeUnits

