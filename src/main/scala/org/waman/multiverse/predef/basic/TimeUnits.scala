package org.waman.multiverse.predef.basic

import org.waman.multiverse.MetricPrefixes._
import org.waman.multiverse.units.basic.SimpleTimeUnit
import spire.implicits._

trait TimeUnits{

  case object ys extends SimpleTimeUnit("yoctosecond", yocto)
  case object zs extends SimpleTimeUnit("zeptosecond", zepto)
  case object as extends SimpleTimeUnit("attosecond", atto)
  case object fs extends SimpleTimeUnit("femtosecond", femto)
  case object ps extends SimpleTimeUnit("picosecond", pico)
  case object ns extends SimpleTimeUnit("nanosecond", nano)
  case object μs extends SimpleTimeUnit("microsecond", micro)
  case object ms extends SimpleTimeUnit("millisecond", milli)

  case object s extends SimpleTimeUnit("second", r"1")
  case object min extends SimpleTimeUnit("minite", r"60")
  case object h extends SimpleTimeUnit("hour", r"3600")
}

object TimeUnits extends TimeUnits

