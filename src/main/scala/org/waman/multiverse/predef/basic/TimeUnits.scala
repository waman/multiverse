package org.waman.multiverse.predef.basic

import org.waman.multiverse.MetricPrefixes._
import org.waman.multiverse.units.basic.SimpleTimeUnit
import spire.implicits._

trait TimeUnits{

  final case object ys extends SimpleTimeUnit("yoctosecond", yocto)
  final case object zs extends SimpleTimeUnit("zeptosecond", zepto)
  final case object as extends SimpleTimeUnit("attosecond", atto)
  final case object fs extends SimpleTimeUnit("femtosecond", femto)
  final case object ps extends SimpleTimeUnit("picosecond", pico)
  final case object ns extends SimpleTimeUnit("nanosecond", nano)
  final case object μs extends SimpleTimeUnit("microsecond", micro)
  final case object ms extends SimpleTimeUnit("millisecond", milli)

  final case object s extends SimpleTimeUnit("second", r"1")
  final case object min extends SimpleTimeUnit("minite", r"60")
  final case object h extends SimpleTimeUnit("hour", r"3600")
}
