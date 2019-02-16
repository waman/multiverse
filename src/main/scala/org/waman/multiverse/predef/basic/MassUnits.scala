package org.waman.multiverse.predef.basic

import org.waman.multiverse.MetricPrefixes._
import org.waman.multiverse.units.basic.SimpleMassUnit
import spire.implicits._

trait MassUnits {

  // NOTE: SI Unit is 'kilo'gram
  case object yg extends SimpleMassUnit("yoctogram", yocto/r"1000")
  case object zg extends SimpleMassUnit("zeptogram", yocto)
  case object ag extends SimpleMassUnit("attogram", zepto)
  case object fg extends SimpleMassUnit("femtogram", atto)
  case object pg extends SimpleMassUnit("picogram", femto)
  case object ng extends SimpleMassUnit("nanogram", pico)
  case object μg extends SimpleMassUnit("microgram", nano)
  case object mg extends SimpleMassUnit("milligram", micro)
  case object g extends SimpleMassUnit("gram", milli)
  case object kg extends SimpleMassUnit("kilogram", r"1")

  case object t extends SimpleMassUnit("ton", kilo)
}

