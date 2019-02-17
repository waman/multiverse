package org.waman.multiverse.predef.basic

import org.waman.multiverse.MetricPrefixes._
import org.waman.multiverse.units.basic.SimpleMassUnit
import spire.implicits._

trait MassUnits{

  // NOTE: SI Unit is 'kilo'gram
  final case object yg extends SimpleMassUnit("yoctogram", yocto/r"1000")
  final case object zg extends SimpleMassUnit("zeptogram", yocto)
  final case object ag extends SimpleMassUnit("attogram", zepto)
  final case object fg extends SimpleMassUnit("femtogram", atto)
  final case object pg extends SimpleMassUnit("picogram", femto)
  final case object ng extends SimpleMassUnit("nanogram", pico)
  final case object μg extends SimpleMassUnit("microgram", nano)
  final case object mg extends SimpleMassUnit("milligram", micro)
  final case object g extends SimpleMassUnit("gram", milli)
  final case object kg extends SimpleMassUnit("kilogram", r"1")

  final case object t extends SimpleMassUnit("ton", kilo)
}

