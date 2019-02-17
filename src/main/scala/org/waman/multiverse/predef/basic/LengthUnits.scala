package org.waman.multiverse.predef.basic

import org.waman.multiverse.MetricPrefixes._
import org.waman.multiverse.units.basic.SimpleLengthUnit
import spire.implicits._

trait LengthUnits{

  final case object ym extends SimpleLengthUnit("yoctometre", yocto)
  final case object zm extends SimpleLengthUnit("zeptometre", zepto)
  final case object am extends SimpleLengthUnit("attometre", atto)
  final case object fm extends SimpleLengthUnit("femtometre", femto)
  final case object pm extends SimpleLengthUnit("picometre", pico)
  final case object nm extends SimpleLengthUnit("nanometre", nano)
  final case object μm extends SimpleLengthUnit("micrometre", micro)
  final case object mm extends SimpleLengthUnit("millimetre", milli)
  final case object cm extends SimpleLengthUnit("centimetre", centi)
  final case object m extends SimpleLengthUnit("metre", r"1")
  final case object km extends SimpleLengthUnit("kilometre", kilo)
}