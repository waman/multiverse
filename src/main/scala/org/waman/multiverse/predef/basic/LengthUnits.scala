package org.waman.multiverse.predef.basic

import org.waman.multiverse.MetricPrefixes._
import org.waman.multiverse.units.basic.SimpleLengthUnit
import spire.implicits._

trait LengthUnits{

  case object ym extends SimpleLengthUnit("yoctometre", yocto)
  case object zm extends SimpleLengthUnit("zeptometre", zepto)
  case object am extends SimpleLengthUnit("attometre", atto)
  case object fm extends SimpleLengthUnit("femtometre", femto)
  case object pm extends SimpleLengthUnit("picometre", pico)
  case object nm extends SimpleLengthUnit("nanometre", nano)
  case object μm extends SimpleLengthUnit("micrometre", micro)
  case object mm extends SimpleLengthUnit("millimetre", milli)
  case object cm extends SimpleLengthUnit("centimetre", centi)
  case object m extends SimpleLengthUnit("metre", r"1")
  case object km extends SimpleLengthUnit("kilometre", kilo)
}

object LengthUnits extends LengthUnits