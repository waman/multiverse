package org.waman.multiverse.predef.basic

import org.waman.multiverse.MetricPrefixes._
import org.waman.multiverse.predef.PhysicalUnitPredef
import org.waman.multiverse.units.basic.{LengthUnit, SimpleLengthUnit}
import spire.implicits._
import spire.math.Real

import scala.reflect.runtime.{universe => ru}

object LengthUnits extends PhysicalUnitPredef[LengthUnit]{

  final case object ym extends SimpleLengthUnit("yoctometre", yocto[Real])
  final case object zm extends SimpleLengthUnit("zeptometre", zepto[Real])
  final case object am extends SimpleLengthUnit("attometre", atto[Real])
  final case object fm extends SimpleLengthUnit("femtometre", femto[Real])
  final case object pm extends SimpleLengthUnit("picometre", pico[Real])
  final case object nm extends SimpleLengthUnit("nanometre", nano[Real])
  final case object μm extends SimpleLengthUnit("micrometre", micro[Real])
  final case object mm extends SimpleLengthUnit("millimetre", milli[Real])
  final case object cm extends SimpleLengthUnit("centimetre", centi[Real])
  final case object m extends SimpleLengthUnit("metre", r"1")
  final case object km extends SimpleLengthUnit("kilometre", kilo[Real])

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}