package org.waman.multiverse.predef.basic

import org.waman.multiverse.MetricPrefixes._
import org.waman.multiverse.predef.PhysicalUnitPredef
import org.waman.multiverse.units.basic.{SimpleTimeUnit, TimeUnit}
import spire.implicits._
import spire.math.Real

import scala.reflect.runtime.{universe => ru}

object TimeUnits extends PhysicalUnitPredef[TimeUnit]{

  final case object ys extends SimpleTimeUnit("yoctosecond", yocto[Real])
  final case object zs extends SimpleTimeUnit("zeptosecond", zepto[Real])
  final case object as extends SimpleTimeUnit("attosecond", atto[Real])
  final case object fs extends SimpleTimeUnit("femtosecond", femto[Real])
  final case object ps extends SimpleTimeUnit("picosecond", pico[Real])
  final case object ns extends SimpleTimeUnit("nanosecond", nano[Real])
  final case object μs extends SimpleTimeUnit("microsecond", micro[Real])
  final case object ms extends SimpleTimeUnit("millisecond", milli[Real])

  final case object s extends SimpleTimeUnit("second", r"1")
  final case object min extends SimpleTimeUnit("minite", r"60")
  final case object h extends SimpleTimeUnit("hour", r"3600")

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}
