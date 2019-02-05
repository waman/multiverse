package org.waman.multiverse.units

import org.waman.multiverse._
import spire.math.{Fractional, Real}
import org.waman.multiverse.predef.TimeUnits.s

class Time[A: Fractional](val value: A, val unit: TimeUnit)
    extends LinearQuantity[A, TimeUnit]{
}

trait TimeFactory[A]{
  def apply(unit: TimeUnit): Time[A]
}

trait TimeUnit extends PhysicalUnit[TimeUnit] {
  override protected def getSIUnit: TimeUnit = s
}

class SimpleTimeUnit(val name: String, val unitValueInSIUnit: Real) extends TimeUnit