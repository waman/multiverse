package org.waman.multiverse.units.basic

import org.waman.multiverse._
import spire.math.{Fractional, Real}
import org.waman.multiverse.predef.BasicUnits.s

class Time[A: Fractional](val value: A, val unit: TimeUnit)
    extends ExtensiveQuantity[Time[A], A, TimeUnit]{

  override protected def newQuantity(value: A, unit: TimeUnit): Time[A] =
    new Time(value, unit)
}

trait TimeUnit extends PhysicalUnit[TimeUnit] {
  override def getSIUnit: TimeUnit = s
}

class SimpleTimeUnit(val name: String, val unitValueInSIUnit: Real) extends TimeUnit