package org.waman.multiverse.units.basic

import org.waman.multiverse._
import org.waman.multiverse.predef.basic.TimeUnits
import org.waman.multiverse.units.mechanics.{TimeSquaredUnit, TimeSquared_TimeSquaredUnit, TimeTimesTime_TimeSquaredUnit}
import spire.math.{Fractional, Real}

class Time[A: Fractional](val value: A, val unit: TimeUnit)
    extends ExtensiveQuantity[Time[A], A, TimeUnit]{

  override protected def newQuantity(value: A, unit: TimeUnit): Time[A] =
    new Time(value, unit)
}

trait TimeUnit extends PhysicalUnit[TimeUnit] with CanSquare[TimeSquaredUnit]{
  override def getSIUnit: TimeUnit = TimeUnits.s

  def *(timeUnit: TimeUnit): TimeSquaredUnit =
    if(this == timeUnit) square
    else new TimeTimesTime_TimeSquaredUnit(this, timeUnit)

  override def square: TimeSquaredUnit = new TimeSquared_TimeSquaredUnit(this)
}

class SimpleTimeUnit(val name: String, val unitValueInSIUnit: Real) extends TimeUnit {
  def this(name: String, factor: Real, u: TimeUnit) = this(name, factor*u.unitValueInSIUnit)
}