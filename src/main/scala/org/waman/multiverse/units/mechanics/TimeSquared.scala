package org.waman.multiverse.units.mechanics

import org.waman.multiverse._
import org.waman.multiverse.predef.mechanics.TimeSquaredUnits
import org.waman.multiverse.units.basic.TimeUnit
import spire.math.{Fractional, Real}

class TimeSquared[A: Fractional](val value: A, val unit: TimeSquaredUnit)
    extends LinearQuantity[A, TimeSquaredUnit]

trait TimeSquaredUnit extends PhysicalUnit[TimeSquaredUnit] {
  override def getSIUnit: TimeSquaredUnit = TimeSquaredUnits.s2
}

class TimeSquared_TimeSquaredUnit(timeUnit: TimeUnit) extends TimeSquaredUnit {

  override val name: String = s"${timeUnit.name} squared"

  override val unitValueInSIUnit: Real = timeUnit.unitValueInSIUnit**2

  override protected def extractSymbol: String = s"${timeUnit.symbol}²"
}

class TimeTimesTime_TimeSquaredUnit(val firstUnit: TimeUnit, val secondUnit: TimeUnit)
  extends ProductUnit[TimeSquaredUnit, TimeUnit, TimeUnit]
    with TimeSquaredUnit