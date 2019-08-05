package org.waman.multiverse.units.mechanics

import org.waman.multiverse._
import org.waman.multiverse.predef.mechanics.TimeSquaredUnits
import org.waman.multiverse.units.basic.TimeUnit
import spire.math.{Fractional, Real}

class TimeSquared[A: Fractional](val value: A, val unit: TimeSquaredUnit)
    extends ScaleQuantity[A, TimeSquaredUnit]

trait TimeSquaredUnit extends LinearUnit[TimeSquaredUnit] {
  override def getSIUnit: TimeSquaredUnit = TimeSquaredUnits.`s²`
}

class TimeSquared_TimeSquaredUnit(timeUnit: TimeUnit) extends TimeSquaredUnit{

  override val name: String = s"${timeUnit.name} squared"
  override lazy val symbol: String = s"${timeUnit.symbol}²"
  override val intervalInSIUnit: Real = timeUnit.intervalInSIUnit**2
  override val aliases: Seq[String] = Nil
}

class TimeTimesTime_TimeSquaredUnit(val firstUnit: TimeUnit, val secondUnit: TimeUnit)
  extends ProductUnit[TimeSquaredUnit, TimeUnit, TimeUnit]
    with TimeSquaredUnit