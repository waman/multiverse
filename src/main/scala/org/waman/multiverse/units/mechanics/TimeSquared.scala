package org.waman.multiverse.units.mechanics

import org.waman.multiverse._
import org.waman.multiverse.predef.mechanics.TimeSquaredUnits
import spire.math.{Fractional, Real}

class TimeSquared[A: Fractional](val value: A, val unit: TimeSquaredUnit)
    extends LinearQuantity[A, TimeSquaredUnit]

trait TimeSquaredUnit extends PhysicalUnit[TimeSquaredUnit] {
  override def getSIUnit: TimeSquaredUnit = TimeSquaredUnits.s2
}

class SimpleTimeSquaredUnit(val name: String, val unitValueInSIUnit: Real) extends TimeSquaredUnit {
  override protected def extractSymbol: String = {
    val s = super.extractSymbol
    s"${s.substring(0, s.length-1)}²"
  }
}