package org.waman.multiverse.units.mechanics

import org.waman.multiverse._
import org.waman.multiverse.predef.basic.LengthUnits.m
import org.waman.multiverse.predef.mechanics.TimeSquaredUnits.s2
import org.waman.multiverse.units.basic.LengthUnit
import spire.math.{Fractional, Real}

class Acceleration[A: Fractional](val value: A, val unit: AccelerationUnit)
    extends LinearQuantity[A, AccelerationUnit]

trait AccelerationUnit extends PhysicalUnit[AccelerationUnit] {
  override def getSIUnit: AccelerationUnit = m/s2
}

class SimpleAccelerationUnit(val name: String, val unitValueInSIUnit: Real) extends AccelerationUnit

class LengthPerTimeSquaredAccelerationUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeSquaredUnit)
    extends QuotientUnit[AccelerationUnit, LengthUnit, TimeSquaredUnit]
      with AccelerationUnit