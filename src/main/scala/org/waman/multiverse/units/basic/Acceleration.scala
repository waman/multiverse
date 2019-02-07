package org.waman.multiverse.units.basic

import org.waman.multiverse._
import spire.math.{Fractional, Real}
import org.waman.multiverse.predef.BasicUnits.{m, s2}

class Acceleration[A: Fractional](val value: A, val unit: AccelerationUnit)
    extends LinearQuantity[A, AccelerationUnit]

trait AccelerationUnit extends PhysicalUnit[AccelerationUnit] {
  override def getSIUnit: AccelerationUnit = m/s2
}

class SimpleAccelerationUnit(val name: String, val unitValueInSIUnit: Real) extends AccelerationUnit

class LengthPerTimeSquaredAccelerationUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeSquaredUnit)
    extends QuotientUnit[AccelerationUnit, LengthUnit, TimeSquaredUnit] with AccelerationUnit{

  override val unitValueInSIUnit: Real = numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
}