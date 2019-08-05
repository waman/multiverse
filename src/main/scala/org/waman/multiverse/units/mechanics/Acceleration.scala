package org.waman.multiverse.units.mechanics

import org.waman.multiverse._
import org.waman.multiverse.predef.basic.LengthUnits.m
import org.waman.multiverse.predef.mechanics.TimeSquaredUnits.`s²`
import org.waman.multiverse.units.basic.{LengthUnit, TimeUnit, VelocityUnit}
import spire.math.{Fractional, Real}

class Acceleration[A: Fractional](val value: A, val unit: AccelerationUnit)
    extends ScaleQuantity[A, AccelerationUnit]

trait AccelerationUnit extends LinearUnit[AccelerationUnit] {
  override def getSIUnit: AccelerationUnit = m/`s²`
}

class SimpleAccelerationUnit(val name: String, val intervalInSIUnit: Real) extends AccelerationUnit with SymbolByClassName[AccelerationUnit]

class LengthPerTimeSquared_AccelerationUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeSquaredUnit)
    extends QuotientUnit[AccelerationUnit, LengthUnit, TimeSquaredUnit]
      with AccelerationUnit

class VelocityPerTime_AccelerationUnit(val numeratorUnit: VelocityUnit, val denominatorUnit: TimeUnit)
  extends QuotientUnit[AccelerationUnit, VelocityUnit, TimeUnit]
    with AccelerationUnit