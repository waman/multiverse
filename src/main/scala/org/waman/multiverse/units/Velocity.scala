package org.waman.multiverse.units

import org.waman.multiverse._
import spire.math.{Fractional, Real}
import org.waman.multiverse.predef.LengthUnits.m
import org.waman.multiverse.predef.TimeUnits.s

class Velocity[A: Fractional](val value: A, val unit: VelocityUnit)
    extends LinearQuantity[A, VelocityUnit]

trait VelocityUnit extends PhysicalUnit[VelocityUnit] {
  override def getSIUnit: VelocityUnit = m/s
}

class SimpleVelocityUnit(val name: String, val unitValueInSIUnit: Real) extends VelocityUnit

class LengthPerTimeVelocityUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeUnit)
    extends QuotientUnit[VelocityUnit, LengthUnit, TimeUnit] with VelocityUnit{

  override val unitValueInSIUnit: Real = numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
}