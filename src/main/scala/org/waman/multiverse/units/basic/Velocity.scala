package org.waman.multiverse.units.basic

import org.waman.multiverse._
import org.waman.multiverse.predef.basic.LengthUnits.m
import org.waman.multiverse.predef.basic.TimeUnits.s
import spire.math.{Fractional, Real}

class Velocity[A: Fractional](val value: A, val unit: VelocityUnit)
    extends LinearQuantity[A, VelocityUnit]

trait VelocityUnit extends PhysicalUnit[VelocityUnit] {
  override def getSIUnit: VelocityUnit = m/s
}

class SimpleVelocityUnit(val name: String, val unitValueInSIUnit: Real) extends VelocityUnit

class LengthPerTimeVelocityUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeUnit)
    extends QuotientUnit[VelocityUnit, LengthUnit, TimeUnit]
      with VelocityUnit