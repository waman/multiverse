package org.waman.multiverse.units.basic

import org.waman.multiverse._
import org.waman.multiverse.predef.basic.LengthUnits.m
import org.waman.multiverse.predef.basic.TimeUnits.s
import org.waman.multiverse.units.mechanics.{AccelerationUnit, VelocityPerTime_AccelerationUnit}
import spire.math.{Fractional, Real}

class Velocity[A: Fractional](val value: A, val unit: VelocityUnit)
    extends LinearQuantity[A, VelocityUnit]

trait VelocityUnit extends PhysicalUnit[VelocityUnit] {
  override def getSIUnit: VelocityUnit = m/s

  def /(timeUnit: TimeUnit): AccelerationUnit = new VelocityPerTime_AccelerationUnit(this, timeUnit)
}

class SimpleVelocityUnit(val name: String, val unitValueInSIUnit: Real) extends VelocityUnit

class LengthPerTime_VelocityUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeUnit)
    extends QuotientUnit[VelocityUnit, LengthUnit, TimeUnit]
      with VelocityUnit