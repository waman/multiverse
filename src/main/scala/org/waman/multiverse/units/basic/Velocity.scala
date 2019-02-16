package org.waman.multiverse.units.basic

import org.waman.multiverse._
import org.waman.multiverse.predef.BasicUnits._
import org.waman.multiverse.units.mechanics.{AccelerationUnit, VelocityPerTimeAccelerationUnit}
import spire.math.{Fractional, Real}

class Velocity[A: Fractional](val value: A, val unit: VelocityUnit)
    extends LinearQuantity[A, VelocityUnit]

trait VelocityUnit extends PhysicalUnit[VelocityUnit] {
  override def getSIUnit: VelocityUnit = m/s

  def /(timeUnit: TimeUnit): AccelerationUnit = new VelocityPerTimeAccelerationUnit(this, timeUnit)
}

class SimpleVelocityUnit(val name: String, val unitValueInSIUnit: Real) extends VelocityUnit

class LengthPerTimeVelocityUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeUnit)
    extends QuotientUnit[VelocityUnit, LengthUnit, TimeUnit]
      with VelocityUnit{

  override val unitValueInSIUnit: Real = numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit

  def ^(n: Int): AccelerationUnit = {
    require(n == 2)
    this/this.denominatorUnit
  }
}