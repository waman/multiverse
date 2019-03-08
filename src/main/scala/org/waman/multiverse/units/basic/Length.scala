package org.waman.multiverse.units.basic

import org.waman.multiverse._
import org.waman.multiverse.predef.basic.LengthUnits
import org.waman.multiverse.units.mechanics.{AccelerationUnit, LengthPerTimeSquaredAccelerationUnit, TimeSquaredUnit}
import spire.math.{Fractional, Real}

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends ExtensiveQuantity[Length[A], A, LengthUnit] {

  override protected def newQuantity(value: A, unit: LengthUnit): Length[A] =
    new Length(value, unit)
}

trait LengthUnit extends PhysicalUnit[LengthUnit]{

  override def getSIUnit: LengthUnit = LengthUnits.m

  def /(timeUnit: TimeUnit): LengthPerTimeVelocityUnit = new LengthPerTimeVelocityUnit(this, timeUnit)
  def /(timeUnit: TimeSquaredUnit): AccelerationUnit = new LengthPerTimeSquaredAccelerationUnit(this, timeUnit)
}

class SimpleLengthUnit(val name: String, val unitValueInSIUnit: Real) extends LengthUnit