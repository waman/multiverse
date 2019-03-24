package org.waman.multiverse.units.basic

import org.waman.multiverse._
import org.waman.multiverse.predef.basic.LengthUnits
import org.waman.multiverse.units.mechanics.{AccelerationUnit, LengthPerTimeSquared_AccelerationUnit, TimeSquaredUnit}
import spire.math.{Fractional, Real}

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends ExtensiveQuantity[Length[A], A, LengthUnit] {

  override protected def newQuantity(value: A, unit: LengthUnit): Length[A] =
    new Length(value, unit)
}

trait LengthUnit extends ScaleUnit[LengthUnit]{

  override def getSIUnit: LengthUnit = LengthUnits.m

  def /(timeUnit: TimeUnit): LengthPerTime_VelocityUnit = new LengthPerTime_VelocityUnit(this, timeUnit)
  def /(timeUnit: TimeSquaredUnit): AccelerationUnit = new LengthPerTimeSquared_AccelerationUnit(this, timeUnit)
}

class SimpleLengthUnit(val name: String, val intervalInSIUnit: Real) extends LengthUnit