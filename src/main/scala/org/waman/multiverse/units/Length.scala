package org.waman.multiverse.units

import org.waman.multiverse._
import spire.math.{Fractional, Real}
import org.waman.multiverse.predef.LengthUnits.m

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends ExtensiveQuantity[Length[A], A, LengthUnit] {

  override protected def newQuantity(value: A, unit: LengthUnit): Length[A] =
    new Length(value, unit)
}

trait LengthUnit extends PhysicalUnit[LengthUnit]{

  override def getSIUnit: LengthUnit = m

  def /(timeUnit: TimeUnit): VelocityUnit = new LengthPerTimeVelocityUnit(this, timeUnit)
}

class SimpleLengthUnit(val name: String, val unitValueInSIUnit: Real) extends LengthUnit