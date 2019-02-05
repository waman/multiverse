package org.waman.multiverse.units

import org.waman.multiverse._
import spire.math.{Fractional, Real}
import org.waman.multiverse.predef.LengthUnits.m

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends LinearQuantity[A, LengthUnit]{
}

trait LengthFactory[A]{
  def apply(unit: LengthUnit): Length[A]
}

trait LengthUnit extends PhysicalUnit[LengthUnit]{
  override protected def getSIUnit: LengthUnit = m
  def /(timeUnit: TimeUnit): VelocityUnit = new LengthPerTimeVelocityUnit(this, timeUnit)
}

class SimpleLengthUnit(val name: String, val unitValueInSIUnit: Real) extends LengthUnit