package org.waman.multiverse.units.basic

import org.waman.multiverse._
import org.waman.multiverse.predef.BasicUnits.m
import spire.math.{Fractional, Real}

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends ExtensiveQuantity[Length[A], A, LengthUnit] {

  override protected def newQuantity(value: A, unit: LengthUnit): Length[A] =
    new Length(value, unit)
}

trait LengthUnit extends PhysicalUnit[LengthUnit]{

  override def getSIUnit: LengthUnit = m

  def /(timeUnit: TimeUnit): LengthPerTimeVelocityUnit = new LengthPerTimeVelocityUnit(this, timeUnit)
}

class SimpleLengthUnit(val name: String, val unitValueInSIUnit: Real) extends LengthUnit