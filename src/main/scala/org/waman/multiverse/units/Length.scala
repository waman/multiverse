package org.waman.multiverse.units

import org.waman.multiverse._
import spire.math.{Fractional, Real}

/*
 * +++++ For Length +++++
 * 1.0 (m) ( = 1.0.apply(m)) --- LengthUnitInterpreter.apply(LengthUnit)
 * length (m) ( = length.apply(m)) --- Length.apply(LengthUnit)
 *
 * +++++ For Velocity +++++
 * 1.0 (m/s) ( = 1.0.apply(m./(s))) --- LengthUnit./(TimeUnit): VelocityUnit, LengthUnitInterpreter.apply(VelocityUnit)
 * velocity (m/s) ( = velocity.apply(m./(s))) --- LengthUnit./(TimeUnit): VelocityUnit, Velocity.apply(VelocityUnit)
 */

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends LinearQuantity[A, LengthUnit]{
}

trait LengthFactory[A]{

  def apply(unit: LengthUnit): Length[A]

//  // Length / Time -> Velocity
//  // Length / TimeSquared -> Acceleration
//  def apply(velocityUnit: VelocityUnit): Velocity[A]
//  def apply(accelerationUnit: AccelerationUnit): Acceleration[A]
}

trait LengthUnit extends PhysicalUnit[LengthUnit]

class SimpleLengthUnit(val name: String, val unitValueInSIUnit: Real) extends LengthUnit