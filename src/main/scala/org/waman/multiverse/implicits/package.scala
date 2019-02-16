package org.waman.multiverse

import org.waman.multiverse.units.basic._
import org.waman.multiverse.units.mechanics.{Acceleration, AccelerationUnit}
import spire.math.{Fractional, Real, SafeLong}

import scala.language.implicitConversions

package object implicits {

  implicit class QuantityFactory[A: Fractional](val value: A){

    def apply(unit: LengthUnit): Length[A] = new Length(value, unit)
    def apply(unit: MassUnit): Mass[A] = new Mass(value, unit)
    def apply(unit: TimeUnit): Time[A] = new Time(value, unit)
    def apply(unit: VelocityUnit): Velocity[A] = new Velocity(value, unit)
    def apply(unit: AccelerationUnit): Acceleration[A] = new Acceleration(value, unit)
  }

//  implicit def convertFractionalToQuantityFactory[A: Fractional](value: A): QuantityFactory[A] =
//    new QuantityFactory(value)

  // Integral value (like 1(m), not 1.0(m)) create a Quantity[Real] instance
  implicit def convertIntToQuantityFactory(value: Int): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertLongToQuantityFactory(value: Long): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertSafeLongToQuantityFactory(value: SafeLong): QuantityFactory[Real] =
    new QuantityFactory(Real(value))

  implicit def convertBigIntToQuantityFactory(value: BigInt): QuantityFactory[Real] =
    new QuantityFactory(Real(value))
}
