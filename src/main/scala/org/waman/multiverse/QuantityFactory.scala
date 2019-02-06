package org.waman.multiverse

import org.waman.multiverse.units._
import spire.math.Fractional

class QuantityFactory[A: Fractional](val value: A){

  def apply(unit: LengthUnit) = new Length(value, unit)
  def apply(unit: TimeUnit) = new Time(value, unit)
  def apply(unit: VelocityUnit) = new Velocity(value, unit)
}