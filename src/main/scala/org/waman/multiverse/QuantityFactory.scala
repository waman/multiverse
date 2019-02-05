package org.waman.multiverse

import org.waman.multiverse.units._
import spire.math.Fractional

class QuantityFactory[A: Fractional](val value: A)
  extends LengthFactory[A]
    with TimeFactory[A]
    with VelocityFactory[A]{

  override def apply(unit: LengthUnit) = new Length(value, unit)
  override def apply(unit: TimeUnit) = new Time(value, unit)
  override def apply(unit: VelocityUnit) = new Velocity(value, unit)
}