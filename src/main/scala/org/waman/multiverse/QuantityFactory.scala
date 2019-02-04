package org.waman.multiverse

import org.waman.multiverse.units._
import spire.math.Fractional

class QuantityFactory[A: Fractional](protected val value: A)
  extends LengthFactory[A]
  with TimeFactory[A]{

  override def apply(unit: LengthUnit) = new Length(value, unit)
  override def apply(unit: TimeUnit) = new Time(value, unit)
}