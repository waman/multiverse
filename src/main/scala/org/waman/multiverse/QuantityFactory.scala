package org.waman.multiverse

import org.waman.multiverse.metric._
import spire.math.Fractional

class QuantityFactory[A: Fractional](protected val value: A)
  extends LengthFactory[A]{

  override def apply(unit: LengthUnit) = new Length(value, unit)
}