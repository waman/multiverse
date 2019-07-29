package org.waman.multiverse.units.thermal

import org.waman.multiverse._
import org.waman.multiverse.predef.thermal.TemperatureUnits
import spire.math.{Fractional, Real}

class Temperature[A: Fractional](val value: A, val unit: TemperatureUnit)
    extends Quantity[A, TemperatureUnit]

trait TemperatureUnit extends PhysicalUnit[TemperatureUnit]{

  override def getSIUnit: TemperatureUnit = TemperatureUnits.K
}

class SimpleTemperatureUnit(val name: String, val zeroInSIUnit: Real, val intervalInSIUnit: Real)
  extends TemperatureUnit with SymbolByClassName[TemperatureUnit]