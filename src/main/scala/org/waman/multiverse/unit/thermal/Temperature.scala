package org.waman.multiverse.unit.thermal

import org.waman.multiverse._
import org.waman.multiverse.unit.PhysicalUnitPredef
import spire.math._
import spire.implicits._

class Temperature[A: Fractional](val value: A, val unit: TemperatureUnit)
    extends HomogeneousQuantity[A, TemperatureUnit]

trait TemperatureUnit extends HomogeneousUnit[TemperatureUnit]{

  override def getSIUnit: TemperatureUnit = TemperatureUnits.K
}

class SimpleTemperatureUnit(val name: String, val zero: Real, val interval: Real)
  extends TemperatureUnit with SymbolByClassName[TemperatureUnit]


import scala.reflect.runtime.{universe => ru}

object TemperatureUnits extends PhysicalUnitPredef[TemperatureUnit]{

  final case object K extends SimpleTemperatureUnit("Kelvin", 0, 1)
  final case object `°C` extends SimpleTemperatureUnit("Celsius", r"273.15", 1)
  final case object `°F` extends SimpleTemperatureUnit("Fahrenheit", r"273.15" - r"5/9" * 32, r"5/9")

  final case object `°R` extends SimpleTemperatureUnit("Rankine", 0, r"5/9")
  final case object `°De` extends SimpleTemperatureUnit("Delisle", r"373.15", -r"2/3")
  final case object `°N` extends SimpleTemperatureUnit("Newton", r"273.15", r"100/33")
  final case object `°Ré` extends SimpleTemperatureUnit("Réaumur", r"273.15", r"5/4")
  final case object `°Rø` extends SimpleTemperatureUnit("Rømer", r"273.15" - r"7.5" * r"40/21", r"40/21")

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}