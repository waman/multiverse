package org.waman.multiverse.unit

import org.waman.multiverse.unit.thermodynamics._
import scala.reflect.runtime.{universe => ru}

object ThermalUnits extends PhysicalUnitPredefProxy {

  // Temperature Units
  def K: TemperatureUnit = TemperatureUnits.K

  def `°C` : TemperatureUnit = TemperatureUnits.`°C`
  /** Equivalent to `°C`*/
  def ℃ : TemperatureUnit = TemperatureUnits.`°C`
  /** Equivalent to `°C`*/
  def degC: TemperatureUnit = TemperatureUnits.`°C`

  override protected def getUnitsType: ru.Type = ru.typeOf[this.type]
}
