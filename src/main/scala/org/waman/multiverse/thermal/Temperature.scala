package org.waman.multiverse.thermal

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait TemperaturePostfixOps[A]{

  import TemperatureUnit._

  protected def temperaturePostfixOps(temperatureUnit: TemperatureUnit): A

  def K: A = temperaturePostfixOps(Kelvin)
}

class Temperature[A: Fractional](val value: A, val unit: TemperatureUnit)
  extends Quantity[A, TemperatureUnit]
    with TemperaturePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: TemperatureUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInKelvin) / real(evalUnit.unitInKelvin)

  override protected def temperaturePostfixOps(temperatureUnit: TemperatureUnit) = apply(temperatureUnit)
}

sealed abstract class TemperatureUnit(val symbol: String, val unitInKelvin: Real)
    extends PhysicalUnit[TemperatureUnit]{

  override def baseUnit = TemperatureUnit.Kelvin
  override def valueInBaseUnit = unitInKelvin
}

object TemperatureUnit extends ConstantsDefined[TemperatureUnit]{

  case object Kelvin extends TemperatureUnit("K", 1)

  override lazy val values = Seq(
    Kelvin
  )
}

trait PredefinedTemperatureUnit extends TemperaturePostfixOps[TemperatureUnit]{

  override protected def temperaturePostfixOps(temperatureUnit: TemperatureUnit) = temperatureUnit
}

object PredefinedTemperatureUnit extends PredefinedTemperatureUnit

trait TemperatureFactory[A]
    extends TemperaturePostfixOps[Temperature[A]]{

  def apply(unit: TemperatureUnit): Temperature[A]

  override protected def temperaturePostfixOps(temperatureUnit: TemperatureUnit) =
    apply(temperatureUnit)
}