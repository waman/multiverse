package org.waman.multiverse.thermal

import org.waman.multiverse._
import org.waman.multiverse.energy.{EnergyPer, EnergyPostfixOps, EnergyUnit}
import spire.implicits._
import spire.math.Fractional

class Entropy[A: Fractional](val value: A, val unit: EntropyUnit)
  extends Quantity[A, EntropyUnit]
    with EntropyPostfixOps[A]
    with EnergyPostfixOps[DivisibleByTemperatureUnit[A]]
    with EnergyPer[TemperaturePostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: EntropyUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def entropyPostfixOps(entropyUnit: EntropyUnit) = apply(entropyUnit)

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = new  DivisibleByTemperatureUnit[A]{
    override def /(temperatureUnit: TemperatureUnit) = apply(energyUnit / temperatureUnit)
  }

  override protected def energyPer(energyUnit: EnergyUnit) = new TemperaturePostfixOps[A]{
    override protected def temperaturePostfixOps(temperatureUnit: TemperatureUnit) =
      apply(energyUnit / temperatureUnit)
  }
}

trait EntropyFactory[A]
    extends EntropyPostfixOps[Entropy[A]]{

  def apply(unit: EntropyUnit): Entropy[A]

  override protected def entropyPostfixOps(entropyUnit: EntropyUnit) =
    apply(entropyUnit)
}