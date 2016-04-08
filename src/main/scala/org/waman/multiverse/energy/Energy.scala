package org.waman.multiverse.energy

import org.waman.multiverse._
import org.waman.multiverse.mass.{DivisibleByMassUnit, MassPostfixOps, MassUnit}
import org.waman.multiverse.radiation.{AbsorbedDose, AbsorbedDoseUnit}
import org.waman.multiverse.thermal._
import org.waman.multiverse.time.{MultiplicativeByTimeUnit, TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

class Energy[A: Fractional](val value: A, val unit: EnergyUnit)
  extends Quantity[A, EnergyUnit]
    with EnergyPostfixOps[A]
    with MultiplicativeByTimeUnit[Action[A]]
    with DivisibleByTemperatureUnit[Entropy[A]]
    with DivisibleByMassUnit[AbsorbedDose[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: EnergyUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = apply(energyUnit)

  override def *(timeUnit: TimeUnit) = new Action(value, unit * timeUnit)

  override def /(temperatureUnit: TemperatureUnit) = new Entropy(value, unit / temperatureUnit)

  override def /(massUnit: MassUnit) = new AbsorbedDose(value, unit / massUnit)
}

trait EnergyFactory[A]
    extends EnergyPostfixOps[Energy[A]]
    with EnergyDot[TimePostfixOps[Action[A]]]
    with EnergyPer[TemperaturePostfixOps[Entropy[A]] with MassPostfixOps[AbsorbedDose[A]]]{

  def apply(unit: EnergyUnit): Energy[A]

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = apply(energyUnit)

  // Energy * Time -> Action
  def apply(unit: ActionUnit): Action[A]

  override protected def energyDot(energyUnit: EnergyUnit) = new TimePostfixOps[Action[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(energyUnit * timeUnit)
  }

  // Energy / Temperature -> Entropy
  // Energy / Mass -> AbsorbedDose
  def apply(unit: EntropyUnit): Entropy[A]
  def apply(unit: AbsorbedDoseUnit): AbsorbedDose[A]

  override protected def energyPer(energyUnit: EnergyUnit) =
    new TemperaturePostfixOps[Entropy[A]] with MassPostfixOps[AbsorbedDose[A]]{
      override protected def temperaturePostfixOps(temperatureUnit: TemperatureUnit) =
        apply(energyUnit / temperatureUnit)

      override protected def massPostfixOps(massUnit: MassUnit) =
        apply(energyUnit / massUnit)
    }
}