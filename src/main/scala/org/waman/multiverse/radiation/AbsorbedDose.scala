package org.waman.multiverse.radiation

import org.waman.multiverse._
import org.waman.multiverse.energy.{EnergyPer, EnergyPostfixOps, EnergyUnit}
import org.waman.multiverse.mass.{MassPostfixOps, MassUnit}
import spire.implicits._
import spire.math.Fractional

class AbsorbedDose[A: Fractional](val value: A, val unit: AbsorbedDoseUnit)
  extends Quantity[A, AbsorbedDoseUnit]
    with AbsorbedDosePostfixOps[A]
    with EnergyPostfixOps[DivisibleByMassUnit[A]]
    with EnergyPer[MassPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AbsorbedDoseUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInGray) / real(evalUnit.unitInGray)

  override protected def absorbedDosePostfixOps(absorbedDoseUnit: AbsorbedDoseUnit) = apply(absorbedDoseUnit)

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = new DivisibleByMassUnit[A]{
    override def /(massUnit: MassUnit) = apply(energyUnit/ massUnit)
  }

  override protected def energyPer(energyUnit: EnergyUnit) = new MassPostfixOps[A]{
    override protected def massPostfixOps(massUnit: MassUnit) = apply(energyUnit / massUnit)
  }
}

trait AbsorbedDoseFactory[A]
    extends AbsorbedDosePostfixOps[AbsorbedDose[A]]{

  def apply(unit: AbsorbedDoseUnit): AbsorbedDose[A]

  override protected def absorbedDosePostfixOps(absorbedDoseUnit: AbsorbedDoseUnit) =
    apply(absorbedDoseUnit)
}