package org.waman.multiverse.mechanics

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait EnergyPostfixOps[A]{

  import EnergyUnit._

  protected def energyPostfixOps(energyUnit: EnergyUnit): A

  def J: A = energyPostfixOps(Joule)
  def eV: A = energyPostfixOps(ElectronVolt)
}

trait EnergyPer[A]{
  import EnergyUnit._

  protected def energyPer(energyUnit: EnergyUnit): A

  def J(per: Per): A = energyPer(Joule)
  def eV(per: Per): A = energyPer(ElectronVolt)
}

class Energy[A: Fractional](val value: A, val unit: EnergyUnit)
  extends Quantity[A, EnergyUnit]
    with EnergyPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: EnergyUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInJoule) / real(evalUnit.unitInJoule)


  override protected def energyPostfixOps(energyUnit: EnergyUnit) = apply(energyUnit)
}

abstract class EnergyUnit(val symbol: String, val unitInJoule: Real)
  extends PhysicalUnit[EnergyUnit]{

  def this(symbol: String, factor: Real, energyUnit: EnergyUnit) =
    this(symbol, factor * energyUnit.unitInJoule)

  override val baseUnit = EnergyUnit.Joule
  override val inBaseUnitAccessor = () => unitInJoule
}

object EnergyUnit{

  case object Joule extends EnergyUnit("J", 1)

  case object ElectronVolt extends EnergyUnit("eV", r"1.60217656535") with NotExact
}

trait PredefinedEnergyUnit extends EnergyPostfixOps[EnergyUnit]{

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = energyUnit
}

object PredefinedEnergyUnit extends PredefinedEnergyUnit

trait EnergyUnitInterpreter[A]
  extends EnergyPostfixOps[Energy[A]]{

  def apply(unit: EnergyUnit): Energy[A]

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = apply(energyUnit)
}