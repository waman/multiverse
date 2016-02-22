package org.waman.multiverse.thermal

import org.waman.multiverse._
import org.waman.multiverse.energy.{EnergyPer, EnergyPostfixOps, EnergyUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait EntropyPostfixOps[A]{

  import EntropyUnit._

  protected def entropyPostfixOps(entropyUnit: EntropyUnit): A

  def bit: A = entropyPostfixOps(Bit)
}

class Entropy[A: Fractional](val value: A, val unit: EntropyUnit)
  extends Quantity[A, EntropyUnit]
    with EntropyPostfixOps[A]
    with EnergyPostfixOps[DivisibleByTemperatureUnit[A]]
    with EnergyPer[TemperaturePostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: EntropyUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInJoulePerKelvin) / real(evalUnit.unitInJoulePerKelvin)

  override protected def entropyPostfixOps(entropyUnit: EntropyUnit) = apply(entropyUnit)

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = new  DivisibleByTemperatureUnit[A]{
    override def /(temperatureUnit: TemperatureUnit) = apply(energyUnit / temperatureUnit)
  }

  override protected def energyPer(energyUnit: EnergyUnit) = new TemperaturePostfixOps[A]{
    override protected def temperaturePostfixOps(temperatureUnit: TemperatureUnit) =
      apply(energyUnit / temperatureUnit)
  }
}

sealed trait EntropyUnit extends PhysicalUnit[EntropyUnit]{

  def unitInJoulePerKelvin: Real

  override lazy val baseUnit = EnergyUnit.Joule / TemperatureUnit.Kelvin
  override lazy val inBaseUnitAccessor = () => unitInJoulePerKelvin
}

object EntropyUnit{

  // Custom
  private[EntropyUnit]
  class EntropyUnitImpl(val symbol: String, val unitInJoulePerKelvin: Real)
    extends EntropyUnit

  case object Bit extends EntropyUnitImpl("bit;b;Sh", r"9.56994016e-24") with NotExact

  // Quotient (Energy / Temperature)
  private[EntropyUnit]
  class QuotientEntropyUnit(val numeratorUnit: EnergyUnit, val denominatorUnit: TemperatureUnit)
    extends EntropyUnit with QuotientUnit[EntropyUnit, EnergyUnit, TemperatureUnit]{

    override lazy val unitInJoulePerKelvin: Real =
      numeratorUnit.unitInJoule / denominatorUnit.unitInKelvin
  }

  def apply(eUnit: EnergyUnit, tUnit: TemperatureUnit): EntropyUnit =
    new QuotientEntropyUnit(eUnit, tUnit)
}

trait PredefinedEntropyUnit extends EntropyPostfixOps[EntropyUnit]{

  override protected def entropyPostfixOps(entropyUnit: EntropyUnit) = entropyUnit
}

object PredefinedEntropyUnit extends PredefinedEntropyUnit

trait EntropyUnitInterpreter[A]
    extends EntropyPostfixOps[Entropy[A]]{

  def apply(unit: EntropyUnit): Entropy[A]

  override protected def entropyPostfixOps(entropyUnit: EntropyUnit) =
    apply(entropyUnit)
}