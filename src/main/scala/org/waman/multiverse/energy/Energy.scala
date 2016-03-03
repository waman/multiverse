package org.waman.multiverse.energy

import org.waman.multiverse._
import org.waman.multiverse.mass.{MassPostfixOps, MassUnit}
import org.waman.multiverse.radiation.{AbsorbedDose, AbsorbedDoseUnit}
import org.waman.multiverse.thermal.{Entropy, EntropyUnit, TemperaturePostfixOps, TemperatureUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait EnergyPostfixOps[A]{

  import EnergyUnit._

  protected def energyPostfixOps(energyUnit: EnergyUnit): A

  def J : A = energyPostfixOps(Joule)
  def eV: A = energyPostfixOps(ElectronVolt)
}

trait EnergyDot[A]{
  import EnergyUnit._

  protected def energyDot(energyUnit: EnergyUnit): A

  def J (dot: Dot): A = energyDot(Joule)
  def eV(dot: Dot): A = energyDot(ElectronVolt)
}

trait EnergyPer[A]{
  import EnergyUnit._

  protected def energyPer(energyUnit: EnergyUnit): A

  def J (per: Per): A = energyPer(Joule)
  def eV(per: Per): A = energyPer(ElectronVolt)
}

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
    else value * real(unit.unitInJoule) / real(evalUnit.unitInJoule)

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = apply(energyUnit)

  override def *(timeUnit: TimeUnit) = new Action(value, unit * timeUnit)

  override def /(temperatureUnit: TemperatureUnit) = new Entropy(value, unit / temperatureUnit)

  override def /(massUnit: MassUnit) = new AbsorbedDose(value, unit / massUnit)
}

sealed abstract class EnergyUnit(val symbols: Seq[String], val unitInJoule: Real)
    extends PhysicalUnit[EnergyUnit]
    with MultiplicativeByTimeUnit[ActionUnit]
    with DivisibleByTemperatureUnit[EntropyUnit]
    with DivisibleByMassUnit[AbsorbedDoseUnit]{

  def this(symbols: Seq[String], factor: Real, energyUnit: EnergyUnit) =
    this(symbols, factor * energyUnit.unitInJoule)

  override def baseUnit = EnergyUnit.Joule
  override def valueInBaseUnit = unitInJoule

  override def *(timeUnit: TimeUnit) = ActionUnit(this, timeUnit)

  override def /(temperatureUnit: TemperatureUnit) = EntropyUnit(this, temperatureUnit)

  override def /(massUnit: MassUnit) = AbsorbedDoseUnit(this, massUnit)
}

object EnergyUnit extends ConstantsDefined[EnergyUnit]{

  import scala.language.implicitConversions
  implicit def convertToSeq(s: String): Seq[String] = Seq(s)

  case object Joule extends EnergyUnit("J", 1)

  case object ElectronVolt extends EnergyUnit("eV", r"1.60217656535e-19") with NotExact

  override lazy val values = Seq(
    Joule,
    ElectronVolt
  )
}

trait PredefinedEnergyUnit extends EnergyPostfixOps[EnergyUnit]{

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = energyUnit
}

object PredefinedEnergyUnit extends PredefinedEnergyUnit

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