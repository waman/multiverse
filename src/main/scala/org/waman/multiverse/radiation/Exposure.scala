package org.waman.multiverse.radiation

import org.waman.multiverse._
import org.waman.multiverse.electric.{ChargePer, ChargePostfixOps, ChargeUnit}
import org.waman.multiverse.mass.{MassPostfixOps, MassUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait ExposurePostfixOps[A]{

  import ExposureUnit._

  protected def exposurePostfixOps(exposureUnit: ExposureUnit): A

  def R: A = exposurePostfixOps(Roentgen)
}

class Exposure[A: Fractional](val value: A, val unit: ExposureUnit)
  extends Quantity[A, ExposureUnit]
    with ExposurePostfixOps[A]
    with ChargePostfixOps[DivisibleByMassUnit[A]]
    with ChargePer[MassPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ExposureUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCoulombPerKiloGram) / real(evalUnit.unitInCoulombPerKiloGram)

  override protected def exposurePostfixOps(exposureUnit: ExposureUnit) = apply(exposureUnit)

  override protected def chargePostfixOps(chargeUnit: ChargeUnit) = new DivisibleByMassUnit[A]{
    override def /(massUnit: MassUnit) = apply(chargeUnit / massUnit)
  }

  override protected def chargePer(chargeUnit: ChargeUnit) = new MassPostfixOps[A]{
    override protected def massPostfixOps(massUnit: MassUnit) = apply(chargeUnit / massUnit)
  }
}

sealed trait ExposureUnit extends PhysicalUnit[ExposureUnit]{

  def unitInCoulombPerKiloGram: Real

  override def baseUnit = ChargeUnit.Coulomb / MassUnit.KiloGram
  override def valueInBaseUnit = unitInCoulombPerKiloGram
}

object ExposureUnit{

  // Custom
  private[ExposureUnit]
  class ExposureUnitImpl(val symbol: String, val unitInCoulombPerKiloGram: Real)
    extends ExposureUnit

  case object Roentgen extends ExposureUnitImpl("R", r"2.58e-4")

  // Quotient (Charge / KiloGram)
  private[ExposureUnit]
  class QuotientExposureUnit(val numeratorUnit: ChargeUnit, val denominatorUnit: MassUnit)
    extends ExposureUnit with QuotientUnit[ExposureUnit, ChargeUnit, MassUnit]{

    override lazy val unitInCoulombPerKiloGram: Real =
      numeratorUnit.unitInCoulomb / denominatorUnit.unitInKiloGram
  }

  def apply(cUnit: ChargeUnit, mUnit: MassUnit): ExposureUnit =
    new QuotientExposureUnit(cUnit, mUnit)
}

trait PredefinedExposureUnit extends ExposurePostfixOps[ExposureUnit]{

  override protected def exposurePostfixOps(exposureUnit: ExposureUnit) = exposureUnit
}

object PredefinedExposureUnit extends PredefinedExposureUnit

trait ExposureUnitInterpreter[A]
    extends ExposurePostfixOps[Exposure[A]]{

  def apply(unit: ExposureUnit): Exposure[A]

  override protected def exposurePostfixOps(exposureUnit: ExposureUnit) =
    apply(exposureUnit)
}