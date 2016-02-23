package org.waman.multiverse.electric

import org.waman.multiverse._
import org.waman.multiverse.metric.{LengthPostfixOps, LengthUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait DipolePostfixOps[A]{

  import DipoleUnit._

  protected def dipolePostfixOps(dipoleUnit: DipoleUnit): A

  def D: A = dipolePostfixOps(Debye)
}

class Dipole[A: Fractional](val value: A, val unit: DipoleUnit)
  extends Quantity[A, DipoleUnit]
    with DipolePostfixOps[A]
    with ChargePostfixOps[MultiplicativeByLengthUnit[A]]
    with ChargeDot[LengthPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: DipoleUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCoulombMetre) / real(evalUnit.unitInCoulombMetre)

  override protected def dipolePostfixOps(dipoleUnit: DipoleUnit) = apply(dipoleUnit)

  override protected def chargePostfixOps(chargeUnit: ChargeUnit) = new MultiplicativeByLengthUnit[A]{
    override def *(lengthUnit: LengthUnit) = apply(chargeUnit * lengthUnit)
  }

  override protected def chargeDot(chargeUnit: ChargeUnit) = new LengthPostfixOps[A]{
    override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(chargeUnit * lengthUnit)
  }
}

sealed trait DipoleUnit extends PhysicalUnit[DipoleUnit]{

  def unitInCoulombMetre: Real

  override def baseUnit = ChargeUnit.Coulomb * LengthUnit.Metre
  override def valueInBaseUnit = unitInCoulombMetre
}

object DipoleUnit{

  // Custom
  private[DipoleUnit]
  class DipoleUnitImpl(val symbol: String, val unitInCoulombMetre: Real)
    extends DipoleUnit{

    def this(symbol: String, chargeUnit: ChargeUnit, lengthUnit: LengthUnit) =
      this(symbol, chargeUnit.unitInCoulomb * lengthUnit.unitInMetre)
  }

  case object Debye extends DipoleUnitImpl("D", r"340")

  // Product (Charge * Length)
  private[DipoleUnit]
  class ProductDipoleUnit(val firstUnit: ChargeUnit, val secondUnit: LengthUnit)
    extends DipoleUnit with ProductUnit[DipoleUnit, ChargeUnit, LengthUnit]{

    override lazy val unitInCoulombMetre: Real = firstUnit.unitInCoulomb * secondUnit.unitInMetre
  }

  def apply(cUnit: ChargeUnit, lUnit: LengthUnit): DipoleUnit =
    new ProductDipoleUnit(cUnit, lUnit)
}

trait PredefinedDipoleUnit extends DipolePostfixOps[DipoleUnit]{

  override protected def dipolePostfixOps(dipoleUnit: DipoleUnit) = dipoleUnit
}

object PredefinedDipoleUnit extends PredefinedDipoleUnit

trait DipoleUnitInterpreter[A]
    extends DipolePostfixOps[Dipole[A]]{

  def apply(unit: DipoleUnit): Dipole[A]

  override protected def dipolePostfixOps(dipoleUnit: DipoleUnit) = apply(dipoleUnit)
}