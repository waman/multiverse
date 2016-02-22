package org.waman.multiverse.radiation

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait EquivalentDosePostfixOps[A]{

  import EquivalentDoseUnit._

  protected def equivalentDosePostfixOps(equivalentDoseUnit: EquivalentDoseUnit): A

  def Sv: A = equivalentDosePostfixOps(Sievert)
}

class EquivalentDose[A: Fractional](val value: A, val unit: EquivalentDoseUnit)
  extends Quantity[A, EquivalentDoseUnit]
    with EquivalentDosePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: EquivalentDoseUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSievert) / real(evalUnit.unitInSievert)

  override protected def equivalentDosePostfixOps(equivalentDoseUnit: EquivalentDoseUnit) = apply(equivalentDoseUnit)
}

sealed abstract class EquivalentDoseUnit(val symbol: String, val unitInSievert: Real)
    extends PhysicalUnit[EquivalentDoseUnit]{

  override val baseUnit = EquivalentDoseUnit.Sievert
  override val inBaseUnitAccessor = () => unitInSievert
}

object EquivalentDoseUnit{

  case object Sievert extends EquivalentDoseUnit("Sv", 1)
}

trait PredefinedEquivalentDoseUnit extends EquivalentDosePostfixOps[EquivalentDoseUnit]{

  override protected def equivalentDosePostfixOps(equivalentDoseUnit: EquivalentDoseUnit) = equivalentDoseUnit
}

object PredefinedEquivalentDoseUnit extends PredefinedEquivalentDoseUnit

trait EquivalentDoseUnitInterpreter[A]
    extends EquivalentDosePostfixOps[EquivalentDose[A]]{

  def apply(unit: EquivalentDoseUnit): EquivalentDose[A]

  override protected def equivalentDosePostfixOps(equivalentDoseUnit: EquivalentDoseUnit) =
    apply(equivalentDoseUnit)
}