package org.waman.multiverse.radiation

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait AbsorbedDosePostfixOps[A]{

  import AbsorbedDoseUnit._

  protected def absorbedDosePostfixOps(absorbedDoseUnit: AbsorbedDoseUnit): A

  def Gy: A = absorbedDosePostfixOps(Gray)
}

class AbsorbedDose[A: Fractional](val value: A, val unit: AbsorbedDoseUnit)
  extends Quantity[A, AbsorbedDoseUnit]
    with AbsorbedDosePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AbsorbedDoseUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInGray) / real(evalUnit.unitInGray)

  override protected def absorbedDosePostfixOps(absorbedDoseUnit: AbsorbedDoseUnit) = apply(absorbedDoseUnit)
}

sealed abstract class AbsorbedDoseUnit(val symbol: String, val unitInGray: Real)
    extends PhysicalUnit[AbsorbedDoseUnit]{

  override val baseUnit = AbsorbedDoseUnit.Gray
  override val inBaseUnitAccessor = () => unitInGray
}

object AbsorbedDoseUnit{

  case object Gray extends AbsorbedDoseUnit("Gy", 1)
}

trait PredefinedAbsorbedDoseUnit extends AbsorbedDosePostfixOps[AbsorbedDoseUnit]{

  override protected def absorbedDosePostfixOps(absorbedDoseUnit: AbsorbedDoseUnit) = absorbedDoseUnit
}

object PredefinedAbsorbedDoseUnit extends PredefinedAbsorbedDoseUnit

trait AbsorbedDoseUnitInterpreter[A]
    extends AbsorbedDosePostfixOps[AbsorbedDose[A]]{

  def apply(unit: AbsorbedDoseUnit): AbsorbedDose[A]

  override protected def absorbedDosePostfixOps(absorbedDoseUnit: AbsorbedDoseUnit) =
    apply(absorbedDoseUnit)
}