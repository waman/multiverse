package org.waman.multiverse.mechanics

import org.waman.multiverse._
import org.waman.multiverse.metric.{LengthPostfixOps, LengthUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

class Torque[A: Fractional](val value: A, val unit: TorqueUnit)
    extends Quantity[A, TorqueUnit]
    with ForcePostfixOps[MultiplicativeByLengthUnit[A]]
    with ForceDot[LengthPostfixOps[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: TorqueUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInNewtonMetre) / real(evalUnit.unitInNewtonMetre)

  override protected def forcePostfixOps(forceUnit: ForceUnit) = new MultiplicativeByLengthUnit[A]{
    override def *(lengthUnit: LengthUnit): A = apply(forceUnit * lengthUnit)
  }

  override protected def forceDot(forceUnit: ForceUnit) = new LengthPostfixOps[A]{
    override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(forceUnit * lengthUnit)
  }
}

sealed trait TorqueUnit extends PhysicalUnit[TorqueUnit]{

  def unitInNewtonMetre: Real

  override def baseUnit = ForceUnit.Newton * LengthUnit.Metre
  override def valueInBaseUnit = unitInNewtonMetre
}

object TorqueUnit{

  // Product (Force * Length)
  private class ProductTorqueUnit(val firstUnit: ForceUnit, val secondUnit: LengthUnit)
    extends TorqueUnit with ProductUnit[TorqueUnit, ForceUnit, LengthUnit]{

    override lazy val unitInNewtonMetre: Real =
      firstUnit.unitInNewton * secondUnit.unitInMetre
  }

  def apply(fUnit: ForceUnit, lUnit: LengthUnit): TorqueUnit =
    new ProductTorqueUnit(fUnit, lUnit)
}

trait TorqueFactory[A]
  extends UnitConverter[A]{

  def apply(unit: TorqueUnit): Torque[A]
}
