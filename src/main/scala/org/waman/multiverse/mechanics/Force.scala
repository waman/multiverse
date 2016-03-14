package org.waman.multiverse.mechanics

import org.waman.multiverse._
import org.waman.multiverse.metric.{LengthPostfixOps, LengthUnit}
import spire.implicits._
import spire.math.Fractional

class Force[A: Fractional](val value: A, val unit: ForceUnit)
  extends Quantity[A, ForceUnit]
    with ForcePostfixOps[A]
    with MultiplicativeByLengthUnit[Torque[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ForceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInNewton) / real(evalUnit.unitInNewton)

  override protected def forcePostfixOps(forceUnit: ForceUnit) = apply(forceUnit)

  override def *(lengthUnit: LengthUnit): Torque[A] = new Torque(value, unit * lengthUnit)
}

trait ForceFactory[A]
    extends ForcePostfixOps[Force[A]]
    with ForceDot[LengthPostfixOps[Torque[A]]]{

  def apply(unit: ForceUnit): Force[A]

  override protected def forcePostfixOps(forceUnit: ForceUnit) = apply(forceUnit)

  // Force * Length -> Torque
  def apply(unit: TorqueUnit): Torque[A]

  override protected def forceDot(forceUnit: ForceUnit) = new LengthPostfixOps[Torque[A]]{
    override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(forceUnit * lengthUnit)
  }
}