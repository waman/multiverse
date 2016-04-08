package org.waman.multiverse.mechanics

import org.waman.multiverse._
import org.waman.multiverse.metric.{MultiplicativeByLengthUnit, LengthPostfixOps, LengthUnit}
import spire.implicits._
import spire.math.Fractional

class Torque[A: Fractional](val value: A, val unit: TorqueUnit)
    extends Quantity[A, TorqueUnit]
    with ForcePostfixOps[MultiplicativeByLengthUnit[A]]
    with ForceDot[LengthPostfixOps[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: TorqueUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def forcePostfixOps(forceUnit: ForceUnit) = new MultiplicativeByLengthUnit[A]{
    override def *(lengthUnit: LengthUnit): A = apply(forceUnit * lengthUnit)
  }

  override protected def forceDot(forceUnit: ForceUnit) = new LengthPostfixOps[A]{
    override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(forceUnit * lengthUnit)
  }
}

trait TorqueFactory[A]
  extends UnitConverter[A]{

  def apply(unit: TorqueUnit): Torque[A]
}
