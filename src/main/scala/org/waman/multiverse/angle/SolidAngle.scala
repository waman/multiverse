package org.waman.multiverse.angle

import org.waman.multiverse.{Quantity, UnitConverter}
import spire.implicits._
import spire.math.Fractional


class SolidAngle[A: Fractional](val value: A, val unit: SolidAngleUnit)
  extends Quantity[A, SolidAngleUnit]
    with SolidAnglePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: SolidAngleUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSteradian) / real(evalUnit.unitInSteradian)

  override protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit) = apply(solidAngleUnit)
}

trait SolidAngleFactory[A]
  extends SolidAnglePostfixOps[SolidAngle[A]]{

  def apply(unit: SolidAngleUnit): SolidAngle[A]

  override protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit) = apply(solidAngleUnit)
}