package org.waman.multiverse.luminous

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPostfixOps, AreaUnit}
import spire.implicits._
import spire.math.Fractional

class Luminance[A: Fractional](val value: A, val unit: LuminanceUnit)
    extends Quantity[A, LuminanceUnit]
    with LuminancePostfixOps[A]
    with LuminousIntensityPostfixOps[DivisibleByAreaUnit[A]]
    with LuminousIntensityPer[AreaPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: LuminanceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCandelaPerSquareMetre) / real(evalUnit.unitInCandelaPerSquareMetre)

  override protected def luminancePostfixOps(luminanceUnit: LuminanceUnit) =
    apply(luminanceUnit)

  override protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit) =
    new DivisibleByAreaUnit[A]{
      override def /(areaUnit: AreaUnit) = apply(luminousIntensityUnit / areaUnit)
    }

  override protected def luminousIntensityPer(luminousIntensityUnit: LuminousIntensityUnit) =
    new AreaPostfixOps[A]{
      override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(luminousIntensityUnit / areaUnit)
    }
}

trait LuminanceFactory[A]
    extends LuminancePostfixOps[Luminance[A]]{

  def apply(unit: LuminanceUnit): Luminance[A]

  override protected def luminancePostfixOps(luminanceUnit: LuminanceUnit) =
    apply(luminanceUnit)
}