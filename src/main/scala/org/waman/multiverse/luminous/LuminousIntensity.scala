package org.waman.multiverse.luminous

import org.waman.multiverse._
import org.waman.multiverse.metric.{DivisibleByAreaUnit, AreaPostfixOps, AreaUnit}
import spire.implicits._
import spire.math.Fractional

class LuminousIntensity[A: Fractional](val value: A, val unit: LuminousIntensityUnit)
  extends Quantity[A, LuminousIntensityUnit]
    with LuminousIntensityPostfixOps[A]
    with DivisibleByAreaUnit[Luminance[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: LuminousIntensityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCandela) / real(evalUnit.unitInCandela)

  override protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit) =
    apply(luminousIntensityUnit)

  override def /(areaUnit: AreaUnit) = new Luminance[A](value, unit / areaUnit)
}

trait LuminousIntensityFactory[A]
    extends LuminousIntensityPostfixOps[LuminousIntensity[A]]
    with LuminousIntensityPer[AreaPostfixOps[Luminance[A]]]{

  def apply(unit: LuminousIntensityUnit): LuminousIntensity[A]

  override protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit) =
    apply(luminousIntensityUnit)

  // LuminousIntensity / Area -> Luminance
  def apply(unit: LuminanceUnit): Luminance[A]

  override protected def luminousIntensityPer(luminousIntensityUnit: LuminousIntensityUnit) =
    new AreaPostfixOps[Luminance[A]]{
      override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(luminousIntensityUnit / areaUnit)
    }
}