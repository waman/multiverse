package org.waman.multiverse.luminous

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPostfixOps, AreaUnit}
import spire.implicits._
import spire.math.Fractional

class Illuminance[A: Fractional](val value: A, val unit: IlluminanceUnit)
  extends Quantity[A, IlluminanceUnit]
    with IlluminancePostfixOps[A]
    with LuminousFluxPostfixOps[DivisibleByAreaUnit[A]]
    with LuminousFluxPer[AreaPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: IlluminanceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInLux) / real(evalUnit.unitInLux)

  override protected def illuminancePostfixOps(illuminanceUnit: IlluminanceUnit) = apply(illuminanceUnit)

  override protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit) = new DivisibleByAreaUnit[A]{
    override def /(areaUnit: AreaUnit) = apply(luminousFluxUnit / areaUnit)
  }

  override protected def luminousFluxPer(luminousFluxUnit: LuminousFluxUnit) = new AreaPostfixOps[A]{
    override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(luminousFluxUnit / areaUnit)
  }
}

trait IlluminanceFactory[A]
    extends IlluminancePostfixOps[Illuminance[A]]{

  def apply(unit: IlluminanceUnit): Illuminance[A]

  override protected def illuminancePostfixOps(illuminanceUnit: IlluminanceUnit) =
    apply(illuminanceUnit)
}