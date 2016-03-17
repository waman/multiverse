package org.waman.multiverse.luminous

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPostfixOps, AreaUnit, DivisibleByAreaUnit}
import spire.implicits._
import spire.math.Fractional

class LuminousFlux[A: Fractional](val value: A, val unit: LuminousFluxUnit)
  extends Quantity[A, LuminousFluxUnit]
    with LuminousFluxPostfixOps[A]
    with DivisibleByAreaUnit[Illuminance[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: LuminousFluxUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInLumen) / real(evalUnit.unitInLumen)

  override protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit) = apply(luminousFluxUnit)

  override def /(areaUnit: AreaUnit) = new Illuminance[A](value, unit / areaUnit)
}

trait LuminousFluxFactory[A]
    extends LuminousFluxPostfixOps[LuminousFlux[A]]
    with LuminousFluxPer[AreaPostfixOps[Illuminance[A]]]{

  def apply(unit: LuminousFluxUnit): LuminousFlux[A]

  override protected def luminousFluxPostfixOps(luminousFluxUnit: LuminousFluxUnit) =
    apply(luminousFluxUnit)

  // LuminousFlux / Area -> Illuminance
  def apply(unit: IlluminanceUnit): Illuminance[A]

  override protected def luminousFluxPer(luminousFluxUnit: LuminousFluxUnit) = new AreaPostfixOps[Illuminance[A]]{
    override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(luminousFluxUnit / areaUnit)
  }
}