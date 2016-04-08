package org.waman.multiverse.magnetic

import org.waman.multiverse._
import org.waman.multiverse.metric.{DivisibleByAreaUnit, AreaPostfixOps, AreaUnit}
import spire.implicits._
import spire.math.Fractional

class FluxDensity[A: Fractional](val value: A, val unit: FluxDensityUnit)
  extends Quantity[A, FluxDensityUnit]
    with FluxDensityPostfixOps[A]
    with FluxPostfixOps[DivisibleByAreaUnit[A]]
    with FluxPer[AreaPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: FluxDensityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def fluxDensityPostfixOps(fluxDensityUnit: FluxDensityUnit) = apply(fluxDensityUnit)

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = new DivisibleByAreaUnit[A]{
    override def /(areaUnit: AreaUnit) = apply(fluxUnit / areaUnit)
  }

  override protected def fluxPer(fluxUnit: FluxUnit) = new AreaPostfixOps[A]{
    override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(fluxUnit / areaUnit)
  }
}

trait FluxDensityFactory[A]
    extends FluxDensityPostfixOps[FluxDensity[A]]{

  def apply(unit: FluxDensityUnit): FluxDensity[A]

  override protected def fluxDensityPostfixOps(fluxDensityUnit: FluxDensityUnit) =
    apply(fluxDensityUnit)
}