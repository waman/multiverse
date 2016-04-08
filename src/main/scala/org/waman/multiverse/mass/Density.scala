package org.waman.multiverse.mass

import org.waman.multiverse._
import org.waman.multiverse.metric.{DivisibleByVolumeUnit, VolumePostfixOps, VolumeUnit}
import spire.implicits._
import spire.math.Fractional

class Density[A: Fractional](val value: A, val unit: DensityUnit)
  extends Quantity[A, DensityUnit]
    with MassPostfixOps[DivisibleByVolumeUnit[A]]
    with MassPer[VolumePostfixOps[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: DensityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def massPostfixOps(massUnit: MassUnit) = new DivisibleByVolumeUnit[A]{
    override def /(volumeUnit: VolumeUnit): A = apply(massUnit / volumeUnit)
  }

  override protected def massPer(massUnit: MassUnit) = new VolumePostfixOps[A]{
    override protected def volumePostfixOps(volumeUnit: VolumeUnit) = apply(massUnit / volumeUnit)
  }
}

trait DensityFactory[A]
    extends UnitConverter[A]{

  def apply(unit: DensityUnit): Density[A]
}
