package org.waman.multiverse

import spire.math.{Real, Fractional}
import spire.implicits._

class Density[A: Fractional](val value: A, val unit: DensityUnit)
  extends Quantity[A, DensityUnit]
    with MassPostfixOps[DivisibleBy[VolumeUnit, A]]
    with MassPer[VolumePostfixOps[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: DensityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInKilogramPerCubicMetre) / real(evalUnit.unitInKilogramPerCubicMetre)

  override protected def massPostfixOps(massUnit: MassUnit) = new DivisibleBy[VolumeUnit, A]{
    override def /(volumeUnit: VolumeUnit): A = apply(massUnit / volumeUnit)
  }

  override protected def massPer(massUnit: MassUnit) = new VolumePostfixOps[A]{
    override protected def volumePostfixOps(volumeUnit: VolumeUnit) = apply(massUnit / volumeUnit)
  }
}

sealed trait DensityUnit extends PhysicalUnit{
  def unitInKilogramPerCubicMetre: Real

  override protected lazy val baseUnit = MassUnit.KiloGram / VolumeUnit.CubicMetre
  override protected val inBaseUnitAccessor = () => unitInKilogramPerCubicMetre
}

object DensityUnit{

  class QuotientDensityUnit(val numeratorUnit: MassUnit, val denominatorUnit: VolumeUnit)
    extends DensityUnit with QuotientUnit[MassUnit, VolumeUnit]{

    override lazy val unitInKilogramPerCubicMetre: Real =
      numeratorUnit.unitInKiloGram / denominatorUnit.unitInCubicMetre
  }

  def apply(mUnit: MassUnit, vUnit: VolumeUnit): DensityUnit =
    new QuotientDensityUnit(mUnit, vUnit)
}

trait DensityUnitInterpreter[A]
    extends UnitConverter[A]{

  def apply(unit: DensityUnit): Density[A]
}
