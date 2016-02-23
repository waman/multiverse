package org.waman.multiverse.mass

import org.waman.multiverse._
import org.waman.multiverse.metric.{VolumePostfixOps, VolumeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

class Density[A: Fractional](val value: A, val unit: DensityUnit)
  extends Quantity[A, DensityUnit]
    with MassPostfixOps[DivisibleByVolumeUnit[A]]
    with MassPer[VolumePostfixOps[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: DensityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInKiloGramPerCubicMetre) / real(evalUnit.unitInKiloGramPerCubicMetre)

  override protected def massPostfixOps(massUnit: MassUnit) = new DivisibleByVolumeUnit[A]{
    override def /(volumeUnit: VolumeUnit): A = apply(massUnit / volumeUnit)
  }

  override protected def massPer(massUnit: MassUnit) = new VolumePostfixOps[A]{
    override protected def volumePostfixOps(volumeUnit: VolumeUnit) = apply(massUnit / volumeUnit)
  }
}

sealed trait DensityUnit extends PhysicalUnit[DensityUnit]{

  def unitInKiloGramPerCubicMetre: Real

  override def baseUnit = MassUnit.KiloGram / VolumeUnit.CubicMetre
  override def valueInBaseUnit = unitInKiloGramPerCubicMetre
}

object DensityUnit{

  // Quotient (Mass / Volume)
  private[DensityUnit]
  class QuotientDensityUnit(val numeratorUnit: MassUnit, val denominatorUnit: VolumeUnit)
    extends DensityUnit with QuotientUnit[DensityUnit, MassUnit, VolumeUnit]{

    override lazy val unitInKiloGramPerCubicMetre: Real =
      numeratorUnit.unitInKiloGram / denominatorUnit.unitInCubicMetre
  }

  def apply(mUnit: MassUnit, vUnit: VolumeUnit): DensityUnit =
    new QuotientDensityUnit(mUnit, vUnit)
}

trait PredefinedDensityUnit

object PredefinedDensityUnit extends PredefinedDensityUnit

trait DensityUnitInterpreter[A]
    extends UnitConverter[A]{

  def apply(unit: DensityUnit): Density[A]
}
