package org.waman.multiverse

import spire.math.{Real, Fractional}
import spire.implicits._

trait DensityPostfixOps[A]{

  protected def densityPostfixOps(densityUnit: DensityUnit): A

  def `kg/m3`: A = densityPostfixOps(DensityUnit.KiloGramPerCubicMetre)
  def `g/cm3`: A = densityPostfixOps(DensityUnit.GramPerCubicCentiMetre)
}

class Density[A: Fractional](val value: A, val unit: DensityUnit)
  extends Quantity[A, DensityUnit]
    with DensityPostfixOps[A]
    with MassPostfixOps[DivisibleBy[VolumeUnit, A]]
    with MassPer[VolumePostfixOps[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: DensityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInKilogramPerCubicMetre) / real(evalUnit.unitInKilogramPerCubicMetre)

  override protected def densityPostfixOps(densityUnit: DensityUnit) = apply(densityUnit)

  override protected def massPostfixOps(massUnit: MassUnit) = new DivisibleBy[VolumeUnit, A]{
    override def /(volumeUnit: VolumeUnit): A = apply(massUnit / volumeUnit)
  }

  override protected def massPer(massUnit: MassUnit) = new VolumePostfixOps[A]{
    override protected def volumePostfixOps(volumeUnit: VolumeUnit) = apply(massUnit / volumeUnit)
  }
}

sealed trait DensityUnit extends PhysicalUnit{
  def unitInKilogramPerCubicMetre: Real

  override protected def baseUnit = DensityUnit.KiloGramPerCubicMetre
  override protected def inBaseUnitAccessor = () => unitInKilogramPerCubicMetre
}

class QuotientDensityUnit(val massUnit: MassUnit, val volumeUnit: VolumeUnit)
  extends DensityUnit with QuotientUnit[MassUnit, VolumeUnit]{

  override def numeratorUnit: MassUnit = massUnit
  override def denominatorUnit: VolumeUnit = volumeUnit

  override def unitInKilogramPerCubicMetre: Real = massUnit.unitInKiloGram / volumeUnit.unitInCubicMetre
}

object DensityUnit{

  case object KiloGramPerCubicMetre extends QuotientDensityUnit(MassUnit.KiloGram, VolumeUnit.CubicMetre)

  case object GramPerCubicCentiMetre extends QuotientDensityUnit(MassUnit.Gram, VolumeUnit.CubicCentiMetre)

  def apply(mUnit: MassUnit, vUnit: VolumeUnit): DensityUnit =
    new QuotientDensityUnit(mUnit, vUnit)
}

trait PredefinedDensityUnit{
  val `kg/m3` = DensityUnit.KiloGramPerCubicMetre
  val `g/cm3` = DensityUnit.GramPerCubicCentiMetre
}

object PredefinedDensityUnit extends PredefinedDensityUnit

trait DensityUnitInterpreter[A]
  extends DensityPostfixOps[Density[A]]
    with UnitConverter[A]{

  def apply(unit: DensityUnit): Density[A]

  override protected def densityPostfixOps(densityUnit: DensityUnit) = apply(densityUnit)
}
