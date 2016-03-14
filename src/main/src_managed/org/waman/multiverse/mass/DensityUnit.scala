package org.waman.multiverse.mass

import org.waman.multiverse._
import org.waman.multiverse.metric.VolumeUnit
import spire.math.Real

sealed trait DensityUnit extends PhysicalUnit[DensityUnit]{

  def unitInKiloGramPerCubicMetre: Real

  override def baseUnit = MassUnit.KiloGram / VolumeUnit.CubicMetre
  override def valueInBaseUnit = unitInKiloGramPerCubicMetre
}

object DensityUnit extends ConstantsDefined[DensityUnit]{

  // intrinsic
  private[DensityUnit]
  class IntrinsicDensityUnit(name: String, val symbols: Seq[String], val unitInKiloGramPerCubicMetre: Real)
      extends DensityUnit{

    def this(name: String, symbols: Seq[String], unit: DensityUnit) =
      this(name, symbols, unit.unitInKiloGramPerCubicMetre)

    def this(name: String, symbols: Seq[String], factor: Real, unit: DensityUnit) =
      this(name, symbols, factor * unit.unitInKiloGramPerCubicMetre)
  }


  override lazy val values = Seq()

  // MassUnit / VolumeUnit -> Density
  private[DensityUnit]
  class MassPerVolumeUnit(val numeratorUnit: MassUnit, val denominatorUnit: VolumeUnit)
      extends DensityUnit with QuotientUnit[DensityUnit, MassUnit, VolumeUnit]{

    override lazy val unitInKiloGramPerCubicMetre: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: MassUnit, dUnit: VolumeUnit): DensityUnit =
    new MassPerVolumeUnit(nUnit, dUnit)
}