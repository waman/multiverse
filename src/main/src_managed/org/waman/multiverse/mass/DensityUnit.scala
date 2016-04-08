package org.waman.multiverse.mass

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._

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


  case object Water extends IntrinsicDensityUnit("Water", Seq("H2O"), r"999.972")
  case object Mercury extends IntrinsicDensityUnit("Mercury", Seq("Hg"), r"13.5951")

  override lazy val values = Seq(Water, Mercury)

  // MassUnit / VolumeUnit -> Density
  private[DensityUnit]
  class QuotientMassPerVolumeUnit(val numeratorUnit: MassUnit, val denominatorUnit: VolumeUnit)
      extends DensityUnit with QuotientUnit[DensityUnit, MassUnit, VolumeUnit]{

    override lazy val unitInKiloGramPerCubicMetre: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: MassUnit, dUnit: VolumeUnit): DensityUnit =
    new QuotientMassPerVolumeUnit(nUnit, dUnit)
}

trait MultiplicativeByDensityUnit[R]{
  def *(unit: DensityUnit): R
}

trait DivisibleByDensityUnit[R]{
  def /(unit: DensityUnit): R
}

trait DensityPostfixOps[A]{
  import DensityUnit._

  protected def densityPostfixOps(unit: DensityUnit): A


  def H2O : A = densityPostfixOps(Water)
  def Hg : A = densityPostfixOps(Mercury)
}

trait DensityDot[A]{
  import DensityUnit._

  protected def densityDot(unit: DensityUnit): A

  def H2O(dot: Dot): A = densityDot(Water)
  def Hg(dot: Dot): A = densityDot(Mercury)
}

trait DensityPer[A]{
  import DensityUnit._

  protected def densityPer(unit: DensityUnit): A

  def H2O(per: Per): A = densityPer(Water)
  def Hg(per: Per): A = densityPer(Mercury)
}

trait PredefinedDensityUnit extends DensityPostfixOps[DensityUnit]{
  override protected def densityPostfixOps(unit: DensityUnit) = unit
  
}

object PredefinedDensityUnit extends PredefinedDensityUnit
