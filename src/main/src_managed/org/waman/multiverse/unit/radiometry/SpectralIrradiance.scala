package org.waman.multiverse.unit.radiometry

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class SpectralIrradiance[A: Fractional](val value: A, val unit: SpectralIrradianceUnit)
    extends LinearQuantity[SpectralIrradiance[A], A, SpectralIrradianceUnit] {

  override protected def newQuantity(value: A, unit: SpectralIrradianceUnit): SpectralIrradiance[A] = new SpectralIrradiance(value, unit)
}

/** This unit is, exactly speaking, spectral irradiance in frequency. */
trait SpectralIrradianceUnit extends LinearUnit[SpectralIrradianceUnit]{

  override def getSIUnit: SpectralIrradianceUnit = SpectralIrradianceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = SpectralIrradianceUnit.dimension
}

object SpectralIrradianceUnit extends UnitInfo[SpectralIrradianceUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1).withDefaultValue(0)

  import org.waman.multiverse.unit.angle.FrequencyUnit
  val getSIUnit: SpectralIrradianceUnit = IrradianceUnit.getSIUnit / FrequencyUnit.getSIUnit

  import SpectralIrradianceUnitObjects._
  def getUnits: Seq[SpectralIrradianceUnit] =
    Seq(jansky, solar_flux_unit)
}

/** For no aliase or user defined units */
class SimpleSpectralIrradianceUnit(val name: String, val symbol: String, val interval: Real) extends SpectralIrradianceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultSpectralIrradianceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends SpectralIrradianceUnit

object SpectralIrradianceUnitObjects{

  final case object jansky extends SimpleSpectralIrradianceUnit("jansky", "Jy", r"1e-26")
  final case object solar_flux_unit extends SimpleSpectralIrradianceUnit("solar flux unit", "sfu", r"1e-22")
}

object SpectralIrradianceUnits{
  def Jy: SpectralIrradianceUnit = SpectralIrradianceUnitObjects.jansky
  def sfu: SpectralIrradianceUnit = SpectralIrradianceUnitObjects.solar_flux_unit
}