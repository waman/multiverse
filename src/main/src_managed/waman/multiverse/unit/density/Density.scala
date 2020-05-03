package waman.multiverse.unit.density

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


class Density[A: Fractional](val value: A, val unit: DensityUnit)
    extends LinearQuantity[Density[A], A, DensityUnit] {

  override protected def newQuantity(value: A, unit: DensityUnit): Density[A] = new Density(value, unit)
}

trait DensityUnit extends LinearUnit[DensityUnit]{

  override def getSIUnit: DensityUnit = DensityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = DensityUnit.dimension
}

object DensityUnit extends UnitInfo[DensityUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](M -> 1, L -> -3).withDefaultValue(0)

  import waman.multiverse.unit.basic.MassUnit
  import waman.multiverse.unit.basic.VolumeUnit
  val getSIUnit: DensityUnit = MassUnit.getSIUnit / VolumeUnit.getSIUnit

  import DensityUnitObjects._
  def getUnits: Seq[DensityUnit] =
    Seq(water, mercury)
}

/** For no aliase or user defined units */
class SimpleDensityUnit(val name: String, val symbol: String, val interval: Real) extends DensityUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultDensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends DensityUnit

object DensityUnitObjects{

  import spire.implicits._


  final case object water extends SimpleDensityUnit("water", "H2O", r"999.972")
  final case object mercury extends SimpleDensityUnit("mercury", "Hg", r"13.5951")
}

object DensityUnits{

  def H2O: DensityUnit = DensityUnitObjects.water
  def Hg: DensityUnit = DensityUnitObjects.mercury
}