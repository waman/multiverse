package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class Density[A: Fractional](val value: A, val unit: DensityUnit)
    extends LinearQuantity[Density[A], A, DensityUnit] {

  override protected def newQuantity(value: A, unit: DensityUnit): Density[A] = new Density(value, unit)

}

trait DensityUnit extends LinearUnit[DensityUnit]{

  override def getSIUnit: DensityUnit = DensityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = DensityUnit.dimension

}

object DensityUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](M -> 1, L -> -3).withDefaultValue(0)

  val getSIUnit: DensityUnit = MassUnit.getSIUnit / VolumeUnit.getSIUnit

  import DensityUnitObjects._
  def getUnits: Seq[DensityUnit] =
    Seq(water, mercury)
}

class DefaultDensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends DensityUnit

object DensityUnitObjects{

  final object water extends DefaultDensityUnit("water", "H2O", Nil, r"999.972")
  final object mercury extends DefaultDensityUnit("mercury", "Hg", Nil, r"13.5951")
}

object DensityUnits{
  def H2O: DensityUnit = DensityUnitObjects.water
  def Hg: DensityUnit = DensityUnitObjects.mercury
}