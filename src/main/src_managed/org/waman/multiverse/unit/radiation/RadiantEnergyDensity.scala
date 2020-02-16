package org.waman.multiverse.unit.radiation

import spire.math.Real
import spire.math.Fractional
import org.waman.multiverse._


class RadiantEnergyDensity[A: Fractional](val value: A, val unit: RadiantEnergyDensityUnit)
    extends LinearQuantity[RadiantEnergyDensity[A], A, RadiantEnergyDensityUnit] {

  override protected def newQuantity(value: A, unit: RadiantEnergyDensityUnit): RadiantEnergyDensity[A] = new RadiantEnergyDensity(value, unit)
}

/** null */
trait RadiantEnergyDensityUnit extends LinearUnit[RadiantEnergyDensityUnit]{

  override def getSIUnit: RadiantEnergyDensityUnit = RadiantEnergyDensityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = RadiantEnergyDensityUnit.dimension
}

object RadiantEnergyDensityUnit extends UnitInfo[RadiantEnergyDensityUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, L -> -1).withDefaultValue(0)

  import org.waman.multiverse.unit.mechanics.EnergyUnit
  import org.waman.multiverse.unit.basic.VolumeUnit
  val getSIUnit: RadiantEnergyDensityUnit = EnergyUnit.getSIUnit / VolumeUnit.getSIUnit

  def getUnits: Seq[RadiantEnergyDensityUnit] =
    Seq()
}

/** For no aliase or user defined units */
class SimpleRadiantEnergyDensityUnit(val name: String, val symbol: String, val interval: Real) extends RadiantEnergyDensityUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultRadiantEnergyDensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends RadiantEnergyDensityUnit

object RadiantEnergyDensityUnitObjects{

}

object RadiantEnergyDensityUnits{
}