package multiverse.unit.electromagnetism

import spire.math.Real
import spire.math.Fractional

import multiverse._


class TimePerLength[A: Fractional](val value: A, val unit: TimePerLengthUnit)
    extends LinearQuantity[TimePerLength[A], A, TimePerLengthUnit] {

  import multiverse.unit.basic.TimeUnitObjects
  import multiverse.unit.basic.LengthUnitObjects

  def toElectricalResistance: ElectricalResistance[A] = new ElectricalResistance(
      apply(TimeUnitObjects.second / LengthUnitObjects.centimetre),
      ElectricalResistanceUnitObjects.statohm)

  override protected def newQuantity(value: A, unit: TimePerLengthUnit): TimePerLength[A] = new TimePerLength(value, unit)
}

/** This unit is defined for electrical resistance in ESU and Gaussian unit systems */
trait TimePerLengthUnit extends LinearUnit[TimePerLengthUnit]{

  override def getSIUnit: TimePerLengthUnit = TimePerLengthUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = TimePerLengthUnit.dimension
}

object TimePerLengthUnit extends UnitInfo[TimePerLengthUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 1, L -> -1).withDefaultValue(0)

  import multiverse.unit.basic.TimeUnit
  import multiverse.unit.basic.LengthUnit
  val getSIUnit: TimePerLengthUnit = TimeUnit.getSIUnit / LengthUnit.getSIUnit

  def getUnits: Seq[TimePerLengthUnit] =
    Seq()
}

/** For no aliase or user defined units */
class SimpleTimePerLengthUnit(val name: String, val symbol: String, val interval: Real) extends TimePerLengthUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultTimePerLengthUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends TimePerLengthUnit

object TimePerLengthUnitObjects{


}

object TimePerLengthUnits{

}