package org.waman.multiverse.unit.electromagnetism

import spire.math.Real
import spire.math.Fractional

import org.waman.multiverse._


class TimeSquaredPerLength[A: Fractional](val value: A, val unit: TimeSquaredPerLengthUnit)
    extends LinearQuantity[TimeSquaredPerLength[A], A, TimeSquaredPerLengthUnit] {

  import org.waman.multiverse.unit.mechanics.TimeSquaredUnitObjects
  import org.waman.multiverse.unit.basic.LengthUnitObjects

  def toInductance: Inductance[A] = new Inductance(
      apply(TimeSquaredUnitObjects.second_squared / LengthUnitObjects.centimetre),
      InductanceUnitObjects.stathenry)

  override protected def newQuantity(value: A, unit: TimeSquaredPerLengthUnit): TimeSquaredPerLength[A] = new TimeSquaredPerLength(value, unit)
}

/** This unit is defined for inductance in ESU and Gaussian unit systems */
trait TimeSquaredPerLengthUnit extends LinearUnit[TimeSquaredPerLengthUnit]{

  override def getSIUnit: TimeSquaredPerLengthUnit = TimeSquaredPerLengthUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = TimeSquaredPerLengthUnit.dimension
}

object TimeSquaredPerLengthUnit extends UnitInfo[TimeSquaredPerLengthUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 2, L -> -1).withDefaultValue(0)

  import org.waman.multiverse.unit.mechanics.TimeSquaredUnit
  import org.waman.multiverse.unit.basic.LengthUnit
  val getSIUnit: TimeSquaredPerLengthUnit = TimeSquaredUnit.getSIUnit / LengthUnit.getSIUnit

  def getUnits: Seq[TimeSquaredPerLengthUnit] =
    Seq()
}

/** For no aliase or user defined units */
class SimpleTimeSquaredPerLengthUnit(val name: String, val symbol: String, val interval: Real) extends TimeSquaredPerLengthUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultTimeSquaredPerLengthUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends TimeSquaredPerLengthUnit

object TimeSquaredPerLengthUnitObjects{


}

object TimeSquaredPerLengthUnits{

}