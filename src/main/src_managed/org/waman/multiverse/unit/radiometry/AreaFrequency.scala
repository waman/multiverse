package org.waman.multiverse.unit.radiometry

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class AreaFrequency[A: Fractional](val value: A, val unit: AreaFrequencyUnit)
    extends LinearQuantity[AreaFrequency[A], A, AreaFrequencyUnit] {

  override protected def newQuantity(value: A, unit: AreaFrequencyUnit): AreaFrequency[A] = new AreaFrequency(value, unit)
}

/** null */
trait AreaFrequencyUnit extends LinearUnit[AreaFrequencyUnit]{

  override def getSIUnit: AreaFrequencyUnit = AreaFrequencyUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AreaFrequencyUnit.dimension
}

object AreaFrequencyUnit extends UnitInfo[AreaFrequencyUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, L -> 2).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.AreaUnit
  import org.waman.multiverse.unit.angle.FrequencyUnit
  val getSIUnit: AreaFrequencyUnit = AreaUnit.getSIUnit * FrequencyUnit.getSIUnit

  def getUnits: Seq[AreaFrequencyUnit] =
    Seq()
}

/** For no aliase or user defined units */
class SimpleAreaFrequencyUnit(val name: String, val symbol: String, val interval: Real) extends AreaFrequencyUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAreaFrequencyUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AreaFrequencyUnit

object AreaFrequencyUnitObjects{

}

object AreaFrequencyUnits{

}