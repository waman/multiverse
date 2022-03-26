package org.waman.multiverse.unit.defs.radio.freq

import spire.math._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.angle._

class AreaFrequency[A: Fractional](val value: A, val unit: AreaFrequencyUnit)
    extends LinearQuantity[AreaFrequency[A], A, AreaFrequencyUnit] {

  override protected def newQuantity(value: A, unit: AreaFrequencyUnit): AreaFrequency[A] = new AreaFrequency(value, unit)
}

/** None */
trait AreaFrequencyUnit extends LinearUnit[AreaFrequencyUnit]{

  override def getSIUnit: AreaFrequencyUnit = AreaFrequencyUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AreaFrequencyUnit.dimension
}

object AreaFrequencyUnit extends UnitInfo[AreaFrequencyUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, L -> 2).withDefaultValue(0)

  val getSIUnit: AreaFrequencyUnit = AreaUnit.getSIUnit * FrequencyUnit.getSIUnit

  def getUnits: Seq[AreaFrequencyUnit] = Seq()
}


/** For no alias or user defined units */
class SimpleAreaFrequencyUnit(val name: String, val symbol: String, val interval: Real) extends AreaFrequencyUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAreaFrequencyUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AreaFrequencyUnit
  