package org.waman.multiverse.unit.defs.radioactivity

import spire.math._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._

class EquivalentDoseRate[A: Fractional](val value: A, val unit: EquivalentDoseRateUnit)
    extends LinearQuantity[EquivalentDoseRate[A], A, EquivalentDoseRateUnit] {

  override protected def newQuantity(value: A, unit: EquivalentDoseRateUnit): EquivalentDoseRate[A] = new EquivalentDoseRate(value, unit)
}

trait EquivalentDoseRateUnit extends LinearUnit[EquivalentDoseRateUnit]{

  override def getSIUnit: EquivalentDoseRateUnit = EquivalentDoseRateUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = EquivalentDoseRateUnit.dimension
}

object EquivalentDoseRateUnit extends UnitInfo[EquivalentDoseRateUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, L -> 2).withDefaultValue(0)

  val getSIUnit: EquivalentDoseRateUnit = EquivalentDoseUnit.getSIUnit / TimeUnit.getSIUnit

  def getUnits: Seq[EquivalentDoseRateUnit] =
    Seq()
}


/** For no aliase or user defined units */
class SimpleEquivalentDoseRateUnit(val name: String, val symbol: String, val interval: Real) extends EquivalentDoseRateUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultEquivalentDoseRateUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends EquivalentDoseRateUnit
  
object EquivalentDoseRateUnitObjects{

}


object EquivalentDoseRateUnits{

}