package org.waman.multiverse.unit.radiation

import spire.math.Real
import spire.math.Fractional
import org.waman.multiverse._

class EquivalentDoseRate[A: Fractional](val value: A, val unit: EquivalentDoseRateUnit)
    extends LinearQuantity[EquivalentDoseRate[A], A, EquivalentDoseRateUnit] {

  override protected def newQuantity(value: A, unit: EquivalentDoseRateUnit): EquivalentDoseRate[A] = new EquivalentDoseRate(value, unit)
}

trait EquivalentDoseRateUnit extends LinearUnit[EquivalentDoseRateUnit]{

  override def getSIUnit: EquivalentDoseRateUnit = EquivalentDoseRateUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = EquivalentDoseRateUnit.dimension

}

object EquivalentDoseRateUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, L -> 2).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.TimeUnit
  val getSIUnit: EquivalentDoseRateUnit = EquivalentDoseUnit.getSIUnit / TimeUnit.getSIUnit

  def getUnits: Seq[EquivalentDoseRateUnit] =
    Seq()
}



class DefaultEquivalentDoseRateUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends EquivalentDoseRateUnit

object EquivalentDoseRateUnitObjects{

}

object EquivalentDoseRateUnits{
}