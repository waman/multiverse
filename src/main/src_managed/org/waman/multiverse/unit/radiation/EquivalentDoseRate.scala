package org.waman.multiverse.unit.radiation

import spire.math.Real
import spire.math.Fractional

import org.waman.multiverse._

class EquivalentDoseRate[A: Fractional](val value: A, val unit: EquivalentDoseRateUnit)
    extends LinearQuantity[EquivalentDoseRate[A], A, EquivalentDoseRateUnit] {

  override protected def newQuantity(value: A, unit: EquivalentDoseRateUnit): EquivalentDoseRate[A] = new EquivalentDoseRate(value, unit)
           
}

trait EquivalentDoseRateUnit extends LinearUnit[EquivalentDoseRateUnit]{
  override def getSIUnit: EquivalentDoseRateUnit = EquivalentDoseRateUnitObjects.getSIUnit

}

class DefaultEquivalentDoseRateUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends EquivalentDoseRateUnit


object EquivalentDoseRateUnitObjects{
  import org.waman.multiverse.unit.basic.TimeUnitObjects

  val getSIUnit: EquivalentDoseRateUnit = EquivalentDoseUnitObjects.getSIUnit / TimeUnitObjects.getSIUnit


  def getUnits: Seq[EquivalentDoseRateUnit] =
    Seq()
}


object EquivalentDoseRateUnits{

  def getSIUnit: EquivalentDoseRateUnit = EquivalentDoseRateUnitObjects.getSIUnit
  def getUnits: Seq[EquivalentDoseRateUnit] = EquivalentDoseRateUnitObjects.getUnits
}
