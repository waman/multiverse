package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional

import org.waman.multiverse._

class TimeSquared[A: Fractional](val value: A, val unit: TimeSquaredUnit)
    extends LinearQuantity[TimeSquared[A], A, TimeSquaredUnit] {

  override protected def newQuantity(value: A, unit: TimeSquaredUnit): TimeSquared[A] = new TimeSquared(value, unit)
}

trait TimeSquaredUnit extends LinearUnit[TimeSquaredUnit]{
  override def getSIUnit: TimeSquaredUnit = TimeSquaredUnitObjects.getSIUnit

}

class DefaultTimeSquaredUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends TimeSquaredUnit


object TimeSquaredUnitObjects{
  import org.waman.multiverse.unit.basic.TimeUnitObjects

  val getSIUnit: TimeSquaredUnit = TimeUnitObjects.getSIUnit * TimeUnitObjects.getSIUnit

  val second_squared: TimeSquaredUnit = getSIUnit

  def getUnits: Seq[TimeSquaredUnit] =
    Seq(second_squared)
}


object TimeSquaredUnits{
  def `s²`: TimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def s2: TimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def `sec²`: TimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def sec2: TimeSquaredUnit = TimeSquaredUnitObjects.second_squared

  def getSIUnit: TimeSquaredUnit = TimeSquaredUnitObjects.getSIUnit
  def getUnits: Seq[TimeSquaredUnit] = TimeSquaredUnitObjects.getUnits
}
