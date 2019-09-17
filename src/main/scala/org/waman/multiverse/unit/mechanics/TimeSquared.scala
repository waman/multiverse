package org.waman.multiverse.unit.mechanics

import org.waman.multiverse._
import spire.math.Fractional

class TimeSquared[A: Fractional](val value: A, val unit: TimeSquaredUnit)
    extends LinearQuantity[TimeSquared[A], A, TimeSquaredUnit] {

  override protected def newQuantity(value: A, unit: TimeSquaredUnit): TimeSquared[A] =
    new TimeSquared(value, unit)
}

trait TimeSquaredUnit extends LinearUnit[TimeSquaredUnit] {

  override def getSIUnit: TimeSquaredUnit = TimeSquaredUnitObjects.getSIUnit
}

object TimeSquaredUnitObjects{
  import org.waman.multiverse.unit

  val `s²`: TimeSquaredUnit = unit.basic.TimeUnitObjects.second.square

  def getSIUnit: TimeSquaredUnit = `s²`
  def getUnits: Seq[TimeSquaredUnit] = Seq(`s²`)
}

object TimeSquaredUnits{
  def s2: TimeSquaredUnit = TimeSquaredUnitObjects.`s²`

  def getSIUnit: TimeSquaredUnit = TimeSquaredUnitObjects.getSIUnit
  def getUnits: Seq[TimeSquaredUnit] = TimeSquaredUnitObjects.getUnits
}