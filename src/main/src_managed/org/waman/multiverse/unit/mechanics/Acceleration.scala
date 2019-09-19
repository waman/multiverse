package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

class Acceleration[A: Fractional](val value: A, val unit: AccelerationUnit)
    extends LinearQuantity[Acceleration[A], A, AccelerationUnit] {

  override protected def newQuantity(value: A, unit: AccelerationUnit): Acceleration[A] = new Acceleration(value, unit)
}

trait AccelerationUnit extends LinearUnit[AccelerationUnit]{
  override def getSIUnit: AccelerationUnit = AccelerationUnitObjects.getSIUnit

}

class DefaultAccelerationUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AccelerationUnit


object AccelerationUnitObjects{
  final object standard_gravity extends DefaultAccelerationUnit("standard gravity", "g_0", Nil, r"9.80665")
  import org.waman.multiverse.unit.basic.LengthUnitObjects

  val getSIUnit: AccelerationUnit = LengthUnitObjects.getSIUnit / TimeSquaredUnitObjects.getSIUnit

  def getUnits: Seq[AccelerationUnit] =
    Seq(standard_gravity)
}

object AccelerationUnits{
  def g_0: AccelerationUnit = AccelerationUnitObjects.standard_gravity

  def getSIUnit: AccelerationUnit = AccelerationUnitObjects.getSIUnit
  def getUnits: Seq[AccelerationUnit] = AccelerationUnitObjects.getUnits
}
