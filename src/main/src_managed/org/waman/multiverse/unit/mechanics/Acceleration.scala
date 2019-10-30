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
  override def getSIUnit: AccelerationUnit = AccelerationUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AccelerationUnit.dimension

}

object AccelerationUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, L -> 1).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.LengthUnit
  val getSIUnit: AccelerationUnit = LengthUnit.getSIUnit / TimeSquaredUnit.getSIUnit

import AccelerationUnitObjects._
  def getUnits: Seq[AccelerationUnit] =
    Seq(standard_gravity)
}



class DefaultAccelerationUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AccelerationUnit

object AccelerationUnitObjects{

  final object standard_gravity extends DefaultAccelerationUnit("standard gravity", "g_0", Nil, r"9.80665")
}

object AccelerationUnits{
  def g_0: AccelerationUnit = AccelerationUnitObjects.standard_gravity
}