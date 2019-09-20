package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

class Area[A: Fractional](val value: A, val unit: AreaUnit)
    extends LinearQuantity[Area[A], A, AreaUnit] {

  override protected def newQuantity(value: A, unit: AreaUnit): Area[A] = new Area(value, unit)
}

trait AreaUnit extends LinearUnit[AreaUnit]{
  override def getSIUnit: AreaUnit = AreaUnitObjects.getSIUnit


  def *(lengthUnit: LengthUnit): VolumeUnit =
    new ProductUnit[VolumeUnit, AreaUnit, LengthUnit](AreaUnit.this, lengthUnit) with VolumeUnit

}

class DefaultAreaUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AreaUnit


object AreaUnitObjects{
  final object are extends DefaultAreaUnit("are", "a", Nil, r"1e2")
  final object hectare extends DefaultAreaUnit("hectare", "ha", Nil, r"1e4")


  val getSIUnit: AreaUnit = LengthUnitObjects.getSIUnit * LengthUnitObjects.getSIUnit

  def getUnits: Seq[AreaUnit] =
    Seq(are, hectare)
}

object AreaUnits{
  def a: AreaUnit = AreaUnitObjects.are
  def ha: AreaUnit = AreaUnitObjects.hectare

  def getSIUnit: AreaUnit = AreaUnitObjects.getSIUnit
  def getUnits: Seq[AreaUnit] = AreaUnitObjects.getUnits
}
