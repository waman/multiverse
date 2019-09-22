package org.waman.multiverse.unit.radiation

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

class Exposure[A: Fractional](val value: A, val unit: ExposureUnit)
    extends LinearQuantity[Exposure[A], A, ExposureUnit] {

  override protected def newQuantity(value: A, unit: ExposureUnit): Exposure[A] = new Exposure(value, unit)
}

trait ExposureUnit extends LinearUnit[ExposureUnit]{
  override def getSIUnit: ExposureUnit = ExposureUnitObjects.getSIUnit

}

class DefaultExposureUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ExposureUnit


object ExposureUnitObjects{
  final object roentgen extends DefaultExposureUnit("roentgen", "R", Nil, r"2.58e-4")

  import org.waman.multiverse.unit.electric.ChargeUnitObjects
  import org.waman.multiverse.unit.basic.MassUnitObjects

  val getSIUnit: ExposureUnit = ChargeUnitObjects.getSIUnit / MassUnitObjects.getSIUnit

  def getUnits: Seq[ExposureUnit] =
    Seq(roentgen)
}

object ExposureUnits{
  def R: ExposureUnit = ExposureUnitObjects.roentgen

  def getSIUnit: ExposureUnit = ExposureUnitObjects.getSIUnit
  def getUnits: Seq[ExposureUnit] = ExposureUnitObjects.getUnits
}
