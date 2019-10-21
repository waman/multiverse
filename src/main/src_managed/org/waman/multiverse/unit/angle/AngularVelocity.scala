package org.waman.multiverse.unit.angle

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

import org.waman.multiverse._

class AngularVelocity[A: Fractional](val value: A, val unit: AngularVelocityUnit)
    extends LinearQuantity[AngularVelocity[A], A, AngularVelocityUnit] {

  override protected def newQuantity(value: A, unit: AngularVelocityUnit): AngularVelocity[A] = new AngularVelocity(value, unit)
           
}

trait AngularVelocityUnit extends LinearUnit[AngularVelocityUnit]{
  override def getSIUnit: AngularVelocityUnit = AngularVelocityUnitObjects.getSIUnit

}

class DefaultAngularVelocityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AngularVelocityUnit


object AngularVelocityUnitObjects{
  import org.waman.multiverse.unit.Constants

  import org.waman.multiverse.unit.basic.TimeUnitObjects

  val getSIUnit: AngularVelocityUnit = AngleUnitObjects.getSIUnit / TimeUnitObjects.getSIUnit

  final object cycle_per_second extends DefaultAngularVelocityUnit("cycle per second", "cps", Nil, r"2" * Constants.Pi)
  final object revolution_per_minute extends DefaultAngularVelocityUnit("revolution per minute", "rpm", Nil, r"2" * Constants.Pi / r"60")

  def getUnits: Seq[AngularVelocityUnit] =
    Seq(cycle_per_second, revolution_per_minute)
}


object AngularVelocityUnits{
  def cps: AngularVelocityUnit = AngularVelocityUnitObjects.cycle_per_second
  def rpm: AngularVelocityUnit = AngularVelocityUnitObjects.revolution_per_minute

  def getSIUnit: AngularVelocityUnit = AngularVelocityUnitObjects.getSIUnit
  def getUnits: Seq[AngularVelocityUnit] = AngularVelocityUnitObjects.getUnits
}
