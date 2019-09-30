package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

import org.waman.multiverse._

class Velocity[A: Fractional](val value: A, val unit: VelocityUnit)
    extends LinearQuantity[Velocity[A], A, VelocityUnit] {

  override protected def newQuantity(value: A, unit: VelocityUnit): Velocity[A] = new Velocity(value, unit)
}

trait VelocityUnit extends LinearUnit[VelocityUnit]{
  override def getSIUnit: VelocityUnit = VelocityUnitObjects.getSIUnit

  import org.waman.multiverse.unit.mechanics.AccelerationUnit

  def /(timeUnit: TimeUnit): AccelerationUnit =
    new QuotientUnit[AccelerationUnit, VelocityUnit, TimeUnit](VelocityUnit.this, timeUnit) with AccelerationUnit

}

class DefaultVelocityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VelocityUnit


object VelocityUnitObjects{
  import org.waman.multiverse.unit.Constants


  val getSIUnit: VelocityUnit = LengthUnitObjects.getSIUnit / TimeUnitObjects.getSIUnit

  final object speed_of_light extends DefaultVelocityUnit("speed of light", "c", Nil, Constants.SpeedOfLight)
  final object mach_number extends DefaultVelocityUnit("mach number", "M", Nil, r"340") with NotExact

  def getUnits: Seq[VelocityUnit] =
    Seq(speed_of_light, mach_number)
}


object VelocityUnits{
  def c: VelocityUnit = VelocityUnitObjects.speed_of_light
  def M: VelocityUnit = VelocityUnitObjects.mach_number

  def getSIUnit: VelocityUnit = VelocityUnitObjects.getSIUnit
  def getUnits: Seq[VelocityUnit] = VelocityUnitObjects.getUnits
}
