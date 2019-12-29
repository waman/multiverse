package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


import org.waman.multiverse.unit.mechanics.Acceleration
import org.waman.multiverse.unit.mechanics.AccelerationUnit


class Velocity[A: Fractional](val value: A, val unit: VelocityUnit)
    extends LinearQuantity[Velocity[A], A, VelocityUnit] {

  override protected def newQuantity(value: A, unit: VelocityUnit): Velocity[A] = new Velocity(value, unit)

  def /(time: Time[A]): Acceleration[A] = new Acceleration(this.value / time.value, this.unit / time.unit)

}

trait VelocityUnit extends LinearUnit[VelocityUnit]{

  override def getSIUnit: VelocityUnit = VelocityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = VelocityUnit.dimension

  def /(timeUnit: TimeUnit): AccelerationUnit =
    new AbstractQuotientUnit[AccelerationUnit, VelocityUnit, TimeUnit](VelocityUnit.this, timeUnit) with AccelerationUnit

}

object VelocityUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, L -> 1).withDefaultValue(0)

  val getSIUnit: VelocityUnit = LengthUnit.getSIUnit / TimeUnit.getSIUnit

  import VelocityUnitObjects._
  def getUnits: Seq[VelocityUnit] =
    Seq(speed_of_light, mach_number)
}

class DefaultVelocityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VelocityUnit

object VelocityUnitObjects{
  import org.waman.multiverse.unit.Constants

  final object speed_of_light extends DefaultVelocityUnit("speed of light", "c", Nil, Constants.SpeedOfLight)
  final object mach_number extends DefaultVelocityUnit("mach number", "M", Nil, r"340") with NotExact
}

object VelocityUnits{
  def c: VelocityUnit = VelocityUnitObjects.speed_of_light
  def M: VelocityUnit = VelocityUnitObjects.mach_number
}