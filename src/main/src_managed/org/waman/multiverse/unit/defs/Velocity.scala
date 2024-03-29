package org.waman.multiverse.unit.defs

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs.mech._
import org.waman.multiverse.Constants

class Velocity[A: Fractional](val value: A, val unit: VelocityUnit)
    extends LinearQuantity[Velocity[A], A, VelocityUnit] {

  override protected def newQuantity(value: A, unit: VelocityUnit): Velocity[A] = new Velocity(value, unit)

  def /(time: Time[A]): Acceleration[A] = new Acceleration(this.value / time.value, this.unit / time.unit)
}

/** None */
trait VelocityUnit extends LinearUnit[VelocityUnit]{

  override def getSIUnit: VelocityUnit = VelocityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = VelocityUnit.dimension

  def /(timeUnit: TimeUnit): AccelerationUnit =
    new QuotientUnit[AccelerationUnit, VelocityUnit, TimeUnit](VelocityUnit.this, timeUnit) with AccelerationUnit
}

object VelocityUnit extends UnitInfo[VelocityUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, L -> 1).withDefaultValue(0)

  val getSIUnit: VelocityUnit = LengthUnit.getSIUnit / TimeUnit.getSIUnit

  import VelocityUnitObjects._

  def getUnits: Seq[VelocityUnit] =
    Seq(speed_of_light, mach_number, knot, kine)
}


/** For no alias or user defined units */
class SimpleVelocityUnit(val name: String, val symbol: String, val interval: Real) extends VelocityUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultVelocityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VelocityUnit
  
object VelocityUnitObjects{

  final case object speed_of_light extends SimpleVelocityUnit("speed of light", "c", Constants.SpeedOfLight)
  final case object mach_number extends SimpleVelocityUnit("mach number", "M", r"340") with NotExact
  final case object knot extends DefaultVelocityUnit("knot", "kn", Seq("kt"), LengthUnitObjects.nautical_mile.interval / TimeUnitObjects.hour.interval)
  final case object kine extends SimpleVelocityUnit("kine", "kine", LengthUnitObjects.centimetre.interval / TimeUnitObjects.second.interval)
}


object VelocityUnits{

  /** speed of light */
  def c: VelocityUnit = VelocityUnitObjects.speed_of_light
  /** mach number */
  def M: VelocityUnit = VelocityUnitObjects.mach_number
  /** knot */
  def kn: VelocityUnit = VelocityUnitObjects.knot
  /** knot */
  def kt: VelocityUnit = VelocityUnitObjects.knot
  /** kine */
  def kine: VelocityUnit = VelocityUnitObjects.kine
}