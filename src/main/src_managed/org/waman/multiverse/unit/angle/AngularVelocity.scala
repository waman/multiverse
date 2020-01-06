package org.waman.multiverse.unit.angle

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class AngularVelocity[A: Fractional](val value: A, val unit: AngularVelocityUnit)
    extends LinearQuantity[AngularVelocity[A], A, AngularVelocityUnit] {

  override protected def newQuantity(value: A, unit: AngularVelocityUnit): AngularVelocity[A] = new AngularVelocity(value, unit)

  import org.waman.multiverse.unit.Constants
  import org.waman.multiverse.unit.basic.TimeUnitObjects

  def toFrequency: Frequency[A] = new Frequency(
      apply(AngleUnitObjects.radian / TimeUnitObjects.second) * implicitly[Fractional[A]].fromReal(r"1" / (r"2" * Constants.Pi)),
      FrequencyUnitObjects.heltz)

}

trait AngularVelocityUnit extends LinearUnit[AngularVelocityUnit]{

  override def getSIUnit: AngularVelocityUnit = AngularVelocityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AngularVelocityUnit.dimension

}

/** For user defined units */
class SimpleAngularVelocityUnit(val name: String, val symbol: String, val interval: Real) extends AngularVelocityUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultAngularVelocityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AngularVelocityUnit

object AngularVelocityUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.TimeUnit
  val getSIUnit: AngularVelocityUnit = AngleUnit.getSIUnit / TimeUnit.getSIUnit

  import AngularVelocityUnitObjects._
  def getUnits: Seq[AngularVelocityUnit] =
    Seq(cycle_per_second, revolution_per_minute)
}

object AngularVelocityUnitObjects{
  import org.waman.multiverse.unit.Constants

  final case object cycle_per_second extends DefaultAngularVelocityUnit("cycle per second", "cps", Nil, r"2" * Constants.Pi)
  final case object revolution_per_minute extends DefaultAngularVelocityUnit("revolution per minute", "rpm", Nil, r"2" * Constants.Pi / r"60")
}

object AngularVelocityUnits{
  def cps: AngularVelocityUnit = AngularVelocityUnitObjects.cycle_per_second
  def rpm: AngularVelocityUnit = AngularVelocityUnitObjects.revolution_per_minute
}