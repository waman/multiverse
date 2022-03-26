package org.waman.multiverse.unit.defs.angle

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.Constants

class AngularVelocity[A: Fractional](val value: A, val unit: AngularVelocityUnit)
    extends LinearQuantity[AngularVelocity[A], A, AngularVelocityUnit] {


  def toFrequency: Frequency[A] = new Frequency(
      apply(AngleUnitObjects.radian / TimeUnitObjects.second) * implicitly[Fractional[A]].fromReal(r"1" / (r"2" * Constants.Pi)),
      FrequencyUnitObjects.heltz)

  override protected def newQuantity(value: A, unit: AngularVelocityUnit): AngularVelocity[A] = new AngularVelocity(value, unit)
}

/** None */
trait AngularVelocityUnit extends LinearUnit[AngularVelocityUnit]{

  override def getSIUnit: AngularVelocityUnit = AngularVelocityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AngularVelocityUnit.dimension
}

object AngularVelocityUnit extends UnitInfo[AngularVelocityUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1).withDefaultValue(0)

  val getSIUnit: AngularVelocityUnit = AngleUnit.getSIUnit / TimeUnit.getSIUnit

  import AngularVelocityUnitObjects._

  def getUnits: Seq[AngularVelocityUnit] =
    Seq(cycle_per_second, revolution_per_minute)
}


/** For no alias or user defined units */
class SimpleAngularVelocityUnit(val name: String, val symbol: String, val interval: Real) extends AngularVelocityUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAngularVelocityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AngularVelocityUnit
  
object AngularVelocityUnitObjects{

  final case object cycle_per_second extends SimpleAngularVelocityUnit("cycle per second", "cps", r"2" * Constants.Pi)
  final case object revolution_per_minute extends SimpleAngularVelocityUnit("revolution per minute", "rpm", r"2" * Constants.Pi / r"60")
}


object AngularVelocityUnits{

  /** cycle per second */
  def cps: AngularVelocityUnit = AngularVelocityUnitObjects.cycle_per_second
  /** revolution per minute */
  def rpm: AngularVelocityUnit = AngularVelocityUnitObjects.revolution_per_minute
}