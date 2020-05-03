package waman.multiverse.unit.fluid

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


class VolumeFlow[A: Fractional](val value: A, val unit: VolumeFlowUnit)
    extends LinearQuantity[VolumeFlow[A], A, VolumeFlowUnit] {

  override protected def newQuantity(value: A, unit: VolumeFlowUnit): VolumeFlow[A] = new VolumeFlow(value, unit)
}

trait VolumeFlowUnit extends LinearUnit[VolumeFlowUnit]{

  override def getSIUnit: VolumeFlowUnit = VolumeFlowUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = VolumeFlowUnit.dimension
}

object VolumeFlowUnit extends UnitInfo[VolumeFlowUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, L -> 3).withDefaultValue(0)

  import waman.multiverse.unit.basic.VolumeUnit
  import waman.multiverse.unit.basic.TimeUnit
  val getSIUnit: VolumeFlowUnit = VolumeUnit.getSIUnit / TimeUnit.getSIUnit

  import VolumeFlowUnitObjects._
  def getUnits: Seq[VolumeFlowUnit] =
    Seq(litre_per_minute, gallon_per_minute, gallon_per_hour, gallon_per_day, cubic_centimetre_per_second, cubic_centimetre_per_minute, cubic_foot_per_second, cubic_foot_per_minute, cubic_foot_per_hour)
}

/** For no aliase or user defined units */
class SimpleVolumeFlowUnit(val name: String, val symbol: String, val interval: Real) extends VolumeFlowUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultVolumeFlowUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VolumeFlowUnit

object VolumeFlowUnitObjects{

  import waman.multiverse.unit.basic.VolumeUnitObjects._
  import waman.multiverse.unit.basic.TimeUnitObjects._

  final case object litre_per_minute extends SimpleVolumeFlowUnit("litre per minute", "LPM", litre.interval / minute.interval)
  final case object gallon_per_minute extends SimpleVolumeFlowUnit("gallon per minute", "GPM", `gallon(US_fl)`.interval / minute.interval)
  final case object gallon_per_hour extends SimpleVolumeFlowUnit("gallon per hour", "GPH", `gallon(US_fl)`.interval / hour.interval)
  final case object gallon_per_day extends SimpleVolumeFlowUnit("gallon per day", "GPD", `gallon(US_fl)`.interval / day.interval)
  final case object cubic_centimetre_per_second extends SimpleVolumeFlowUnit("cubic_centimetre per second", "ccs", cubic_centimetre.interval / second.interval)
  final case object cubic_centimetre_per_minute extends SimpleVolumeFlowUnit("cubic_centimetre per minute", "ccm", cubic_centimetre.interval / minute.interval)
  final case object cubic_foot_per_second extends SimpleVolumeFlowUnit("cubic_foot per second", "cfs", cubic_foot.interval / second.interval)
  final case object cubic_foot_per_minute extends DefaultVolumeFlowUnit("cubic_foot per minute", "cfm", Seq("CFM"), cubic_foot.interval / minute.interval)
  final case object cubic_foot_per_hour extends SimpleVolumeFlowUnit("cubic_foot per hour", "cfh", cubic_foot.interval / hour.interval)
}

object VolumeFlowUnits{

  def LPM: VolumeFlowUnit = VolumeFlowUnitObjects.litre_per_minute
  def GPM: VolumeFlowUnit = VolumeFlowUnitObjects.gallon_per_minute
  def GPH: VolumeFlowUnit = VolumeFlowUnitObjects.gallon_per_hour
  def GPD: VolumeFlowUnit = VolumeFlowUnitObjects.gallon_per_day
  def ccs: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_centimetre_per_second
  def ccm: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_centimetre_per_minute
  def cfs: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_foot_per_second
  def cfm: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_foot_per_minute
  def CFM: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_foot_per_minute
  def cfh: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_foot_per_hour
}