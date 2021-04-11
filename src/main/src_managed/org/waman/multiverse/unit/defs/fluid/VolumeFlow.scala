package org.waman.multiverse.unit.defs.fluid

import spire.math._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._

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

  val getSIUnit: VolumeFlowUnit = VolumeUnit.getSIUnit / TimeUnit.getSIUnit
  import VolumeFlowUnitObjects._

  def getUnits: Seq[VolumeFlowUnit] =
    Seq(litre_per_minute, gallon_per_minute, gallon_per_hour, gallon_per_day, cubic_centimetre_per_second, cubic_centimetre_per_minute, cubic_foot_per_second, cubic_foot_per_minute, cubic_oot_per_hour)
}


/** For no aliase or user defined units */
class SimpleVolumeFlowUnit(val name: String, val symbol: String, val interval: Real) extends VolumeFlowUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultVolumeFlowUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VolumeFlowUnit
  
object VolumeFlowUnitObjects{

  final case object litre_per_minute extends SimpleVolumeFlowUnit("litre per minute", "LPM", VolumeUnitObjects.litre.interval / TimeUnitObjects.minute.interval)
  final case object gallon_per_minute extends SimpleVolumeFlowUnit("gallon per minute", "GPM", VolumeUnitObjects.`gallon(US_fl)`.interval / TimeUnitObjects.minute.interval)
  final case object gallon_per_hour extends SimpleVolumeFlowUnit("gallon per hour", "GPH", VolumeUnitObjects.`gallon(US_fl)`.interval / TimeUnitObjects.hour.interval)
  final case object gallon_per_day extends SimpleVolumeFlowUnit("gallon per day", "GPD", VolumeUnitObjects.`gallon(US_fl)`.interval / TimeUnitObjects.day.interval)
  final case object cubic_centimetre_per_second extends SimpleVolumeFlowUnit("cubic centimetre per second", "ccs", VolumeUnitObjects.cubic_centimetre.interval / TimeUnitObjects.second.interval)
  final case object cubic_centimetre_per_minute extends SimpleVolumeFlowUnit("cubic centimetre per minute", "ccm", VolumeUnitObjects.cubic_centimetre.interval / TimeUnitObjects.minute.interval)
  final case object cubic_foot_per_second extends SimpleVolumeFlowUnit("cubic foot per second", "cfs", VolumeUnitObjects.cubic_foot.interval / TimeUnitObjects.second.interval)
  final case object cubic_foot_per_minute extends DefaultVolumeFlowUnit("cubic foot per minute", "cfm", Seq("CFM"), VolumeUnitObjects.cubic_foot.interval / TimeUnitObjects.minute.interval)
  final case object cubic_oot_per_hour extends SimpleVolumeFlowUnit("cubic oot per hour", "cfh", VolumeUnitObjects.cubic_foot.interval / TimeUnitObjects.hour.interval)
}


object VolumeFlowUnits{

  /** litre per minute */
  def LPM: VolumeFlowUnit = VolumeFlowUnitObjects.litre_per_minute
  /** gallon per minute */
  def GPM: VolumeFlowUnit = VolumeFlowUnitObjects.gallon_per_minute
  /** gallon per hour */
  def GPH: VolumeFlowUnit = VolumeFlowUnitObjects.gallon_per_hour
  /** gallon per day */
  def GPD: VolumeFlowUnit = VolumeFlowUnitObjects.gallon_per_day
  /** cubic centimetre per second */
  def ccs: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_centimetre_per_second
  /** cubic centimetre per minute */
  def ccm: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_centimetre_per_minute
  /** cubic foot per second */
  def cfs: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_foot_per_second
  /** cubic foot per minute */
  def cfm: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_foot_per_minute
  /** cubic foot per minute */
  def CFM: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_foot_per_minute
  /** cubic oot per hour */
  def cfh: VolumeFlowUnit = VolumeFlowUnitObjects.cubic_oot_per_hour
}