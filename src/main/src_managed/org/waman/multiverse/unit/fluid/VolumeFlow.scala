package org.waman.multiverse.unit.fluid

import spire.math.Real
import spire.math.Fractional
import org.waman.multiverse._

class VolumeFlow[A: Fractional](val value: A, val unit: VolumeFlowUnit)
    extends LinearQuantity[VolumeFlow[A], A, VolumeFlowUnit] {

  override protected def newQuantity(value: A, unit: VolumeFlowUnit): VolumeFlow[A] = new VolumeFlow(value, unit)
}

trait VolumeFlowUnit extends LinearUnit[VolumeFlowUnit]{

  override def getSIUnit: VolumeFlowUnit = VolumeFlowUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = VolumeFlowUnit.dimension

}

object VolumeFlowUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, L -> 3).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.VolumeUnit
  import org.waman.multiverse.unit.basic.TimeUnit
  val getSIUnit: VolumeFlowUnit = VolumeUnit.getSIUnit / TimeUnit.getSIUnit

  import VolumeFlowUnitObjects._
  def getUnits: Seq[VolumeFlowUnit] =
    Seq(litre_per_minute, gallon_per_minute, gallon_per_hour, gallon_per_day, cubic_centimetre_per_second, cubic_centimetre_per_minute, cubic_foot_per_second, cubic_foot_per_minute, cubic_foot_per_hour)
}



class DefaultVolumeFlowUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VolumeFlowUnit

object VolumeFlowUnitObjects{
  import org.waman.multiverse.unit.basic.VolumeUnitObjects
  import org.waman.multiverse.unit.basic.TimeUnitObjects

  final object litre_per_minute extends DefaultVolumeFlowUnit("litre per minute", "LPM", Nil, VolumeUnitObjects.litre.interval / TimeUnitObjects.minute.interval)
  final object gallon_per_minute extends DefaultVolumeFlowUnit("gallon per minute", "GPM", Nil, VolumeUnitObjects.`gallon(US_fl)`.interval / TimeUnitObjects.minute.interval)
  final object gallon_per_hour extends DefaultVolumeFlowUnit("gallon per hour", "GPH", Nil, VolumeUnitObjects.`gallon(US_fl)`.interval / TimeUnitObjects.hour.interval)
  final object gallon_per_day extends DefaultVolumeFlowUnit("gallon per day", "GPD", Nil, VolumeUnitObjects.`gallon(US_fl)`.interval / TimeUnitObjects.day.interval)
  final object cubic_centimetre_per_second extends DefaultVolumeFlowUnit("cubic_centimetre per second", "ccs", Nil, VolumeUnitObjects.cubic_centimetre.interval / TimeUnitObjects.second.interval)
  final object cubic_centimetre_per_minute extends DefaultVolumeFlowUnit("cubic_centimetre per minute", "ccm", Nil, VolumeUnitObjects.cubic_centimetre.interval / TimeUnitObjects.minute.interval)
  final object cubic_foot_per_second extends DefaultVolumeFlowUnit("cubic_foot per second", "cfs", Nil, VolumeUnitObjects.cubic_foot.interval / TimeUnitObjects.second.interval)
  final object cubic_foot_per_minute extends DefaultVolumeFlowUnit("cubic_foot per minute", "cfm", Seq("CFM"), VolumeUnitObjects.cubic_foot.interval / TimeUnitObjects.minute.interval)
  final object cubic_foot_per_hour extends DefaultVolumeFlowUnit("cubic_foot per hour", "cfh", Nil, VolumeUnitObjects.cubic_foot.interval / TimeUnitObjects.hour.interval)
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