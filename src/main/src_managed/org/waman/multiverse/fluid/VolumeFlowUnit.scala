package org.waman.multiverse.fluid

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._
import org.waman.multiverse.time._

sealed trait VolumeFlowUnit extends PhysicalUnit[VolumeFlowUnit]{

  def unitInCubicMetrePerSecond: Real

  override def baseUnit = VolumeUnit.CubicMetre / TimeUnit.Second
  override def valueInBaseUnit = unitInCubicMetrePerSecond
}

object VolumeFlowUnit extends ConstantsDefined[VolumeFlowUnit]{

  // intrinsic
  private[VolumeFlowUnit]
  class IntrinsicVolumeFlowUnit(name: String, val symbols: Seq[String], val unitInCubicMetrePerSecond: Real)
      extends VolumeFlowUnit{

    def this(name: String, symbols: Seq[String], unit: VolumeFlowUnit) =
      this(name, symbols, unit.unitInCubicMetrePerSecond)

    def this(name: String, symbols: Seq[String], factor: Real, unit: VolumeFlowUnit) =
      this(name, symbols, factor * unit.unitInCubicMetrePerSecond)
  }


  case object LitrePerMinute extends IntrinsicVolumeFlowUnit("LitrePerMinute", Seq("LPM"), VolumeUnit.Litre / TimeUnit.Minute)
  case object CubicFootPerMinute extends IntrinsicVolumeFlowUnit("CubicFootPerMinute", Seq("CFM"), VolumeUnit.CubicFoot / TimeUnit.Minute)
  case object GallonPerMinute extends IntrinsicVolumeFlowUnit("GallonPerMinute", Seq("GPM"), VolumeUnit.Gallon_US_fluid / TimeUnit.Minute)
  case object GallonPerHour extends IntrinsicVolumeFlowUnit("GallonPerHour", Seq("GPH"), VolumeUnit.Gallon_US_fluid / TimeUnit.Hour)
  case object GallonPerDay extends IntrinsicVolumeFlowUnit("GallonPerDay", Seq("GPD"), VolumeUnit.Gallon_US_fluid / TimeUnit.Day)

  override lazy val values = Seq(LitrePerMinute, CubicFootPerMinute, GallonPerMinute, GallonPerHour, GallonPerDay)

  // VolumeUnit / TimeUnit -> VolumeFlow
  private[VolumeFlowUnit]
  class QuotientVolumePerTimeUnit(val numeratorUnit: VolumeUnit, val denominatorUnit: TimeUnit)
      extends VolumeFlowUnit with QuotientUnit[VolumeFlowUnit, VolumeUnit, TimeUnit]{

    override lazy val unitInCubicMetrePerSecond: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: VolumeUnit, dUnit: TimeUnit): VolumeFlowUnit =
    new QuotientVolumePerTimeUnit(nUnit, dUnit)
}

trait MultiplicativeByVolumeFlowUnit[R]{
  def *(unit: VolumeFlowUnit): R
}

trait DivisibleByVolumeFlowUnit[R]{
  def /(unit: VolumeFlowUnit): R
}

trait VolumeFlowPostfixOps[A]{
  import VolumeFlowUnit._

  protected def volumeFlowPostfixOps(unit: VolumeFlowUnit): A


  def LPM : A = volumeFlowPostfixOps(LitrePerMinute)
  def CFM : A = volumeFlowPostfixOps(CubicFootPerMinute)
  def GPM : A = volumeFlowPostfixOps(GallonPerMinute)
  def GPH : A = volumeFlowPostfixOps(GallonPerHour)
  def GPD : A = volumeFlowPostfixOps(GallonPerDay)
}

trait VolumeFlowDot[A]{
  import VolumeFlowUnit._

  protected def volumeFlowDot(unit: VolumeFlowUnit): A

  def LPM(dot: Dot): A = volumeFlowDot(LitrePerMinute)
  def CFM(dot: Dot): A = volumeFlowDot(CubicFootPerMinute)
  def GPM(dot: Dot): A = volumeFlowDot(GallonPerMinute)
  def GPH(dot: Dot): A = volumeFlowDot(GallonPerHour)
  def GPD(dot: Dot): A = volumeFlowDot(GallonPerDay)
}

trait VolumeFlowPer[A]{
  import VolumeFlowUnit._

  protected def volumeFlowPer(unit: VolumeFlowUnit): A

  def LPM(per: Per): A = volumeFlowPer(LitrePerMinute)
  def CFM(per: Per): A = volumeFlowPer(CubicFootPerMinute)
  def GPM(per: Per): A = volumeFlowPer(GallonPerMinute)
  def GPH(per: Per): A = volumeFlowPer(GallonPerHour)
  def GPD(per: Per): A = volumeFlowPer(GallonPerDay)
}

trait PredefinedVolumeFlowUnit extends VolumeFlowPostfixOps[VolumeFlowUnit]{
  override protected def volumeFlowPostfixOps(unit: VolumeFlowUnit) = unit
  
}

object PredefinedVolumeFlowUnit extends PredefinedVolumeFlowUnit
