package org.waman.multiverse.fluid

import org.waman.multiverse._
import org.waman.multiverse.metric.{VolumePer, VolumePostfixOps, VolumeUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait VolumeFlowPostfixOps[A]{
  import VolumeFlowUnit._

  protected def volumeFlowPostfixOps(volumeFlowUnit: VolumeFlowUnit): A

  def LPM: A = volumeFlowPostfixOps(LitrePerMinute)

  def CFM: A = volumeFlowPostfixOps(CubicFootPerMinute)

  def GPM: A = volumeFlowPostfixOps(GallonPerMinute)
  def GPH: A = volumeFlowPostfixOps(GallonPerHour)
  def GPD: A = volumeFlowPostfixOps(GallonPerDay)
}

class VolumeFlow[A: Fractional](val value: A, val unit: VolumeFlowUnit) extends Quantity[A, VolumeFlowUnit]
  with VolumeFlowPostfixOps[A]
  with VolumePostfixOps[DivisibleByTimeUnit[A]]
  with VolumePer[TimePostfixOps[A]]
  with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: VolumeFlowUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCubicMetrePerSecond) / real(evalUnit.unitInCubicMetrePerSecond)

  override protected def volumeFlowPostfixOps(volumeFlowUnit: VolumeFlowUnit) = apply(volumeFlowUnit)

  override protected def volumePostfixOps(volumeUnit: VolumeUnit) = new DivisibleByTimeUnit[A]{
    override def /(timeUnit: TimeUnit): A = apply(volumeUnit / timeUnit)
  }

  override protected def volumePer(volumeUnit: VolumeUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit): A = apply(volumeUnit / timeUnit)
  }
}

sealed abstract class VolumeFlowUnit extends PhysicalUnit[VolumeFlowUnit]{

  def unitInCubicMetrePerSecond: Real

  override def baseUnit = VolumeUnit.CubicMetre / TimeUnit.Second
  override def valueInBaseUnit = unitInCubicMetrePerSecond
}

object VolumeFlowUnit extends ConstantsDefined[VolumeFlowUnit]{
  import TimeUnit._
  import VolumeUnit._

  private[VolumeFlowUnit]
  abstract class IntrinsicVolumeFlowUnit(val symbol: String, val unitInCubicMetrePerSecond: Real)
      extends VolumeFlowUnit{

    def this(symbol: String, vUnit: VolumeUnit, tUnit: TimeUnit) =
      this(symbol, vUnit.unitInCubicMetre / tUnit.unitInSecond)
  }

  case object LitrePerMinute extends IntrinsicVolumeFlowUnit("LPM", Litre, Minute)

  case object CubicFootPerMinute extends IntrinsicVolumeFlowUnit("CFM", CubicFoot, Minute)

  case object GallonPerMinute extends IntrinsicVolumeFlowUnit("GPM", Gallon_US_fluid, Minute)
  case object GallonPerHour   extends IntrinsicVolumeFlowUnit("GPH", Gallon_US_fluid, Hour)
  case object GallonPerDay    extends IntrinsicVolumeFlowUnit("GPD", Gallon_US_fluid, Day)

  override lazy val values = Seq(
    LitrePerMinute,
    CubicFootPerMinute,
    GallonPerMinute,
    GallonPerHour,
    GallonPerDay
  )

  private[VolumeFlowUnit]
  class QuotientVolumeFlowUnit(val numeratorUnit: VolumeUnit, val denominatorUnit: TimeUnit)
    extends VolumeFlowUnit with QuotientUnit[VolumeFlowUnit, VolumeUnit, TimeUnit]{

    override lazy val unitInCubicMetrePerSecond: Real =
      numeratorUnit.unitInCubicMetre / denominatorUnit.unitInSecond
  }

  def apply(lUnit: VolumeUnit, tUnit: TimeUnit): VolumeFlowUnit =
    new QuotientVolumeFlowUnit(lUnit, tUnit)
}

trait PredefinedVolumeFlowUnit extends VolumeFlowPostfixOps[VolumeFlowUnit]{
  override protected def volumeFlowPostfixOps(volumeFlowUnit: VolumeFlowUnit) = volumeFlowUnit
}

object PredefinedVolumeFlowUnit extends PredefinedVolumeFlowUnit

trait VolumeFlowFactory[A]
  extends VolumeFlowPostfixOps[VolumeFlow[A]]{

  def apply(unit: VolumeFlowUnit): VolumeFlow[A]

  override protected def volumeFlowPostfixOps(volumeFlowUnit: VolumeFlowUnit) = apply(volumeFlowUnit)
}