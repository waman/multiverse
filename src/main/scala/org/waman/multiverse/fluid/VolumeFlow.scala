package org.waman.multiverse.fluid

import org.waman.multiverse._
import org.waman.multiverse.metric.{VolumePer, VolumePostfixOps, VolumeUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait VolumeFlowPostfixOps[A]{
  import VolumeFlowUnit._

  protected def volumeFlowPostfixOps(volumeFlowUnit: VolumeFlowUnit): A

//  def CFM: A = volumeFlowPostfixOps(CubicFootPerMinute)
//  def GPD: A = volumeFlowPostfixOps(GallonPerDay)
//  def GPH: A = volumeFlowPostfixOps(GallonPerHour)
//  def GPM: A = volumeFlowPostfixOps(GallonPerMinute)
  def LPM: A = volumeFlowPostfixOps(LitrePerMinute)
}

class VolumeFlow[A: Fractional](val value: A, val unit: VolumeFlowUnit) extends Quantity[A, VolumeFlowUnit]
  with VolumeFlowPostfixOps[A]
  with VolumePostfixOps[DivisibleByTime[A]]
  with VolumePer[TimePostfixOps[A]]
  with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: VolumeFlowUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCubicMetrePerSecond) / real(evalUnit.unitInCubicMetrePerSecond)

  override protected def volumeFlowPostfixOps(volumeFlowUnit: VolumeFlowUnit) = apply(volumeFlowUnit)

  override protected def volumePostfixOps(volumeUnit: VolumeUnit) = new DivisibleByTime[A]{
    override def /(timeUnit: TimeUnit): A = apply(volumeUnit / timeUnit)
  }

  override protected def volumePer(volumeUnit: VolumeUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit): A = apply(volumeUnit / timeUnit)
  }
}

sealed abstract class VolumeFlowUnit extends PhysicalUnit[VolumeFlowUnit]{

  def unitInCubicMetrePerSecond: Real

  override lazy val baseUnit = VolumeUnit.CubicMetre / TimeUnit.Second
  override lazy val inBaseUnitAccessor = () => unitInCubicMetrePerSecond
}

object VolumeFlowUnit{
  import TimeUnit._
  import VolumeUnit._

  abstract class VolumeFlowUnitImpl(val symbol: String, val unitInCubicMetrePerSecond: Real)
    extends VolumeFlowUnit

//  case object CubicFootPerMinute extends VolumeFlowUnitImpl("CFM", CubicFeet)
//  case object GallonPerDay       extends VolumeFlowUnitImpl("GPD", Real.pi / r"180")
//  case object GallonPerHour      extends VolumeFlowUnitImpl("GPH", Real.pi / r"180")
//  case object GallonPerMinute    extends VolumeFlowUnitImpl("GPM", Real.pi / r"200")
  case object LitrePerMinute     extends VolumeFlowUnitImpl("LPM", Litre.unitInCubicMetre / Minute.unitInSecond)


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

trait VolumeFlowUnitInterpreter[A]
  extends VolumeFlowPostfixOps[VolumeFlow[A]]{

  def apply(unit: VolumeFlowUnit): VolumeFlow[A]

  override protected def volumeFlowPostfixOps(volumeFlowUnit: VolumeFlowUnit) = apply(volumeFlowUnit)
}