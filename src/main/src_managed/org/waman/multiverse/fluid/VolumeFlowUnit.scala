package org.waman.multiverse.fluid

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._
import org.waman.multiverse.time._
import org.waman.multiverse.metric.VolumeUnit._
import org.waman.multiverse.time.TimeUnit._

sealed trait VolumeFlowUnit extends PhysicalUnit[VolumeFlowUnit]{

  override def getSIUnit = CubicMetre / Second
}

object VolumeFlowUnit extends ConstantsDefined[VolumeFlowUnit]{

  // intrinsic
  private[VolumeFlowUnit]
  class IntrinsicVolumeFlowUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends VolumeFlowUnit{

    def this(name: String, symbols: Seq[String], unit: VolumeFlowUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: VolumeFlowUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object LitrePerMinute extends IntrinsicVolumeFlowUnit("LitrePerMinute", Seq("LPM"), Litre / Minute)
  case object GallonPerMinute extends IntrinsicVolumeFlowUnit("GallonPerMinute", Seq("GPM"), Gallon_US_fluid / Minute)
  case object GallonPerHour extends IntrinsicVolumeFlowUnit("GallonPerHour", Seq("GPH"), Gallon_US_fluid / Hour)
  case object GallonPerDay extends IntrinsicVolumeFlowUnit("GallonPerDay", Seq("GPD"), Gallon_US_fluid / Day)
  case object CubicCentiMetrePerSecond extends IntrinsicVolumeFlowUnit("CubicCentiMetrePerSecond", Seq("ccs"), CubicCentiMetre / Second)
  case object CubicCentiMetrePerMinute extends IntrinsicVolumeFlowUnit("CubicCentiMetrePerMinute", Seq("ccm"), CubicCentiMetre / Minute)
  case object CubicFootPerSecond extends IntrinsicVolumeFlowUnit("CubicFootPerSecond", Seq("cfs"), CubicFoot / Second)
  case object CubicFootPerMinute extends IntrinsicVolumeFlowUnit("CubicFootPerMinute", Seq("cfm", "CFM"), CubicFoot / Minute)
  case object CubicFootPerHour extends IntrinsicVolumeFlowUnit("CubicFootPerHour", Seq("cfh"), CubicFoot / Hour)

  override lazy val values = Seq(LitrePerMinute, GallonPerMinute, GallonPerHour, GallonPerDay, CubicCentiMetrePerSecond, CubicCentiMetrePerMinute, CubicFootPerSecond, CubicFootPerMinute, CubicFootPerHour)

  // VolumeUnit / TimeUnit -> VolumeFlow
  private[VolumeFlowUnit]
  class QuotientVolumePerTimeUnit(val numeratorUnit: VolumeUnit, val denominatorUnit: TimeUnit)
      extends VolumeFlowUnit with QuotientUnit[VolumeFlowUnit, VolumeUnit, TimeUnit]{

    override lazy val unitValueInSIUnit: Real =
      numeratorUnit.unitValueInSIUnit / denominatorUnit.unitValueInSIUnit
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
  def GPM : A = volumeFlowPostfixOps(GallonPerMinute)
  def GPH : A = volumeFlowPostfixOps(GallonPerHour)
  def GPD : A = volumeFlowPostfixOps(GallonPerDay)
  def ccs : A = volumeFlowPostfixOps(CubicCentiMetrePerSecond)
  def ccm : A = volumeFlowPostfixOps(CubicCentiMetrePerMinute)
  def cfs : A = volumeFlowPostfixOps(CubicFootPerSecond)
  def cfm : A = volumeFlowPostfixOps(CubicFootPerMinute)
  def CFM : A = volumeFlowPostfixOps(CubicFootPerMinute)
  def cfh : A = volumeFlowPostfixOps(CubicFootPerHour)
}

trait VolumeFlowDot[A]{
  import VolumeFlowUnit._

  protected def volumeFlowDot(unit: VolumeFlowUnit): A

  def LPM(dot: Dot): A = volumeFlowDot(LitrePerMinute)
  def GPM(dot: Dot): A = volumeFlowDot(GallonPerMinute)
  def GPH(dot: Dot): A = volumeFlowDot(GallonPerHour)
  def GPD(dot: Dot): A = volumeFlowDot(GallonPerDay)
  def ccs(dot: Dot): A = volumeFlowDot(CubicCentiMetrePerSecond)
  def ccm(dot: Dot): A = volumeFlowDot(CubicCentiMetrePerMinute)
  def cfs(dot: Dot): A = volumeFlowDot(CubicFootPerSecond)
  def cfm(dot: Dot): A = volumeFlowDot(CubicFootPerMinute)
  def CFM(dot: Dot): A = volumeFlowDot(CubicFootPerMinute)
  def cfh(dot: Dot): A = volumeFlowDot(CubicFootPerHour)
}

trait VolumeFlowPer[A]{
  import VolumeFlowUnit._

  protected def volumeFlowPer(unit: VolumeFlowUnit): A

  def LPM(per: Per): A = volumeFlowPer(LitrePerMinute)
  def GPM(per: Per): A = volumeFlowPer(GallonPerMinute)
  def GPH(per: Per): A = volumeFlowPer(GallonPerHour)
  def GPD(per: Per): A = volumeFlowPer(GallonPerDay)
  def ccs(per: Per): A = volumeFlowPer(CubicCentiMetrePerSecond)
  def ccm(per: Per): A = volumeFlowPer(CubicCentiMetrePerMinute)
  def cfs(per: Per): A = volumeFlowPer(CubicFootPerSecond)
  def cfm(per: Per): A = volumeFlowPer(CubicFootPerMinute)
  def CFM(per: Per): A = volumeFlowPer(CubicFootPerMinute)
  def cfh(per: Per): A = volumeFlowPer(CubicFootPerHour)
}

trait PredefinedVolumeFlowUnit extends VolumeFlowPostfixOps[VolumeFlowUnit]{
  override protected def volumeFlowPostfixOps(unit: VolumeFlowUnit) = unit
  
}

object PredefinedVolumeFlowUnit extends PredefinedVolumeFlowUnit
