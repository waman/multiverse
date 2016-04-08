package org.waman.multiverse.fluid

import org.waman.multiverse._
import org.waman.multiverse.metric.{VolumePer, VolumePostfixOps, VolumeUnit}
import org.waman.multiverse.time.{DivisibleByTimeUnit, TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

class VolumeFlow[A: Fractional](val value: A, val unit: VolumeFlowUnit) extends Quantity[A, VolumeFlowUnit]
  with VolumeFlowPostfixOps[A]
  with VolumePostfixOps[DivisibleByTimeUnit[A]]
  with VolumePer[TimePostfixOps[A]]
  with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: VolumeFlowUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def volumeFlowPostfixOps(volumeFlowUnit: VolumeFlowUnit) = apply(volumeFlowUnit)

  override protected def volumePostfixOps(volumeUnit: VolumeUnit) = new DivisibleByTimeUnit[A]{
    override def /(timeUnit: TimeUnit): A = apply(volumeUnit / timeUnit)
  }

  override protected def volumePer(volumeUnit: VolumeUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit): A = apply(volumeUnit / timeUnit)
  }
}

trait VolumeFlowFactory[A]
  extends VolumeFlowPostfixOps[VolumeFlow[A]]{

  def apply(unit: VolumeFlowUnit): VolumeFlow[A]

  override protected def volumeFlowPostfixOps(volumeFlowUnit: VolumeFlowUnit) = apply(volumeFlowUnit)
}