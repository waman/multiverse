package org.waman.multiverse.metric

import org.waman.multiverse._
import org.waman.multiverse.fluid.{VolumeFlow, VolumeFlowUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

class Volume[A: Fractional](val value: A, val unit: VolumeUnit)
    extends Quantity[A, VolumeUnit]
    with VolumePostfixOps[A]
    with AreaPostfixOps[MultiplicativeByLengthUnit[A]]
    with AreaDot[LengthPostfixOps[A]]
    with DivisibleByTimeUnit[VolumeFlow[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: VolumeUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCubicMetre) / real(evalUnit.unitInCubicMetre)

  override protected def volumePostfixOps(volumeUnit: VolumeUnit) = apply(volumeUnit)

  override protected def areaPostfixOps(areaUnit: AreaUnit) = new MultiplicativeByLengthUnit[A] {
    override def *(lengthUnit: LengthUnit) = apply(areaUnit * lengthUnit)
  }

  override protected def areaDot(areaUnit: AreaUnit) = new LengthPostfixOps[A]{
    override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(areaUnit * lengthUnit)
  }

  override def /(timeUnit: TimeUnit): VolumeFlow[A] =
    new VolumeFlow(value, unit / timeUnit)
}

trait VolumeFactory[A] extends VolumePostfixOps[Volume[A]]
  with VolumePer[TimePostfixOps[VolumeFlow[A]]]{

  def apply(unit: VolumeUnit): Volume[A]

  override protected def volumePostfixOps(volumeUnit: VolumeUnit) = apply(volumeUnit)

  // Volume / Time -> VolumeFlow
  def apply(volumeFlowUnit: VolumeFlowUnit): VolumeFlow[A]

  override protected def volumePer(volumeUnit: VolumeUnit) =  new TimePostfixOps[VolumeFlow[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(volumeUnit / timeUnit)
  }
}