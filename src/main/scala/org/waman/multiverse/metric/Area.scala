package org.waman.multiverse.metric

import org.waman.multiverse._
import org.waman.multiverse.fluid.{KinematicViscosity, KinematicViscosityUnit}
import org.waman.multiverse.time.{DivisibleByTimeUnit, TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

class Area[A: Fractional](val value: A, val unit: AreaUnit)
    extends Quantity[A, AreaUnit]
    with AreaPostfixOps[A]
    with LengthPostfixOps[MultiplicativeByLengthUnit[A]]
    with LengthDot[LengthPostfixOps[A]]
    with MultiplicativeByLengthUnit[Volume[A]]
    with DivisibleByTimeUnit[KinematicViscosity[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AreaUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSquareMetre) / real(evalUnit.unitInSquareMetre)

  override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(areaUnit)

  override protected def lengthPostfixOps(lengthUnit1: LengthUnit) = new MultiplicativeByLengthUnit[A]{
    override def *(lengthUnit2: LengthUnit) = apply(lengthUnit1 * lengthUnit2)
  }

  override protected def lengthDot(lengthUnit1: LengthUnit) = new LengthPostfixOps[A]{
    override protected def lengthPostfixOps(lengthUnit2: LengthUnit) = apply(lengthUnit1 * lengthUnit2)
  }

  override def *(lengthUnit: LengthUnit) = new Volume(value, unit * lengthUnit)

  override def /(timeUnit: TimeUnit) = new KinematicViscosity[A](value, unit / timeUnit)
}

trait AreaFactory[A]
    extends AreaPostfixOps[Area[A]]
    with AreaDot[LengthPostfixOps[Volume[A]]]
    with AreaPer[TimePostfixOps[KinematicViscosity[A]]]{

  def apply(unit: AreaUnit): Area[A]

  override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(areaUnit)

  // Area * Length -> Volume
  protected def apply(unit: VolumeUnit): Volume[A]

  override protected def areaDot(areaUnit: AreaUnit) = new LengthPostfixOps[Volume[A]]{
    override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(areaUnit * lengthUnit)
  }

  // Area / Time -> KinematicViscosity
  protected def apply(unit: KinematicViscosityUnit): KinematicViscosity[A]

  override protected def areaPer(areaUnit: AreaUnit) = new TimePostfixOps[KinematicViscosity[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(areaUnit / timeUnit)
  }
}