package org.waman.multiverse.angle

import org.waman.multiverse.MultiverseUtil.twoPi
import org.waman.multiverse._
import org.waman.multiverse.time._
import spire.implicits._
import spire.math._

class AngularVelocity[A: Fractional](val value: A, val unit: AngularVelocityUnit)
    extends Quantity[A, AngularVelocityUnit]
    with AngularVelocityPostfixOps[A]
    with AnglePostfixOps[DivisibleByTimeUnit[A]]
    with AnglePer[TimePostfixOps[A]]
    with UnitConverter[A]{

  protected override lazy val algebra: Fractional[A] = implicitly[Fractional[A]]

  def apply(evalUnit: AngularVelocityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def angularVelocityPostfixOps(angularVelocityUnit: AngularVelocityUnit) =
    apply(angularVelocityUnit)

  override protected def anglePostfixOps(angleUnit: AngleUnit) = new DivisibleByTimeUnit[A]{
    override def /(timeUnit: TimeUnit): A = apply(angleUnit / timeUnit)
  }

  override protected def anglePer(angleUnit: AngleUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(angleUnit / timeUnit)
  }

  def toFrequency: Frequency[A] = new Frequency(
    div(rad(UnitSystem./).s, twoPi),
    FrequencyUnit.Heltz)
}

trait AngularVelocityFactory[A]
    extends AngularVelocityPostfixOps[AngularVelocity[A]]{

  def apply(unit: AngularVelocityUnit): AngularVelocity[A]

  override protected def angularVelocityPostfixOps(angularVelocityUnit: AngularVelocityUnit) =
    apply(angularVelocityUnit)
}
