package org.waman.multiverse.time

import org.waman.multiverse.angle.{AngleUnit, AngularVelocity}
import org.waman.multiverse.{Quantity, UnitConverter}
import spire.implicits._
import spire.math.{Fractional, Real}

class Frequency[A: Fractional](val value: A, val unit: FrequencyUnit)
  extends Quantity[A, FrequencyUnit]
    with FrequencyPostfixOps[A]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: FrequencyUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def frequencyPostfixOps(frequencyUnit: FrequencyUnit) = apply(frequencyUnit)

  def toAngularVelocity: AngularVelocity[A] = new AngularVelocity(
    times(Hz, 2.0 * Real.pi),
    AngleUnit.Radian / TimeUnit.Second
  )
}

trait FrequencyFactory[A] extends FrequencyPostfixOps[Frequency[A]]{

  def apply(unit: FrequencyUnit): Frequency[A]

  override protected def frequencyPostfixOps(frequencyUnit: FrequencyUnit) = apply(frequencyUnit)
}