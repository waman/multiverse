package org.waman.multiverse.angle

import org.waman.multiverse._
import org.waman.multiverse.thermal.{DegreeTemperaturePostfixOps, Temperature, TemperatureUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional


trait DegreePostfixOps[A]{
  def ° : A
}

class Angle[A: Fractional](val value: A, val unit: AngleUnit)
    extends Quantity[A, AngleUnit]
    with AnglePostfixOps[A]
    with DivisibleByTimeUnit[AngularVelocity[A]]
    with DegreeTemperaturePostfixOps[Temperature[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AngleUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInRadian) / real(evalUnit.unitInRadian)

  override protected def anglePostfixOps(angleUnit: AngleUnit) = apply(angleUnit)

  override def /(timeUnit: TimeUnit): AngularVelocity[A] = new AngularVelocity(value, unit / timeUnit)

  override protected def degreeTemperaturePostfixOps(unit: TemperatureUnit) = new Temperature(value, unit)
}

trait AngleFactory[A]
    extends AnglePostfixOps[Angle[A]]
    with AnglePer[TimePostfixOps[AngularVelocity[A]]]{

  def apply(unit: AngleUnit): Angle[A]

  override protected def anglePostfixOps(angleUnit: AngleUnit) = apply(angleUnit)

  // Angle / Time -> AngularVelocity
  def apply(unit: AngularVelocityUnit): AngularVelocity[A]

  override protected def anglePer(angleUnit: AngleUnit) = new TimePostfixOps[AngularVelocity[A]] {
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(angleUnit / timeUnit)
  }
}