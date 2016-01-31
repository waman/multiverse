package org.waman.multiverse

import spire.math._
import spire.implicits._

trait AngularVelocityPostfixOps[A]{

  protected def angularVelocityPostfixOps(angularVelocityUnit: AngularVelocityUnit): A

  def `rad/s`: A = angularVelocityPostfixOps(AngularVelocityUnit.RadianPerSecond)
  def `deg/s`: A = angularVelocityPostfixOps(AngularVelocityUnit.DegreePerSecond)
}

class AngularVelocity[A: Fractional](val value: A, val unit: AngularVelocityUnit)
    extends Quantity[A, AngularVelocityUnit]
    with AngularVelocityPostfixOps[A]
    with AnglePostfixOps[DivisibleBy[TimeUnit, A]]
    with AnglePer[TimePostfixOps[A]]
    with UnitConverter[A]{

  protected override lazy val algebra: Fractional[A] = implicitly[Fractional[A]]

  def apply(evalUnit: AngularVelocityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInRadianPerSecond) / real(evalUnit.unitInRadianPerSecond)

  override protected def angularVelocityPostfixOps(angularVelocityUnit: AngularVelocityUnit) =
    apply(angularVelocityUnit)

  override protected def anglePostfixOps(angleUnit: AngleUnit) = new DivisibleBy[TimeUnit, A]{
    override def /(timeUnit: TimeUnit): A = apply(angleUnit / timeUnit)
  }

  override protected def anglePer(angleUnit: AngleUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(angleUnit / timeUnit)
  }
}

trait AngularVelocityUnit extends PhysicalUnit{
  def unitInRadianPerSecond: Real

  override protected def baseUnit = AngularVelocityUnit.RadianPerSecond
  override protected def inBaseUnitAccessor = () => unitInRadianPerSecond
}

sealed class QuotientAngularVelocityUnit(val angleUnit: AngleUnit, val timeUnit: TimeUnit)
    extends AngularVelocityUnit with QuotientUnit[AngleUnit, TimeUnit]{

  override def numeratorUnit: AngleUnit = angleUnit
  override def denominatorUnit: TimeUnit = timeUnit

  override def unitInRadianPerSecond: Real = angleUnit.unitInRadian / timeUnit.unitInSecond
}

object AngularVelocityUnit{

  case object RadianPerSecond
    extends QuotientAngularVelocityUnit(AngleUnit.Radian, TimeUnit.Second)

  case object DegreePerSecond
    extends QuotientAngularVelocityUnit(AngleUnit.Degree, TimeUnit.Second)

  def apply(aUnit: AngleUnit, tUnit: TimeUnit): AngularVelocityUnit =
    new QuotientAngularVelocityUnit(aUnit, tUnit)
}

trait PredefinedAngularVelocityUnit{
  val `rad/s` = AngularVelocityUnit.RadianPerSecond
  val `deg/s` = AngularVelocityUnit.DegreePerSecond
}

object PredefinedAngularVelocityUnit extends PredefinedAngularVelocityUnit

trait AngularVelocityUnitInterpreter[A] extends AngularVelocityPostfixOps[AngularVelocity[A]]{

  def apply(unit: AngularVelocityUnit): AngularVelocity[A]

  override protected def angularVelocityPostfixOps(angularVelocityUnit: AngularVelocityUnit) =
    apply(angularVelocityUnit)
}
