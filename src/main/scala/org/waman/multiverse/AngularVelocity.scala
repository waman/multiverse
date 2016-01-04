package org.waman.multiverse

import spire.math._
import spire.implicits._

trait AngularVelocityPostfixOps[A]{
  def `rad/s`: A
  def `deg/s`: A
}

class AngularVelocity[A: Fractional](val value: A, val unit: AngularVelocityUnit)
    extends ValueWithUnit[A, AngularVelocityUnit]
    with AngularVelocityPostfixOps[A]
    with AnglePostfixOps[DivisibleByTime[A]]
    with AnglePer[TimePostfixOps[A]]
    with UnitConverter[A]{

  protected override lazy val algebra: Fractional[A] = implicitly[Fractional[A]]

  def apply(evalUnit: AngularVelocityUnit): A =
    if(evalUnit == unit) value
    else value * real(unit.inRadianPerSecond) / real(evalUnit.inRadianPerSecond)

  override def `rad/s`: A = apply(AngularVelocityUnit.RadianPerSecond)
  override def `deg/s`: A = apply(AngularVelocityUnit.DegreePerSecond)

  private def callAngle(angleUnit: AngleUnit) = new DivisibleByTime[A]{
    override def /(timeUnit: TimeUnit): A = apply(angleUnit / timeUnit)
  }

  override def rad = callAngle(AngleUnit.Radian)
  override def deg = callAngle(AngleUnit.Degree)

  private def callAnglePer(angleUnit: AngleUnit) = new TimePostfixOps[A]{
    override def ns     = apply(angleUnit / TimeUnit.Nanosecond)
    override def µs     = apply(angleUnit / TimeUnit.Microsecond)
    override def ms     = apply(angleUnit / TimeUnit.Millisecond)
    override def s      = apply(angleUnit / TimeUnit.Second)
    override def d      = apply(angleUnit / TimeUnit.Day)
    override def minute = apply(angleUnit / TimeUnit.Minute)
    override def h      = apply(angleUnit / TimeUnit.Hour)
  }

  override def rad(per: Per) = callAnglePer(AngleUnit.Radian)
  override def deg(per: Per) = callAnglePer(AngleUnit.Degree)
}

trait AngularVelocityUnit extends PhysicalUnit{
  def inRadianPerSecond: Real
}

trait CompositeAngularVelocityUnit extends AngularVelocityUnit{
  def angleUnit: AngleUnit
  def timeUnit: TimeUnit

  override val name: String = s"${angleUnit.name}Per${timeUnit.name}"
  override val symbol: String = s"${angleUnit.symbol}/${timeUnit.symbol}"
  override def inRadianPerSecond: Real = angleUnit.inRadian / timeUnit.inSecond
}

object AngularVelocityUnit{

  case object RadianPerSecond extends CompositeAngularVelocityUnit{
    override def angleUnit: AngleUnit = AngleUnit.Radian
    override def timeUnit: TimeUnit = TimeUnit.Second
  }

  case object DegreePerSecond extends CompositeAngularVelocityUnit{
    override def angleUnit: AngleUnit = AngleUnit.Degree
    override def timeUnit: TimeUnit = TimeUnit.Second
  }

  def apply(aUnit: AngleUnit, tUnit: TimeUnit): AngularVelocityUnit = new CompositeAngularVelocityUnit{
    override def angleUnit: AngleUnit = aUnit
    override def timeUnit: TimeUnit = tUnit
  }
}

trait AngularVelocityUnitInterpreter[A] extends AngularVelocityPostfixOps[AngularVelocity[A]]{

  def apply(unit: AngularVelocityUnit): AngularVelocity[A]

  override def `rad/s`: AngularVelocity[A] = apply(AngularVelocityUnit.RadianPerSecond)
  override def `deg/s`: AngularVelocity[A] = apply(AngularVelocityUnit.DegreePerSecond)

}
