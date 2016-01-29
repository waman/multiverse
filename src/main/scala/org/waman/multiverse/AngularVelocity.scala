package org.waman.multiverse

import spire.math._
import spire.implicits._

trait AngularVelocityPostfixOps[A]{
  def `rad/s`: A
  def `deg/s`: A
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
    else value * real(unit.inRadianPerSecond) / real(evalUnit.inRadianPerSecond)

  override def `rad/s` = apply(AngularVelocityUnit.RadianPerSecond)
  override def `deg/s` = apply(AngularVelocityUnit.DegreePerSecond)

  private def callAngle(angleUnit: AngleUnit) = new DivisibleBy[TimeUnit, A]{
    override def /(timeUnit: TimeUnit): A = apply(angleUnit / timeUnit)
  }

  override def rad  = callAngle(AngleUnit.Radian)
  override def deg  = callAngle(AngleUnit.Degree)
  override def °    = callAngle(AngleUnit.SymbolicDegree)
  override def grad = callAngle(AngleUnit.Gradian)

  private def callAnglePer(angleUnit: AngleUnit) = new TimePostfixOps[A]{
    override def ns     = apply(angleUnit / TimeUnit.NanoSecond)
    override def μs     = apply(angleUnit / TimeUnit.MicroSecond)
    override def ms     = apply(angleUnit / TimeUnit.MilliSecond)
    override def s      = apply(angleUnit / TimeUnit.Second)
    override def d      = apply(angleUnit / TimeUnit.Day)
    override def minute = apply(angleUnit / TimeUnit.Minute)
    override def h      = apply(angleUnit / TimeUnit.Hour)
  }

  override def rad(per: Per)  = callAnglePer(AngleUnit.Radian)
  override def deg(per: Per)  = callAnglePer(AngleUnit.Degree)
  override def °(per: Per)    = callAnglePer(AngleUnit.SymbolicDegree)
  override def grad(per: Per) = callAnglePer(AngleUnit.Gradian)
}

trait AngularVelocityUnit extends PhysicalUnit{
  def inRadianPerSecond: Real
}

sealed class QuotientAngularVelocityUnit(val angleUnit: AngleUnit, val timeUnit: TimeUnit)
    extends AngularVelocityUnit with QuotientUnit[AngleUnit, TimeUnit]{

  override def numeratorUnit: AngleUnit = angleUnit
  override def denominatorUnit: TimeUnit = timeUnit

  override def inRadianPerSecond: Real = angleUnit.inRadian / timeUnit.inSecond
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

  override def `rad/s` = apply(AngularVelocityUnit.RadianPerSecond)
  override def `deg/s` = apply(AngularVelocityUnit.DegreePerSecond)

}
