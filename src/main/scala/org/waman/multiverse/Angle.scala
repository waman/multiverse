package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait AnglePostfixOps[A]{
  def rad: A

  def deg : A
  def °   : A
  def grad: A
}

trait AnglePer[A]{
  def rad(per: Per): A

  def deg(per: Per) : A
  def °(per: Per)   : A
  def grad(per: Per): A
}

class Angle[A: Fractional](val value: A, val unit: AngleUnit)
    extends Quantity[A, AngleUnit]
    with AnglePostfixOps[A]
    with DivisibleBy[TimeUnit, AngularVelocity[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AngleUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.inRadian) / real(evalUnit.inRadian)

  override def rad  = apply(AngleUnit.Radian)

  override def deg  = apply(AngleUnit.Degree)
  override def °    = apply(AngleUnit.Degree)
  override def grad = apply(AngleUnit.Gradian)

  override def /(timeUnit: TimeUnit): AngularVelocity[A] = new AngularVelocity(value, unit / timeUnit)
}

abstract class AngleUnit(val symbol: String, val inRadian: Real)
    extends PhysicalUnit
    with DivisibleBy[TimeUnit, AngularVelocityUnit]{

  override protected def baseUnit = AngleUnit.Radian
  override protected def inBaseUnitAccessor = () => inRadian

  override def /(timeUnit: TimeUnit): AngularVelocityUnit = AngularVelocityUnit(this, timeUnit)
}

object AngleUnit{
  case object Radian extends AngleUnit("rad", r"1")
  case object Degree extends AngleUnit("deg", Real.pi / r"180")
  case object SymbolicDegree extends AngleUnit("°", Real.pi / r"180")
  case object Gradian extends AngleUnit("grad", Real.pi / r"200")
}

trait PredefinedAngleUnit{
  val rad  = AngleUnit.Radian
  val deg  = AngleUnit.Degree
  val °    = AngleUnit.SymbolicDegree
  val grad = AngleUnit.Gradian
}

object PredefinedAngleUnit extends PredefinedAngleUnit

trait AngleUnitInterpreter[A]
    extends AnglePostfixOps[Angle[A]]
    with AnglePer[TimePostfixOps[AngularVelocity[A]]]{

  def apply(unit: AngleUnit): Angle[A]

  override def rad  = apply(AngleUnit.Radian)
  override def deg  = apply(AngleUnit.Degree)
  override def °    = apply(AngleUnit.SymbolicDegree)
  override def grad = apply(AngleUnit.Gradian)

  protected def newAnglePer(unit: AngleUnit): TimePostfixOps[AngularVelocity[A]] =
    new TimePostfixOps[AngularVelocity[A]] {
      override def ns     = apply(unit / TimeUnit.NanoSecond)
      override def μs     = apply(unit / TimeUnit.MicroSecond)
      override def ms     = apply(unit / TimeUnit.MilliSecond)
      override def s      = apply(unit / TimeUnit.Second)
      override def minute = apply(unit / TimeUnit.Minute)
      override def h      = apply(unit / TimeUnit.Hour)
      override def d      = apply(unit / TimeUnit.Day)
    }

  def apply(unit: AngularVelocityUnit): AngularVelocity[A]

  override def rad(per: Per)  = newAnglePer(AngleUnit.Radian)
  override def deg(per: Per)  = newAnglePer(AngleUnit.Degree)
  override def °(per: Per)    = newAnglePer(AngleUnit.SymbolicDegree)
  override def grad(per: Per) = newAnglePer(AngleUnit.Gradian)
}