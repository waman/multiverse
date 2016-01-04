package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait AnglePostfixOps[A]{
  def deg: A
  def rad: A
}

trait AnglePer[A]{
  def deg(per: Per): A
  def rad(per: Per): A
}

class Angle[A: Fractional](val value: A, val unit: AngleUnit)
    extends ValueWithUnit[A, AngleUnit]
    with AnglePostfixOps[A]
    with DivisibleByTime[AngularVelocity[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AngleUnit): A =
    if(evalUnit == unit) value
    else value * real(unit.inRadian) / real(evalUnit.inRadian)

  override def rad: A = apply(AngleUnit.Radian)
  override def deg: A = apply(AngleUnit.Degree)

  override def /(timeUnit: TimeUnit): AngularVelocity[A] = new AngularVelocity(value, unit / timeUnit)
}

abstract class AngleUnit(val name: String, val symbol: String, val inRadian: Real)
    extends PhysicalUnit
    with DivisibleByTime[AngularVelocityUnit]{

  override def /(timeUnit: TimeUnit): AngularVelocityUnit = AngularVelocityUnit(this, timeUnit)
}

object AngleUnit{
  case object Radian extends AngleUnit("Radian", "rad", r"1")
  case object Degree extends AngleUnit("Degree", "deg", Real.pi / r"180")
}

trait AngleUnitInterpreter[A]
    extends AnglePostfixOps[Angle[A]]
    with AnglePer[TimePostfixOps[AngularVelocity[A]]]{

  def apply(unit: AngleUnit): Angle[A]

  override def rad: Angle[A] = apply(AngleUnit.Radian)
  override def deg: Angle[A] = apply(AngleUnit.Degree)

  protected def newAnglePer(unit: AngleUnit): TimePostfixOps[AngularVelocity[A]]

  override def rad(per: Per) = newAnglePer(AngleUnit.Radian)
  override def deg(per: Per) = newAnglePer(AngleUnit.Degree)
}