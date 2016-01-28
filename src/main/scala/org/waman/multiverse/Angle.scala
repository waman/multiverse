package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait AnglePostfixOps[A]{
  def rad: A
  def deg: A
  def °  : A
}

trait AnglePer[A]{
  def rad(per: Per): A
  def deg(per: Per): A
  def °(per: Per): A
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

  override def rad: A = apply(AngleUnit.Radian)
  override def deg: A = apply(AngleUnit.Degree)
  override def °  : A = apply(AngleUnit.Degree)

  override def /(timeUnit: TimeUnit): AngularVelocity[A] = new AngularVelocity(value, unit / timeUnit)
}

abstract class AngleUnit(val name: String, val symbol: String, val inRadian: Real)
    extends PhysicalUnit
    with DivisibleBy[TimeUnit, AngularVelocityUnit]{

  override def /(timeUnit: TimeUnit): AngularVelocityUnit = AngularVelocityUnit(this, timeUnit)
}

object AngleUnit{
  case object Radian extends AngleUnit("Radian", "rad", r"1")
  case object Degree extends AngleUnit("Degree", "deg", Real.pi / r"180")
  case object SymbolicDegree extends AngleUnit("Degree", "°", Real.pi / r"180")
}

trait PredefinedAngleUnit{
  val rad = AngleUnit.Radian
  val deg = AngleUnit.Degree
  val °   = AngleUnit.SymbolicDegree
}

object PredefinedAngleUnit extends PredefinedAngleUnit

trait AngleUnitInterpreter[A]
    extends AnglePostfixOps[Angle[A]]
    with AnglePer[TimePostfixOps[AngularVelocity[A]]]{

  def apply(unit: AngleUnit): Angle[A]

  override def rad: Angle[A] = apply(AngleUnit.Radian)
  override def deg: Angle[A] = apply(AngleUnit.Degree)
  override def °  : Angle[A] = apply(AngleUnit.SymbolicDegree)

  protected def newAnglePer(unit: AngleUnit): TimePostfixOps[AngularVelocity[A]]

  override def rad(per: Per) = newAnglePer(AngleUnit.Radian)
  override def deg(per: Per) = newAnglePer(AngleUnit.Degree)
  override def °(per: Per)   = newAnglePer(AngleUnit.SymbolicDegree)
}