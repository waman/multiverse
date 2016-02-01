package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait AnglePostfixOps[A]{

  protected def anglePostfixOps(angleUnit: AngleUnit): A

  def rad: A = anglePostfixOps(AngleUnit.Radian)
  def deg: A = anglePostfixOps(AngleUnit.Degree)
  def °  : A = anglePostfixOps(AngleUnit.SymbolicDegree)
  def gon: A = anglePostfixOps(AngleUnit.Gradian)
  def tr : A = anglePostfixOps(AngleUnit.Turn)
}

trait AnglePer[A]{

  protected def anglePer(angleUnit: AngleUnit): A

  def rad(per: Per): A = anglePer(AngleUnit.Radian)
  def deg(per: Per): A = anglePer(AngleUnit.Degree)
  def °  (per: Per): A = anglePer(AngleUnit.SymbolicDegree)
  def gon(per: Per): A = anglePer(AngleUnit.Gradian)
  def tr (per: Per): A = anglePer(AngleUnit.Turn)
}

class Angle[A: Fractional](val value: A, val unit: AngleUnit)
    extends Quantity[A, AngleUnit]
    with AnglePostfixOps[A]
    with DivisibleBy[TimeUnit, AngularVelocity[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AngleUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInRadian) / real(evalUnit.unitInRadian)

  override protected def anglePostfixOps(angleUnit: AngleUnit) = apply(angleUnit)

  override def /(timeUnit: TimeUnit): AngularVelocity[A] = new AngularVelocity(value, unit / timeUnit)
}

abstract class AngleUnit(val symbol: String, val unitInRadian: Real)
    extends PhysicalUnit
    with DivisibleBy[TimeUnit, AngularVelocityUnit]{

  override protected val baseUnit = AngleUnit.Radian
  override protected val inBaseUnitAccessor = () => unitInRadian

  override def /(timeUnit: TimeUnit): AngularVelocityUnit = AngularVelocityUnit(this, timeUnit)
}

object AngleUnit{
  case object Radian         extends AngleUnit("rad", r"1")
  case object Degree         extends AngleUnit("deg", Real.pi / r"180")
  case object SymbolicDegree extends AngleUnit("°"  , Real.pi / r"180")
  case object Gradian        extends AngleUnit("gon", Real.pi / r"200")
  case object Turn           extends AngleUnit("tr" , r"2" * Real.pi)
}

trait PredefinedAngleUnit{
  val rad = AngleUnit.Radian
  val deg = AngleUnit.Degree
  val °   = AngleUnit.SymbolicDegree
  val gon = AngleUnit.Gradian
  val tr  = AngleUnit.Turn
}

object PredefinedAngleUnit extends PredefinedAngleUnit

trait AngleUnitInterpreter[A]
    extends AnglePostfixOps[Angle[A]]
    with AnglePer[TimePostfixOps[AngularVelocity[A]]]{

  def apply(unit: AngleUnit): Angle[A]

  override protected def anglePostfixOps(angleUnit: AngleUnit) = apply(angleUnit)

  protected def newAnglePer(unit: AngleUnit): TimePostfixOps[AngularVelocity[A]] =
    new TimePostfixOps[AngularVelocity[A]] {
      override protected def timePostfixOps(timeUnit: TimeUnit) = apply(unit / timeUnit)
    }

  def apply(unit: AngularVelocityUnit): AngularVelocity[A]

  override protected def anglePer(angleUnit: AngleUnit) = newAnglePer(angleUnit)
}