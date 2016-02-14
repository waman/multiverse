package org.waman.multiverse

import org.waman.multiverse.MultiverseUtil.twoPi
import spire.implicits._
import spire.math.{Fractional, Real}

trait AnglePostfixOps[A]{
  import AngleUnit._

  protected def anglePostfixOps(angleUnit: AngleUnit): A

  def rad: A = anglePostfixOps(Radian)
  def deg: A = anglePostfixOps(Degree)
  def °  : A = anglePostfixOps(Degree)
  def gon: A = anglePostfixOps(Gradian)
  def ᵍ  : A = anglePostfixOps(Gradian)
  def tr : A = anglePostfixOps(Turn)
}

trait AnglePer[A]{
  import AngleUnit._

  protected def anglePer(angleUnit: AngleUnit): A

  def rad(per: Per): A = anglePer(Radian)
  def deg(per: Per): A = anglePer(Degree)
  def °  (per: Per): A = anglePer(Degree)
  def gon(per: Per): A = anglePer(Gradian)
  def ᵍ  (per: Per): A = anglePer(Gradian)
  def tr (per: Per): A = anglePer(Turn)
}

class Angle[A: Fractional](val value: A, val unit: AngleUnit)
    extends Quantity[A, AngleUnit]
    with AnglePostfixOps[A]
    with DivisibleByTime[AngularVelocity[A]]
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
    with DivisibleByTime[AngularVelocityUnit]{

  def this(symbol: String, factor: Real, angleUnit: AngleUnit) =
    this(symbol, factor * angleUnit.unitInRadian)

  override protected val baseUnit = AngleUnit.Radian
  override protected val inBaseUnitAccessor = () => unitInRadian

  override def /(timeUnit: TimeUnit): AngularVelocityUnit =
    AngularVelocityUnit(this, timeUnit)
}

object AngleUnit{

  case object Radian  extends AngleUnit("rad", r"1")
  case object Degree  extends AngleUnit("deg;°", twoPi / r"360")
  case object Gradian extends AngleUnit("gon;ᵍ", twoPi / r"400")
  case object Turn    extends AngleUnit("tr" , twoPi)
}

trait PredefinedAngleUnit extends AnglePostfixOps[AngleUnit]{
  override protected def anglePostfixOps(angleUnit: AngleUnit) = angleUnit
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