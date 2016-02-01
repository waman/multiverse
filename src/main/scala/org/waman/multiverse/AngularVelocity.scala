package org.waman.multiverse

import spire.math._
import spire.implicits._

trait AngularVelocityPostfixOps[A]{

  protected def angularVelocityPostfixOps(angularVelocityUnit: AngularVelocityUnit): A

  def rpm: A = angularVelocityPostfixOps(AngularVelocityUnit.RevolutionPerMinute)
  def cps: A = angularVelocityPostfixOps(AngularVelocityUnit.CyclePerSecond)
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

sealed abstract class AngularVelocityUnit extends PhysicalUnit{
  def unitInRadianPerSecond: Real

  override protected lazy val baseUnit = AngleUnit.Radian / TimeUnit.Second
  override protected lazy val inBaseUnitAccessor = () => unitInRadianPerSecond
}

object AngularVelocityUnit{

  private[AngularVelocityUnit] class AngularVelocityUnitImpl(val symbol: String, val unitInRadianPerSecond: Real)
    extends AngularVelocityUnit

  case object RevolutionPerMinute extends AngularVelocityUnitImpl("rpm", r"2" * Real.pi / r"60")
  case object CyclePerSecond      extends AngularVelocityUnitImpl("cps", r"2" * Real.pi)


  private[AngularVelocityUnit]
  class QuotientAngularVelocityUnit(val numeratorUnit: AngleUnit, val denominatorUnit: TimeUnit)
    extends AngularVelocityUnit with QuotientUnit[AngleUnit, TimeUnit]{

    override lazy val unitInRadianPerSecond: Real = numeratorUnit.unitInRadian / denominatorUnit.unitInSecond
  }

  def apply(aUnit: AngleUnit, tUnit: TimeUnit): AngularVelocityUnit =
    new QuotientAngularVelocityUnit(aUnit, tUnit)
}

trait PredefinedAngularVelocityUnit{
  val rpm = AngularVelocityUnit.RevolutionPerMinute
  val cps = AngularVelocityUnit.CyclePerSecond
}

object PredefinedAngularVelocityUnit extends PredefinedAngularVelocityUnit

trait AngularVelocityUnitInterpreter[A] extends AngularVelocityPostfixOps[AngularVelocity[A]]{

  def apply(unit: AngularVelocityUnit): AngularVelocity[A]

  override protected def angularVelocityPostfixOps(angularVelocityUnit: AngularVelocityUnit) =
    apply(angularVelocityUnit)
}
