package org.waman.multiverse.angle

import org.waman.multiverse.MultiverseUtil.twoPi
import org.waman.multiverse._
import org.waman.multiverse.time.{Frequency, FrequencyUnit, TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math._

trait AngularVelocityPostfixOps[A]{
  import AngularVelocityUnit._

  protected def angularVelocityPostfixOps(angularVelocityUnit: AngularVelocityUnit): A

  def rpm: A = angularVelocityPostfixOps(RevolutionPerMinute)
  def cps: A = angularVelocityPostfixOps(CyclePerSecond)
}

class AngularVelocity[A: Fractional](val value: A, val unit: AngularVelocityUnit)
    extends Quantity[A, AngularVelocityUnit]
    with AngularVelocityPostfixOps[A]
    with AnglePostfixOps[DivisibleByTime[A]]
    with AnglePer[TimePostfixOps[A]]
    with UnitConverter[A]{

  protected override lazy val algebra: Fractional[A] = implicitly[Fractional[A]]

  def apply(evalUnit: AngularVelocityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInRadianPerSecond) / real(evalUnit.unitInRadianPerSecond)

  override protected def angularVelocityPostfixOps(angularVelocityUnit: AngularVelocityUnit) =
    apply(angularVelocityUnit)

  override protected def anglePostfixOps(angleUnit: AngleUnit) = new DivisibleByTime[A]{
    override def /(timeUnit: TimeUnit): A = apply(angleUnit / timeUnit)
  }

  override protected def anglePer(angleUnit: AngleUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(angleUnit / timeUnit)
  }

  def toFrequency: Frequency[A] = new Frequency(
    div(rad(UnitSystem./).s, twoPi),
    FrequencyUnit.Heltz)
}

sealed abstract class AngularVelocityUnit extends PhysicalUnit[AngularVelocityUnit]{

  def unitInRadianPerSecond: Real

  override lazy val baseUnit = AngleUnit.Radian / TimeUnit.Second
  override lazy val inBaseUnitAccessor = () => unitInRadianPerSecond
}

object AngularVelocityUnit{

  // Custom
  private[AngularVelocityUnit]
  class AngularVelocityUnitImpl(val symbol: String, val unitInRadianPerSecond: Real)
    extends AngularVelocityUnit

  case object CyclePerSecond      extends AngularVelocityUnitImpl("cps", twoPi)
  case object RevolutionPerMinute extends AngularVelocityUnitImpl("rpm", twoPi / r"60")

  // Quotient
  private class QuotientAngularVelocityUnit(val numeratorUnit: AngleUnit, val denominatorUnit: TimeUnit)
    extends AngularVelocityUnit with QuotientUnit[AngularVelocityUnit, AngleUnit, TimeUnit]{

    override lazy val unitInRadianPerSecond: Real =
      numeratorUnit.unitInRadian / denominatorUnit.unitInSecond
  }

  def apply(aUnit: AngleUnit, tUnit: TimeUnit): AngularVelocityUnit =
    new QuotientAngularVelocityUnit(aUnit, tUnit)
}

trait PredefinedAngularVelocityUnit extends AngularVelocityPostfixOps[AngularVelocityUnit]{
  override protected def angularVelocityPostfixOps(avUnit: AngularVelocityUnit) = avUnit
}

object PredefinedAngularVelocityUnit extends PredefinedAngularVelocityUnit

trait AngularVelocityUnitInterpreter[A]
    extends AngularVelocityPostfixOps[AngularVelocity[A]]{

  def apply(unit: AngularVelocityUnit): AngularVelocity[A]

  override protected def angularVelocityPostfixOps(angularVelocityUnit: AngularVelocityUnit) =
    apply(angularVelocityUnit)
}
