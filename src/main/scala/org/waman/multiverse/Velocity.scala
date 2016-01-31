package org.waman.multiverse

import spire.math.{Real, Fractional}
import spire.implicits._

trait VelocityPostfixOps[A]{

  protected def velocityPostfixOps(velocityUnit: VelocityUnit): A

  def `m/s`  = velocityPostfixOps(VelocityUnit.MetrePerSecond)
  def `km/h` = velocityPostfixOps(VelocityUnit.KiloMetrePerHour)
}

class Velocity[A: Fractional](val value: A, val unit: VelocityUnit)
    extends Quantity[A, VelocityUnit]
    with VelocityPostfixOps[A]  // for style like "velocity.`m/s`" and "velocity `m/s`"
    with LengthPostfixOps[DivisibleBy[TimeUnit, A]]  // for style like "velocity.m/s" ( = "velocity.m./(s)")
    with LengthPer[TimePostfixOps[A]]  // for style like "velocity m/s" ( = "velocity.m(/).s")
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  // for style like "velocity (m/s)" ( = "velocity.apply(m/s)")
  def apply(evalUnit: VelocityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInMetrePerSecond) / real(evalUnit.unitInMetrePerSecond)

  override protected def velocityPostfixOps(velocityUnit: VelocityUnit) = apply(velocityUnit)

  // for style like "velocity.m/s"
  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = new DivisibleBy[TimeUnit, A]{
    override def /(timeUnit: TimeUnit): A = apply(lengthUnit / timeUnit)
  }

  // for style like "velocity m/s"
  override protected def lengthPer(lengthUnit: LengthUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit): A = apply(lengthUnit / timeUnit)
  }
}

sealed trait VelocityUnit extends PhysicalUnit{
  def unitInMetrePerSecond: Real

  override protected def baseUnit = VelocityUnit.MetrePerSecond
  override protected def inBaseUnitAccessor = () => unitInMetrePerSecond
}

class QuotientVelocityUnit(val lengthUnit: LengthUnit, val timeUnit: TimeUnit)
    extends VelocityUnit with QuotientUnit[LengthUnit, TimeUnit]{

  override def numeratorUnit: LengthUnit = lengthUnit
  override def denominatorUnit: TimeUnit = timeUnit

  override def unitInMetrePerSecond: Real = lengthUnit.unitInMetre / timeUnit.unitInSecond
}

object VelocityUnit{

  case object MetrePerSecond extends QuotientVelocityUnit(LengthUnit.Metre, TimeUnit.Second)

  case object KiloMetrePerHour extends QuotientVelocityUnit(LengthUnit.KiloMetre, TimeUnit.Hour)

  def apply(lUnit: LengthUnit, tUnit: TimeUnit): VelocityUnit =
    new QuotientVelocityUnit(lUnit, tUnit)
}

trait PredefinedVelocityUnit{
  val `m/s`  = VelocityUnit.MetrePerSecond
  val `km/h` = VelocityUnit.KiloMetrePerHour
}

object PredefinedVelocityUnit extends PredefinedVelocityUnit

trait VelocityUnitInterpreter[A]
    extends VelocityPostfixOps[Velocity[A]]
    with UnitConverter[A]{

  def apply(velocityUnit: VelocityUnit): Velocity[A]

  override protected def velocityPostfixOps(velocityUnit: VelocityUnit) =
    apply(velocityUnit)
}
