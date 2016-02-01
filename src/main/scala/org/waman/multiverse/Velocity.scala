package org.waman.multiverse

import spire.math.{Real, Fractional}
import spire.implicits._

trait VelocityPostfixOps[A]{

  protected def velocityPostfixOps(velocityUnit: VelocityUnit): A

  def c = velocityPostfixOps(VelocityUnit.SpeedOfLight)
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

  override protected lazy val baseUnit = LengthUnit.Metre / TimeUnit.Second
  override protected lazy val inBaseUnitAccessor = () => unitInMetrePerSecond
}

object VelocityUnit{

  private[VelocityUnit] abstract class VelocityUnitImpl(val symbol: String, val unitInMetrePerSecond: Real)
    extends VelocityUnit

  case object SpeedOfLight extends VelocityUnitImpl("c", r"299792458")


  private[VelocityUnit] class QuotientVelocityUnit(val numeratorUnit: LengthUnit, val denominatorUnit: TimeUnit)
    extends VelocityUnit with QuotientUnit[LengthUnit, TimeUnit]{

    override lazy val unitInMetrePerSecond: Real = numeratorUnit.unitInMetre / denominatorUnit.unitInSecond
  }

  def apply(lUnit: LengthUnit, tUnit: TimeUnit): VelocityUnit =
    new QuotientVelocityUnit(lUnit, tUnit)
}

trait PredefinedVelocityUnit{
  val c = VelocityUnit.SpeedOfLight
}

object PredefinedVelocityUnit extends PredefinedVelocityUnit

trait VelocityUnitInterpreter[A]
    extends VelocityPostfixOps[Velocity[A]]
    with UnitConverter[A]{

  def apply(velocityUnit: VelocityUnit): Velocity[A]

  override protected def velocityPostfixOps(velocityUnit: VelocityUnit) =
    apply(velocityUnit)
}
