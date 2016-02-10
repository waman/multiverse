package org.waman.multiverse

import spire.math.{Real, Fractional}
import spire.implicits._

trait AccelerationPostfixOps[A]{

  protected def accelerationPostfixOps(accelerationUnit: AccelerationUnit): A

  def g0  = accelerationPostfixOps(AccelerationUnit.StandardGravity)
  def Gal = accelerationPostfixOps(AccelerationUnit.Galileo)

  def ips2 = accelerationPostfixOps(AccelerationUnit.InchPerSecondSquared)
  def fps2 = accelerationPostfixOps(AccelerationUnit.FootPerSecondSquared)
  def mps2 = accelerationPostfixOps(AccelerationUnit.MilePerSecondSquared)
}

class Acceleration[A: Fractional](val value: A, val unit: AccelerationUnit)
  extends Quantity[A, AccelerationUnit]
    with AccelerationPostfixOps[A]
    with LengthPostfixOps[DivisibleByTimeSquared[A]]
    with VelocityPostfixOps[DivisibleByTime[A]]
    with LengthPer[TimeSquaredPostfixOps[A]]
    with VelocityPer[TimePostfixOps[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AccelerationUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInMetrePerSecondSquared) / real(evalUnit.unitInMetrePerSecondSquared)

  override protected def accelerationPostfixOps(accelerationUnit: AccelerationUnit) = apply(accelerationUnit)

  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = new DivisibleByTimeSquared[A]{
    override def /(timeSquaredUnit: TimeSquaredUnit): A = apply(lengthUnit / timeSquaredUnit)
  }

  override protected def velocityPostfixOps(velocityUnit: VelocityUnit) = new DivisibleByTime[A]{
    override def /(timeUnit: TimeUnit): A = apply(velocityUnit / timeUnit)
  }

  override protected def lengthPer(lengthUnit: LengthUnit) = new TimeSquaredPostfixOps[A]{
    override protected def timeSquaredPostfixOps(timeSquaredUnit: TimeSquaredUnit): A =
      apply(lengthUnit / timeSquaredUnit)
  }

  override protected def velocityPer(velocityUnit: VelocityUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit): A = apply(velocityUnit / timeUnit)
  }
}

sealed trait AccelerationUnit extends PhysicalUnit{
  def unitInMetrePerSecondSquared: Real

  override protected lazy val baseUnit = LengthUnit.Metre / TimeSquaredUnit.SecondSquared
  override protected lazy val inBaseUnitAccessor = () => unitInMetrePerSecondSquared
}

object AccelerationUnit{

  private[AccelerationUnit] abstract class AccelerationUnitImpl(val symbol: String, val unitInMetrePerSecondSquared: Real)
    extends AccelerationUnit{

    def this(symbol: String, lengthUnit: LengthUnit) =
      this(symbol, lengthUnit.unitInMetre / TimeSquaredUnit.SecondSquared.unitInSecondSquared)
  }

  case object StandardGravity extends AccelerationUnitImpl("g0", r"9.80665")
  case object Galileo extends AccelerationUnitImpl("Gal", LengthUnit.CentiMetre)

  case object InchPerSecondSquared extends AccelerationUnitImpl("ips2", LengthUnit.Inch)
  case object FootPerSecondSquared extends AccelerationUnitImpl("fps2", LengthUnit.Foot)
  case object MilePerSecondSquared extends AccelerationUnitImpl("mps2", LengthUnit.Mile)

  // Length/Time2
  private[AccelerationUnit]
  class LengthOverTimeSquared(val numeratorUnit: LengthUnit, val denominatorUnit: TimeSquaredUnit)
    extends AccelerationUnit with QuotientUnit[LengthUnit, TimeSquaredUnit]{

    override lazy val unitInMetrePerSecondSquared: Real =
      numeratorUnit.unitInMetre / denominatorUnit.unitInSecondSquared
  }

  def apply(lUnit: LengthUnit, t2Unit: TimeSquaredUnit): AccelerationUnit =
    new LengthOverTimeSquared(lUnit, t2Unit)

  // Velocity/Time
  private[AccelerationUnit]
  class VelocityOverTime(val numeratorUnit: VelocityUnit, val denominatorUnit: TimeUnit)
    extends AccelerationUnit with QuotientUnit[VelocityUnit, TimeUnit]{

    override lazy val unitInMetrePerSecondSquared: Real =
      numeratorUnit.unitInMetrePerSecond / denominatorUnit.unitInSecond
  }

  def apply(vUnit: VelocityUnit, tUnit: TimeUnit): AccelerationUnit =
    new VelocityOverTime(vUnit, tUnit)
}

trait PredefinedAccelerationUnit extends AccelerationPostfixOps[AccelerationUnit]{
  override protected def accelerationPostfixOps(accelerationUnit: AccelerationUnit) = accelerationUnit
}

object PredefinedAccelerationUnit extends PredefinedAccelerationUnit

trait AccelerationUnitInterpreter[A]
  extends AccelerationPostfixOps[Acceleration[A]]
    with UnitConverter[A]{

  def apply(accelerationUnit: AccelerationUnit): Acceleration[A]

  override protected def accelerationPostfixOps(accelerationUnit: AccelerationUnit) =
    apply(accelerationUnit)
}

