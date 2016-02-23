package org.waman.multiverse.mechanics

import org.waman.multiverse._
import org.waman.multiverse.metric.{LengthPer, LengthPostfixOps, LengthUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeSquaredPostfixOps, TimeSquaredUnit, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait AccelerationPostfixOps[A]{
  import AccelerationUnit._

  protected def accelerationPostfixOps(accelerationUnit: AccelerationUnit): A

  def g0  = accelerationPostfixOps(StandardGravity)
  def Gal = accelerationPostfixOps(Galileo)

  def ips2 = accelerationPostfixOps(InchPerSecondSquared)
  def fps2 = accelerationPostfixOps(FootPerSecondSquared)
  def mps2 = accelerationPostfixOps(MilePerSecondSquared)
}

class Acceleration[A: Fractional](val value: A, val unit: AccelerationUnit)
  extends Quantity[A, AccelerationUnit]
    with AccelerationPostfixOps[A]
    with LengthPostfixOps[DivisibleByTimeSquaredUnit[A]]
    with VelocityPostfixOps[DivisibleByTimeUnit[A]]
    with LengthPer[TimeSquaredPostfixOps[A]]
    with VelocityPer[TimePostfixOps[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AccelerationUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInMetrePerSecondSquared) / real(evalUnit.unitInMetrePerSecondSquared)

  override protected def accelerationPostfixOps(accelerationUnit: AccelerationUnit) = apply(accelerationUnit)

  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = new DivisibleByTimeSquaredUnit[A]{
    override def /(timeSquaredUnit: TimeSquaredUnit): A = apply(lengthUnit / timeSquaredUnit)
  }

  override protected def velocityPostfixOps(velocityUnit: VelocityUnit) = new DivisibleByTimeUnit[A]{
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

sealed abstract class AccelerationUnit extends PhysicalUnit[AccelerationUnit]{
  def unitInMetrePerSecondSquared: Real

  override def baseUnit = LengthUnit.Metre / TimeSquaredUnit.SecondSquared
  override def valueInBaseUnit = unitInMetrePerSecondSquared
}

object AccelerationUnit{

  // Custom
  private[AccelerationUnit]
  class AccelerationUnitImpl(val symbol: String, val unitInMetrePerSecondSquared: Real)
    extends AccelerationUnit{

    def this(symbol: String, lengthUnit: LengthUnit) =
      this(symbol, lengthUnit.unitInMetre / TimeSquaredUnit.SecondSquared.unitInSecondSquared)
  }

  case object StandardGravity extends AccelerationUnitImpl("g0", r"9.80665")
  case object Galileo extends AccelerationUnitImpl("Gal", LengthUnit.CentiMetre)

  case object InchPerSecondSquared extends AccelerationUnitImpl("ips2", LengthUnit.Inch)
  case object FootPerSecondSquared extends AccelerationUnitImpl("fps2", LengthUnit.Foot)
  case object MilePerSecondSquared extends AccelerationUnitImpl("mps2", LengthUnit.Mile)

  // Quotient (Length/Time2)
  private[AccelerationUnit]
  class LengthOverTimeSquared(val numeratorUnit: LengthUnit, val denominatorUnit: TimeSquaredUnit)
    extends AccelerationUnit with QuotientUnit[AccelerationUnit, LengthUnit, TimeSquaredUnit]{

    override lazy val unitInMetrePerSecondSquared: Real =
      numeratorUnit.unitInMetre / denominatorUnit.unitInSecondSquared
  }

  def apply(lUnit: LengthUnit, t2Unit: TimeSquaredUnit): AccelerationUnit =
    new LengthOverTimeSquared(lUnit, t2Unit)

  // Quotient (Velocity/Time)
  private[AccelerationUnit]
  class VelocityOverTime(val numeratorUnit: VelocityUnit, val denominatorUnit: TimeUnit)
    extends AccelerationUnit with QuotientUnit[AccelerationUnit, VelocityUnit, TimeUnit]{

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

