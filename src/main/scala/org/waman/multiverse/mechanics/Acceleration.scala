package org.waman.multiverse.mechanics

import org.waman.multiverse._
import org.waman.multiverse.metric.{LengthPer, LengthPostfixOps, LengthUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeSquaredPostfixOps, TimeSquaredUnit, TimeUnit}
import spire.implicits._
import spire.math.Fractional

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

trait AccelerationFactory[A]
  extends AccelerationPostfixOps[Acceleration[A]]
    with UnitConverter[A]{

  def apply(accelerationUnit: AccelerationUnit): Acceleration[A]

  override protected def accelerationPostfixOps(accelerationUnit: AccelerationUnit) =
    apply(accelerationUnit)
}

