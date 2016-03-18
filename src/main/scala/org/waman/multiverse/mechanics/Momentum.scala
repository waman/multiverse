package org.waman.multiverse.mechanics

import org.waman.multiverse._
import org.waman.multiverse.metric.{LengthPostfixOps, LengthUnit, MultiplicativeByLengthUnit}
import org.waman.multiverse.time.{MultiplicativeByTimeUnit, TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

class Momentum[A: Fractional](val value: A, val unit: MomentumUnit)
    extends Quantity[A, MomentumUnit]
    with ForcePostfixOps[MultiplicativeByTimeUnit[A]]
    with ForceDot[TimePostfixOps[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: MomentumUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInNewtonSecond) / real(evalUnit.unitInNewtonSecond)

  override protected def forcePostfixOps(forceUnit: ForceUnit) = new MultiplicativeByTimeUnit[A]{
    override def *(timeUnit: TimeUnit): A = apply(forceUnit * timeUnit)
  }

  override protected def forceDot(forceUnit: ForceUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(lengthUnit: TimeUnit) = apply(forceUnit * lengthUnit)
  }
}

trait MomentumFactory[A]
  extends UnitConverter[A]{

  def apply(unit: MomentumUnit): Momentum[A]
}
