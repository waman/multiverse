package org.waman.multiverse.time

import org.waman.multiverse.{Quantity, UnitConverter}
import spire.implicits._
import spire.math.Fractional

class TimeSquared[A: Fractional](val value: A, val unit: TimeSquaredUnit)
  extends Quantity[A, TimeSquaredUnit]
    with TimeSquaredPostfixOps[A]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: TimeSquaredUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitValueInSIUnit) / real(evalUnit.unitValueInSIUnit)

  override protected def timeSquaredPostfixOps(timeSquaredUnit: TimeSquaredUnit) = apply(timeSquaredUnit)
}

trait TimeSquaredFactory[A] extends TimeSquaredPostfixOps[TimeSquared[A]]{

  def apply(unit: TimeSquaredUnit): TimeSquared[A]

  override protected def timeSquaredPostfixOps(timeSquaredUnit: TimeSquaredUnit) = apply(timeSquaredUnit)
}