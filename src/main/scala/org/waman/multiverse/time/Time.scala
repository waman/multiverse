package org.waman.multiverse.time

import org.waman.multiverse.{Quantity, UnitConverter}
import spire.implicits._
import spire.math.Fractional

class Time[A: Fractional](val value: A, val unit: TimeUnit)
    extends Quantity[A, TimeUnit]
    with TimePostfixOps[A]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: TimeUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSecond) / real(evalUnit.unitInSecond)

  override protected def timePostfixOps(timeUnit: TimeUnit) = apply(timeUnit)
}

trait TimeFactory[A] extends TimePostfixOps[Time[A]]{

  def apply(unit: TimeUnit): Time[A]

  override protected def timePostfixOps(timeUnit: TimeUnit) = apply(timeUnit)
}