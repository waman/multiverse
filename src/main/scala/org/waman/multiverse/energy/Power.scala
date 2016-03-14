package org.waman.multiverse.energy

import org.waman.multiverse._
import spire.implicits._
import spire.math.Fractional

class Power[A: Fractional](val value: A, val unit: PowerUnit)
  extends Quantity[A, PowerUnit]
    with PowerPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: PowerUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInWatt) / real(evalUnit.unitInWatt)


  override protected def powerPostfixOps(powerUnit: PowerUnit) = apply(powerUnit)
}

trait PowerFactory[A]
  extends PowerPostfixOps[Power[A]]{

  def apply(unit: PowerUnit): Power[A]

  override protected def powerPostfixOps(powerUnit: PowerUnit) = apply(powerUnit)
}