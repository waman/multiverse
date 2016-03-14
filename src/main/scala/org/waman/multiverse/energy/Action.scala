package org.waman.multiverse.energy

import org.waman.multiverse._
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.Fractional

class Action[A: Fractional](val value: A, val unit: ActionUnit)
    extends Quantity[A, ActionUnit]
    with ActionPostfixOps[A]
    with EnergyPostfixOps[MultiplicativeByTimeUnit[A]]
    with EnergyDot[TimePostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ActionUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInJouleSecond) / real(evalUnit.unitInJouleSecond)


  override protected def actionPostfixOps(actionUnit: ActionUnit) = apply(actionUnit)

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = new MultiplicativeByTimeUnit[A] {
    override def *(timeUnit: TimeUnit) = apply(energyUnit * timeUnit)
  }

  override protected def energyDot(energyUnit: EnergyUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(energyUnit * timeUnit)
  }
}

trait ActionFactory[A]
  extends ActionPostfixOps[Action[A]]{

  def apply(unit: ActionUnit): Action[A]

  override protected def actionPostfixOps(actionUnit: ActionUnit) = apply(actionUnit)
}