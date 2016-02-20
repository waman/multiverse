package org.waman.multiverse.energy

import org.waman.multiverse._
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait ActionPostfixOps[A]{
  import ActionUnit._

  protected def actionPostfixOps(actionUnit: ActionUnit): A

  def hbar: A = actionPostfixOps(AtomicUnitOfAction)
  def ħ   : A = hbar
}

class Action[A: Fractional](val value: A, val unit: ActionUnit)
    extends Quantity[A, ActionUnit]
    with ActionPostfixOps[A]
    with EnergyPostfixOps[MultiplicativeByTime[A]]
    with EnergyDot[TimePostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: ActionUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInJouleSecond) / real(evalUnit.unitInJouleSecond)


  override protected def actionPostfixOps(actionUnit: ActionUnit) = apply(actionUnit)

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = new MultiplicativeByTime[A] {
    override def *(timeUnit: TimeUnit) = apply(energyUnit * timeUnit)
  }

  override protected def energyDot(energyUnit: EnergyUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(energyUnit * timeUnit)
  }
}

sealed trait ActionUnit extends PhysicalUnit[ActionUnit]{

  def unitInJouleSecond: Real

  override  lazy val baseUnit = EnergyUnit.Joule * TimeUnit.Second
  override  lazy val inBaseUnitAccessor = () => unitInJouleSecond
}

object ActionUnit{

  private[ActionUnit]
  abstract class ActionUnitImpl(val symbol: String, val unitInJouleSecond: Real)
      extends ActionUnit{

    def this(symbol: String, energyUnit: EnergyUnit, timeUnit: TimeUnit) =
      this(symbol, energyUnit.unitInJoule * timeUnit.unitInSecond)
  }

  case object AtomicUnitOfAction extends ActionUnitImpl("hbar;ħ", r"1.05457168e-34") with NotExact

  // Length/Time
  private[ActionUnit]
  class ProductActionUnit(val firstUnit: EnergyUnit, val secondUnit: TimeUnit)
      extends ActionUnit
      with ProductUnit[ActionUnit, EnergyUnit, TimeUnit]{

    override lazy val unitInJouleSecond: Real = firstUnit.unitInJoule * secondUnit.unitInSecond
  }

  def apply(eUnit: EnergyUnit, tUnit: TimeUnit) = new ProductActionUnit(eUnit, tUnit)
}

trait PredefinedActionUnit extends ActionPostfixOps[ActionUnit]{

  override protected def actionPostfixOps(actionUnit: ActionUnit) = actionUnit
}

object PredefinedActionUnit extends PredefinedActionUnit

trait ActionUnitInterpreter[A]
  extends ActionPostfixOps[Action[A]]{

  def apply(unit: ActionUnit): Action[A]

  override protected def actionPostfixOps(actionUnit: ActionUnit) = apply(actionUnit)
}