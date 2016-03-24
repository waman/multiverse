package org.waman.multiverse.energy

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.time.TimeUnit

sealed trait ActionUnit extends PhysicalUnit[ActionUnit]{

  def unitInJouleSecond: Real

  override def baseUnit = EnergyUnit.Joule * TimeUnit.Second
  override def valueInBaseUnit = unitInJouleSecond
}

object ActionUnit extends ConstantsDefined[ActionUnit]{

  // intrinsic
  private[ActionUnit]
  class IntrinsicActionUnit(name: String, val symbols: Seq[String], val unitInJouleSecond: Real)
      extends ActionUnit{

    def this(name: String, symbols: Seq[String], unit: ActionUnit) =
      this(name, symbols, unit.unitInJouleSecond)

    def this(name: String, symbols: Seq[String], factor: Real, unit: ActionUnit) =
      this(name, symbols, factor * unit.unitInJouleSecond)
  }


  case object ReducedPlanckConstant extends IntrinsicActionUnit("ReducedPlanckConstant", Seq("ħ", "hbar"), r"1.05457172647e-34") with NotExact

  override lazy val values = Seq(ReducedPlanckConstant)

  // EnergyUnit * TimeUnit -> Action
  private[ActionUnit]
  class ProductEnergyDotTimeUnit(val firstUnit: EnergyUnit, val secondUnit: TimeUnit)
      extends ActionUnit with ProductUnit[ActionUnit, EnergyUnit, TimeUnit]{

    override lazy val unitInJouleSecond: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
  }

  def apply(unit1: EnergyUnit, unit2: TimeUnit): ActionUnit =
    new ProductEnergyDotTimeUnit(unit1, unit2)
}

trait MultiplicativeByActionUnit[R]{
  def *(unit: ActionUnit): R
}

trait DivisibleByActionUnit[R]{
  def /(unit: ActionUnit): R
}

trait ActionPostfixOps[A]{
  import ActionUnit._

  protected def actionPostfixOps(unit: ActionUnit): A


  def ħ : A = actionPostfixOps(ReducedPlanckConstant)
  def hbar : A = actionPostfixOps(ReducedPlanckConstant)
}

trait ActionDot[A]{
  import ActionUnit._

  protected def actionDot(unit: ActionUnit): A

  def ħ(dot: Dot): A = actionDot(ReducedPlanckConstant)
  def hbar(dot: Dot): A = actionDot(ReducedPlanckConstant)
}

trait ActionPer[A]{
  import ActionUnit._

  protected def actionPer(unit: ActionUnit): A

  def ħ(per: Per): A = actionPer(ReducedPlanckConstant)
  def hbar(per: Per): A = actionPer(ReducedPlanckConstant)
}

trait PredefinedActionUnit extends ActionPostfixOps[ActionUnit]{
  override protected def actionPostfixOps(unit: ActionUnit) = unit
  
}

object PredefinedActionUnit extends PredefinedActionUnit
