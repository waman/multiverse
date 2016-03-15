package org.waman.multiverse.energy

import org.waman.multiverse._
import org.waman.multiverse.time.TimeUnit
import spire.implicits._
import spire.math.Real

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


  case object AtomicUnitOfAction extends IntrinsicActionUnit("AtomicUnitOfAction", Seq("ħ", "hbar"), r"1.05457168e-34") with NotExact

  override lazy val values = Seq(AtomicUnitOfAction)

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

trait ActionPostfixOps[A]{
  import ActionUnit._

  protected def actionPostfixOps(unit: ActionUnit): A

  def ħ : A = actionPostfixOps(AtomicUnitOfAction)
  def hbar : A = actionPostfixOps(AtomicUnitOfAction)
}

trait ActionDot[A]{
  import ActionUnit._

  protected def actionDot(unit: ActionUnit): A

  def ħ(dot: Dot): A = actionDot(AtomicUnitOfAction)
  def hbar(dot: Dot): A = actionDot(AtomicUnitOfAction)
}

trait ActionPer[A]{
  import ActionUnit._

  protected def actionPer(unit: ActionUnit): A

  def ħ(per: Per): A = actionPer(AtomicUnitOfAction)
  def hbar(per: Per): A = actionPer(AtomicUnitOfAction)
}

trait PredefinedActionUnit extends ActionPostfixOps[ActionUnit]{
  override protected def actionPostfixOps(unit: ActionUnit) = unit
  
}

object PredefinedActionUnit extends PredefinedActionUnit
