package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

import org.waman.multiverse._

class Action[A: Fractional](val value: A, val unit: ActionUnit)
    extends LinearQuantity[Action[A], A, ActionUnit] {

  override protected def newQuantity(value: A, unit: ActionUnit): Action[A] = new Action(value, unit)
}

trait ActionUnit extends LinearUnit[ActionUnit]{
  override def getSIUnit: ActionUnit = ActionUnitObjects.getSIUnit

}

class DefaultActionUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ActionUnit


object ActionUnitObjects{
  import org.waman.multiverse.unit.Constants

  import org.waman.multiverse.unit.basic.TimeUnitObjects

  val getSIUnit: ActionUnit = EnergyUnitObjects.getSIUnit * TimeUnitObjects.getSIUnit

  final object planck_constant extends DefaultActionUnit("planck constant", "h", Nil, Constants.PlanckConstant)
  final object reduced_planck_constant extends DefaultActionUnit("reduced planck constant", "ħ", Nil, Constants.PlanckConstant / r"2.0" / Constants.Pi)

  def getUnits: Seq[ActionUnit] =
    Seq(planck_constant, reduced_planck_constant)
}


object ActionUnits{
  def h: ActionUnit = ActionUnitObjects.planck_constant
  def ħ: ActionUnit = ActionUnitObjects.reduced_planck_constant

  def getSIUnit: ActionUnit = ActionUnitObjects.getSIUnit
  def getUnits: Seq[ActionUnit] = ActionUnitObjects.getUnits
}
