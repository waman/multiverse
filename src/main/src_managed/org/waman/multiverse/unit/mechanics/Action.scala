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
  override def getSIUnit: ActionUnit = ActionUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ActionUnit.dimension

}

object ActionUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, M -> 1, L -> 2).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.TimeUnit
  val getSIUnit: ActionUnit = EnergyUnit.getSIUnit * TimeUnit.getSIUnit

import ActionUnitObjects._
  def getUnits: Seq[ActionUnit] =
    Seq(planck_constant, reduced_planck_constant)
}



class DefaultActionUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ActionUnit

object ActionUnitObjects{
  import org.waman.multiverse.unit.Constants

  final object planck_constant extends DefaultActionUnit("planck constant", "h", Nil, Constants.PlanckConstant)
  final object reduced_planck_constant extends DefaultActionUnit("reduced planck constant", "ħ", Nil, Constants.PlanckConstant / r"2.0" / Constants.Pi)
}

object ActionUnits{
  def h: ActionUnit = ActionUnitObjects.planck_constant
  def ħ: ActionUnit = ActionUnitObjects.reduced_planck_constant
}