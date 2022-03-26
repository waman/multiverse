package org.waman.multiverse.unit.defs.mech

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.Constants

class AngularMomentum[A: Fractional](val value: A, val unit: AngularMomentumUnit)
    extends LinearQuantity[AngularMomentum[A], A, AngularMomentumUnit] {

  override protected def newQuantity(value: A, unit: AngularMomentumUnit): AngularMomentum[A] = new AngularMomentum(value, unit)
}

/** Some(This can be used as a unit of action.) */
trait AngularMomentumUnit extends LinearUnit[AngularMomentumUnit]{

  override def getSIUnit: AngularMomentumUnit = AngularMomentumUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AngularMomentumUnit.dimension
}

object AngularMomentumUnit extends UnitInfo[AngularMomentumUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, M -> 1, L -> 2).withDefaultValue(0)

  val getSIUnit: AngularMomentumUnit = EnergyUnit.getSIUnit * TimeUnit.getSIUnit

  import AngularMomentumUnitObjects._

  def getUnits: Seq[AngularMomentumUnit] =
    Seq(planck_constant, reduced_planck_constant)
}


/** For no alias or user defined units */
class SimpleAngularMomentumUnit(val name: String, val symbol: String, val interval: Real) extends AngularMomentumUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAngularMomentumUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AngularMomentumUnit
  
object AngularMomentumUnitObjects{

  final case object planck_constant extends SimpleAngularMomentumUnit("planck constant", "h", Constants.PlanckConstant)
  final case object reduced_planck_constant extends DefaultAngularMomentumUnit("reduced planck constant", "ħ", Seq("hbar", "atomic_unit_of_action"), Constants.PlanckConstant / r"2.0" / Constants.Pi)
}


object AngularMomentumUnits{

  /** planck constant */
  def h: AngularMomentumUnit = AngularMomentumUnitObjects.planck_constant
  /** reduced planck constant */
  def ħ: AngularMomentumUnit = AngularMomentumUnitObjects.reduced_planck_constant
  /** reduced planck constant */
  def hbar: AngularMomentumUnit = AngularMomentumUnitObjects.reduced_planck_constant
  /** reduced planck constant */
  def atomic_unit_of_action: AngularMomentumUnit = AngularMomentumUnitObjects.reduced_planck_constant
}