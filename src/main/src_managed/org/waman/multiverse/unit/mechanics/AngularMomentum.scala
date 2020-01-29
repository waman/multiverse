package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class AngularMomentum[A: Fractional](val value: A, val unit: AngularMomentumUnit)
    extends LinearQuantity[AngularMomentum[A], A, AngularMomentumUnit] {

  override protected def newQuantity(value: A, unit: AngularMomentumUnit): AngularMomentum[A] = new AngularMomentum(value, unit)
}

/** This can be used as a unit of action. */
trait AngularMomentumUnit extends LinearUnit[AngularMomentumUnit]{

  override def getSIUnit: AngularMomentumUnit = AngularMomentumUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AngularMomentumUnit.dimension
}

object AngularMomentumUnit extends UnitInfo[AngularMomentumUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, M -> 1, L -> 2).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.TimeUnit
  val getSIUnit: AngularMomentumUnit = EnergyUnit.getSIUnit * TimeUnit.getSIUnit

  import AngularMomentumUnitObjects._
  def getUnits: Seq[AngularMomentumUnit] =
    Seq(planck_constant, reduced_planck_constant)
}

/** For no aliase or user defined units */
class SimpleAngularMomentumUnit(val name: String, val symbol: String, val interval: Real) extends AngularMomentumUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAngularMomentumUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AngularMomentumUnit

object AngularMomentumUnitObjects{
  import org.waman.multiverse.unit.Constants

  final case object planck_constant extends SimpleAngularMomentumUnit("planck constant", "h", Constants.PlanckConstant)
  final case object reduced_planck_constant extends SimpleAngularMomentumUnit("reduced planck constant", "ħ", Constants.PlanckConstant / r"2.0" / Constants.Pi)
}

object AngularMomentumUnits{
  def h: AngularMomentumUnit = AngularMomentumUnitObjects.planck_constant
  def `ħ`: AngularMomentumUnit = AngularMomentumUnitObjects.reduced_planck_constant
}