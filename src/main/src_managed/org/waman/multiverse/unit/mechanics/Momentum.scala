package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.basic.Area
import org.waman.multiverse.unit.basic.AreaUnit

import org.waman.multiverse.unit.fluid.DynamicViscosity
import org.waman.multiverse.unit.fluid.DynamicViscosityUnit

class Momentum[A: Fractional](val value: A, val unit: MomentumUnit)
    extends LinearQuantity[Momentum[A], A, MomentumUnit] {

  override protected def newQuantity(value: A, unit: MomentumUnit): Momentum[A] = new Momentum(value, unit)
  def /(area: Area[A]): DynamicViscosity[A] = new DynamicViscosity(this.value / area.value, this.unit / area.unit)

}

trait MomentumUnit extends LinearUnit[MomentumUnit]{

  override def getSIUnit: MomentumUnit = MomentumUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = MomentumUnit.dimension

  def /(areaUnit: AreaUnit): DynamicViscosityUnit =
    new QuotientUnit[DynamicViscosityUnit, MomentumUnit, AreaUnit](MomentumUnit.this, areaUnit) with DynamicViscosityUnit

}

object MomentumUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 1, M -> 1, L -> 1).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.TimeUnit
  val getSIUnit: MomentumUnit = ForceUnit.getSIUnit * TimeUnit.getSIUnit

  def getUnits: Seq[MomentumUnit] =
    Seq()
}



class DefaultMomentumUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends MomentumUnit

object MomentumUnitObjects{

}

object MomentumUnits{
}