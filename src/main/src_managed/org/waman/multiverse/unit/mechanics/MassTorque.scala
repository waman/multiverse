package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.basic.Time
import org.waman.multiverse.unit.basic.TimeUnit


class MassTorque[A: Fractional](val value: A, val unit: MassTorqueUnit)
    extends LinearQuantity[MassTorque[A], A, MassTorqueUnit] {

  override protected def newQuantity(value: A, unit: MassTorqueUnit): MassTorque[A] = new MassTorque(value, unit)

  def /(time: Time[A]): Momentum[A] = new Momentum(this.value / time.value, this.unit / time.unit)
}

/** null */
trait MassTorqueUnit extends LinearUnit[MassTorqueUnit]{

  override def getSIUnit: MassTorqueUnit = MassTorqueUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = MassTorqueUnit.dimension

  def /(timeUnit: TimeUnit): MomentumUnit =
    new AbstractQuotientUnit[MomentumUnit, MassTorqueUnit, TimeUnit](MassTorqueUnit.this, timeUnit) with MomentumUnit
}

object MassTorqueUnit extends UnitInfo[MassTorqueUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](M -> 1, L -> 1).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.MassUnit
  import org.waman.multiverse.unit.basic.LengthUnit
  val getSIUnit: MassTorqueUnit = MassUnit.getSIUnit * LengthUnit.getSIUnit

  def getUnits: Seq[MassTorqueUnit] =
    Seq()
}

/** For no aliase or user defined units */
class SimpleMassTorqueUnit(val name: String, val symbol: String, val interval: Real) extends MassTorqueUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultMassTorqueUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends MassTorqueUnit

object MassTorqueUnitObjects{

}

object MassTorqueUnits{

}