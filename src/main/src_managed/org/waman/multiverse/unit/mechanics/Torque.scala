package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import org.waman.multiverse._

class Torque[A: Fractional](val value: A, val unit: TorqueUnit)
    extends LinearQuantity[Torque[A], A, TorqueUnit] {

  override protected def newQuantity(value: A, unit: TorqueUnit): Torque[A] = new Torque(value, unit)
}

trait TorqueUnit extends LinearUnit[TorqueUnit]{

  override def getSIUnit: TorqueUnit = TorqueUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = TorqueUnit.dimension

}

object TorqueUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, L -> 2).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.LengthUnit
  val getSIUnit: TorqueUnit = ForceUnit.getSIUnit * LengthUnit.getSIUnit

  def getUnits: Seq[TorqueUnit] =
    Seq()
}



class DefaultTorqueUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends TorqueUnit

object TorqueUnitObjects{

}

object TorqueUnits{
}