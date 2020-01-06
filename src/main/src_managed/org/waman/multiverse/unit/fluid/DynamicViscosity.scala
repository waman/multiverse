package org.waman.multiverse.unit.fluid

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class DynamicViscosity[A: Fractional](val value: A, val unit: DynamicViscosityUnit)
    extends LinearQuantity[DynamicViscosity[A], A, DynamicViscosityUnit] {

  override protected def newQuantity(value: A, unit: DynamicViscosityUnit): DynamicViscosity[A] = new DynamicViscosity(value, unit)

}

trait DynamicViscosityUnit extends LinearUnit[DynamicViscosityUnit]{

  override def getSIUnit: DynamicViscosityUnit = DynamicViscosityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = DynamicViscosityUnit.dimension

}

/** For user defined units */
class SimpleDynamicViscosityUnit(val name: String, val symbol: String, val interval: Real) extends DynamicViscosityUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultDynamicViscosityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends DynamicViscosityUnit

object DynamicViscosityUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1, M -> 1, L -> -1).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.TimeUnit
  val getSIUnit: DynamicViscosityUnit = PressureUnit.getSIUnit * TimeUnit.getSIUnit

  import DynamicViscosityUnitObjects._
  def getUnits: Seq[DynamicViscosityUnit] =
    Seq(poise)
}

object DynamicViscosityUnitObjects{

  final case object poise extends DefaultDynamicViscosityUnit("poise", "P", Nil, r"0.1")
}

object DynamicViscosityUnits{
  def P: DynamicViscosityUnit = DynamicViscosityUnitObjects.poise
}