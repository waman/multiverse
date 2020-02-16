package org.waman.multiverse.unit.radiation

import spire.math.Real
import spire.math.Fractional
import org.waman.multiverse._


class Radiance[A: Fractional](val value: A, val unit: RadianceUnit)
    extends LinearQuantity[Radiance[A], A, RadianceUnit] {

  override protected def newQuantity(value: A, unit: RadianceUnit): Radiance[A] = new Radiance(value, unit)
}

/** null */
trait RadianceUnit extends LinearUnit[RadianceUnit]{

  override def getSIUnit: RadianceUnit = RadianceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = RadianceUnit.dimension
}

object RadianceUnit extends UnitInfo[RadianceUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, M -> 1).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.AreaUnit
  val getSIUnit: RadianceUnit = RadiantIntensityUnit.getSIUnit / AreaUnit.getSIUnit

  def getUnits: Seq[RadianceUnit] =
    Seq()
}

/** For no aliase or user defined units */
class SimpleRadianceUnit(val name: String, val symbol: String, val interval: Real) extends RadianceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultRadianceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends RadianceUnit

object RadianceUnitObjects{

}

object RadianceUnits{
}