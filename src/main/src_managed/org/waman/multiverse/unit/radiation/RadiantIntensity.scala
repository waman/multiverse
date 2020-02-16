package org.waman.multiverse.unit.radiation

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.basic.Area
import org.waman.multiverse.unit.basic.AreaUnit


class RadiantIntensity[A: Fractional](val value: A, val unit: RadiantIntensityUnit)
    extends LinearQuantity[RadiantIntensity[A], A, RadiantIntensityUnit] {

  override protected def newQuantity(value: A, unit: RadiantIntensityUnit): RadiantIntensity[A] = new RadiantIntensity(value, unit)

  def /(area: Area[A]): Radiance[A] = new Radiance(this.value / area.value, this.unit / area.unit)
}

/** null */
trait RadiantIntensityUnit extends LinearUnit[RadiantIntensityUnit]{

  override def getSIUnit: RadiantIntensityUnit = RadiantIntensityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = RadiantIntensityUnit.dimension

  def /(areaUnit: AreaUnit): RadianceUnit =
    new AbstractQuotientUnit[RadianceUnit, RadiantIntensityUnit, AreaUnit](RadiantIntensityUnit.this, areaUnit) with RadianceUnit
}

object RadiantIntensityUnit extends UnitInfo[RadiantIntensityUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -3, M -> 1, L -> 2).withDefaultValue(0)

  import org.waman.multiverse.unit.mechanics.PowerUnit
  import org.waman.multiverse.unit.angle.SolidAngleUnit
  val getSIUnit: RadiantIntensityUnit = PowerUnit.getSIUnit / SolidAngleUnit.getSIUnit

  def getUnits: Seq[RadiantIntensityUnit] =
    Seq()
}

/** For no aliase or user defined units */
class SimpleRadiantIntensityUnit(val name: String, val symbol: String, val interval: Real) extends RadiantIntensityUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultRadiantIntensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends RadiantIntensityUnit

object RadiantIntensityUnitObjects{

}

object RadiantIntensityUnits{
}