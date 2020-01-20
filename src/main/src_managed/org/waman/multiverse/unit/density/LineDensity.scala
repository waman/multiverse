package org.waman.multiverse.unit.density

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class LineDensity[A: Fractional](val value: A, val unit: LineDensityUnit)
    extends LinearQuantity[LineDensity[A], A, LineDensityUnit] {

  override protected def newQuantity(value: A, unit: LineDensityUnit): LineDensity[A] = new LineDensity(value, unit)

}

trait LineDensityUnit extends LinearUnit[LineDensityUnit]{

  override def getSIUnit: LineDensityUnit = LineDensityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = LineDensityUnit.dimension

}

/** For user defined units */
class SimpleLineDensityUnit(val name: String, val symbol: String, val interval: Real) extends LineDensityUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultLineDensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LineDensityUnit

object LineDensityUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](M -> 1, L -> -1).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.MassUnit
  import org.waman.multiverse.unit.basic.LengthUnit
  val getSIUnit: LineDensityUnit = MassUnit.getSIUnit / LengthUnit.getSIUnit

  import LineDensityUnitObjects._
  def getUnits: Seq[LineDensityUnit] =
    Seq(denier, tex)
}

object LineDensityUnitObjects{

  final case object denier extends DefaultLineDensityUnit("denier", "D", Nil, r"1"/r"9e6")
  final case object tex extends DefaultLineDensityUnit("tex", "tex", Nil, r"1e-6")
}

object LineDensityUnits{
  def D: LineDensityUnit = LineDensityUnitObjects.denier
  def tex: LineDensityUnit = LineDensityUnitObjects.tex
}