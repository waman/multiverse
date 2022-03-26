package org.waman.multiverse.unit.defs.dens

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._

class LineDensity[A: Fractional](val value: A, val unit: LineDensityUnit)
    extends LinearQuantity[LineDensity[A], A, LineDensityUnit] {

  override protected def newQuantity(value: A, unit: LineDensityUnit): LineDensity[A] = new LineDensity(value, unit)
}

/** None */
trait LineDensityUnit extends LinearUnit[LineDensityUnit]{

  override def getSIUnit: LineDensityUnit = LineDensityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = LineDensityUnit.dimension
}

object LineDensityUnit extends UnitInfo[LineDensityUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](M -> 1, L -> -1).withDefaultValue(0)

  val getSIUnit: LineDensityUnit = MassUnit.getSIUnit / LengthUnit.getSIUnit

  import LineDensityUnitObjects._

  def getUnits: Seq[LineDensityUnit] =
    Seq(denier, tex)
}


/** For no alias or user defined units */
class SimpleLineDensityUnit(val name: String, val symbol: String, val interval: Real) extends LineDensityUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultLineDensityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LineDensityUnit
  
object LineDensityUnitObjects{

  final case object denier extends SimpleLineDensityUnit("denier", "D", r"9e-6")
  final case object tex extends SimpleLineDensityUnit("tex", "tex", r"1e-6")
}


object LineDensityUnits{

  /** denier */
  def D: LineDensityUnit = LineDensityUnitObjects.denier
  /** tex */
  def tex: LineDensityUnit = LineDensityUnitObjects.tex
}