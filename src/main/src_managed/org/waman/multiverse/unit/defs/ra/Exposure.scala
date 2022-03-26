package org.waman.multiverse.unit.defs.ra

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs.em._
import org.waman.multiverse.unit.defs._

class Exposure[A: Fractional](val value: A, val unit: ExposureUnit)
    extends LinearQuantity[Exposure[A], A, ExposureUnit] {

  override protected def newQuantity(value: A, unit: ExposureUnit): Exposure[A] = new Exposure(value, unit)
}

/** None */
trait ExposureUnit extends LinearUnit[ExposureUnit]{

  override def getSIUnit: ExposureUnit = ExposureUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = ExposureUnit.dimension
}

object ExposureUnit extends UnitInfo[ExposureUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 1, M -> -1, I -> 1).withDefaultValue(0)

  val getSIUnit: ExposureUnit = ElectricChargeUnit.getSIUnit / MassUnit.getSIUnit

  import ExposureUnitObjects._

  def getUnits: Seq[ExposureUnit] =
    Seq(roentgen)
}


/** For no alias or user defined units */
class SimpleExposureUnit(val name: String, val symbol: String, val interval: Real) extends ExposureUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultExposureUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends ExposureUnit
  
object ExposureUnitObjects{

  final case object roentgen extends SimpleExposureUnit("roentgen", "R", r"2.58e-4")
}


object ExposureUnits{

  /** roentgen */
  def R: ExposureUnit = ExposureUnitObjects.roentgen
}