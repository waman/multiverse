package org.waman.multiverse.unit.defs.em

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.Constants

class MagneticFieldStrength[A: Fractional](val value: A, val unit: MagneticFieldStrengthUnit)
    extends LinearQuantity[MagneticFieldStrength[A], A, MagneticFieldStrengthUnit] {

  override protected def newQuantity(value: A, unit: MagneticFieldStrengthUnit): MagneticFieldStrength[A] = new MagneticFieldStrength(value, unit)
}

/** None */
trait MagneticFieldStrengthUnit extends LinearUnit[MagneticFieldStrengthUnit]{

  override def getSIUnit: MagneticFieldStrengthUnit = MagneticFieldStrengthUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = MagneticFieldStrengthUnit.dimension
}

object MagneticFieldStrengthUnit extends UnitInfo[MagneticFieldStrengthUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](I -> 1, L -> -1).withDefaultValue(0)

  val getSIUnit: MagneticFieldStrengthUnit = ElectricCurrentUnit.getSIUnit / LengthUnit.getSIUnit

  import MagneticFieldStrengthUnitObjects._

  def getUnits: Seq[MagneticFieldStrengthUnit] =
    Seq(oersted)
}


/** For no aliase or user defined units */
class SimpleMagneticFieldStrengthUnit(val name: String, val symbol: String, val interval: Real) extends MagneticFieldStrengthUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultMagneticFieldStrengthUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends MagneticFieldStrengthUnit
  
object MagneticFieldStrengthUnitObjects{

  final case object oersted extends SimpleMagneticFieldStrengthUnit("oersted", "Oe", r"1e3" / (r"4" * Constants.Pi))
}


object MagneticFieldStrengthUnits{

  /** oersted */
  def Oe: MagneticFieldStrengthUnit = MagneticFieldStrengthUnitObjects.oersted
}