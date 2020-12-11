package multiverse.unit.electromagnetism

import spire.math.Real
import spire.math.Fractional

import multiverse._


class MagneticFieldStrength[A: Fractional](val value: A, val unit: MagneticFieldStrengthUnit)
    extends LinearQuantity[MagneticFieldStrength[A], A, MagneticFieldStrengthUnit] {

  override protected def newQuantity(value: A, unit: MagneticFieldStrengthUnit): MagneticFieldStrength[A] = new MagneticFieldStrength(value, unit)
}

trait MagneticFieldStrengthUnit extends LinearUnit[MagneticFieldStrengthUnit]{

  override def getSIUnit: MagneticFieldStrengthUnit = MagneticFieldStrengthUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = MagneticFieldStrengthUnit.dimension
}

object MagneticFieldStrengthUnit extends UnitInfo[MagneticFieldStrengthUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](I -> 1, L -> -1).withDefaultValue(0)

  import multiverse.unit.basic.LengthUnit
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

  import spire.implicits._

  import multiverse.unit.Constants

  final case object oersted extends SimpleMagneticFieldStrengthUnit("oersted", "Oe", r"1e3" / (r"4" * Constants.Pi))
}

object MagneticFieldStrengthUnits{

  def Oe: MagneticFieldStrengthUnit = MagneticFieldStrengthUnitObjects.oersted
}