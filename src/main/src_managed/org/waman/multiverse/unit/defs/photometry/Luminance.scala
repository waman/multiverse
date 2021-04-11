package org.waman.multiverse.unit.defs.photometry

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.Constants

class Luminance[A: Fractional](val value: A, val unit: LuminanceUnit)
    extends LinearQuantity[Luminance[A], A, LuminanceUnit] {

  override protected def newQuantity(value: A, unit: LuminanceUnit): Luminance[A] = new Luminance(value, unit)
}

trait LuminanceUnit extends LinearUnit[LuminanceUnit]{

  override def getSIUnit: LuminanceUnit = LuminanceUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = LuminanceUnit.dimension
}

object LuminanceUnit extends UnitInfo[LuminanceUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](J -> 1, L -> -2).withDefaultValue(0)

  val getSIUnit: LuminanceUnit = LuminousIntensityUnit.getSIUnit / AreaUnit.getSIUnit
  import LuminanceUnitObjects._

  def getUnits: Seq[LuminanceUnit] =
    Seq(stilb, lambert, apo_stilb, skot, bril, foot_lambert)
}


/** For no aliase or user defined units */
class SimpleLuminanceUnit(val name: String, val symbol: String, val interval: Real) extends LuminanceUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultLuminanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LuminanceUnit
  
object LuminanceUnitObjects{

  final case object stilb extends SimpleLuminanceUnit("stilb", "sb", r"1e4")
  final case object lambert extends SimpleLuminanceUnit("lambert", "Lb", r"1e4" / Constants.Pi)
  final case object apo_stilb extends SimpleLuminanceUnit("apo stilb", "asb", r"1" / Constants.Pi)
  final case object skot extends SimpleLuminanceUnit("skot", "sk", r"1e-3" / Constants.Pi)
  final case object bril extends SimpleLuminanceUnit("bril", "bril", r"1e-7" / Constants.Pi)
  final case object foot_lambert extends SimpleLuminanceUnit("foot lambert", "fLb", r"1" / Constants.Pi * LuminousIntensityUnitObjects.candela.interval / AreaUnitObjects.square_foot.interval)
}


object LuminanceUnits{

  /** stilb */
  def sb: LuminanceUnit = LuminanceUnitObjects.stilb
  /** lambert */
  def Lb: LuminanceUnit = LuminanceUnitObjects.lambert
  /** apo stilb */
  def asb: LuminanceUnit = LuminanceUnitObjects.apo_stilb
  /** skot */
  def sk: LuminanceUnit = LuminanceUnitObjects.skot
  /** bril */
  def bril: LuminanceUnit = LuminanceUnitObjects.bril
  /** foot lambert */
  def fLb: LuminanceUnit = LuminanceUnitObjects.foot_lambert
}