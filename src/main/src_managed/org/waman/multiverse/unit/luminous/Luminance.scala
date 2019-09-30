package org.waman.multiverse.unit.luminous

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

import org.waman.multiverse._

class Luminance[A: Fractional](val value: A, val unit: LuminanceUnit)
    extends LinearQuantity[Luminance[A], A, LuminanceUnit] {

  override protected def newQuantity(value: A, unit: LuminanceUnit): Luminance[A] = new Luminance(value, unit)
}

trait LuminanceUnit extends LinearUnit[LuminanceUnit]{
  override def getSIUnit: LuminanceUnit = LuminanceUnitObjects.getSIUnit

}

class DefaultLuminanceUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LuminanceUnit


object LuminanceUnitObjects{
  import org.waman.multiverse.unit.Constants

  import org.waman.multiverse.unit.basic.AreaUnitObjects

  val getSIUnit: LuminanceUnit = LuminousIntensityUnitObjects.getSIUnit / AreaUnitObjects.getSIUnit

  final object stilb extends DefaultLuminanceUnit("stilb", "sb", Nil, r"1e4")
  final object lambert extends DefaultLuminanceUnit("lambert", "Lb", Nil, r"1e4" / Constants.Pi)
  final object apo_stilb extends DefaultLuminanceUnit("apo stilb", "asb", Nil, r"1" / Constants.Pi)
  final object skot extends DefaultLuminanceUnit("skot", "sk", Nil, r"1e-3" / Constants.Pi)
  final object bril extends DefaultLuminanceUnit("bril", "bril", Nil, r"1e-7" / Constants.Pi)

  def getUnits: Seq[LuminanceUnit] =
    Seq(stilb, lambert, apo_stilb, skot, bril)
}


object LuminanceUnits{
  def sb: LuminanceUnit = LuminanceUnitObjects.stilb
  def Lb: LuminanceUnit = LuminanceUnitObjects.lambert
  def asb: LuminanceUnit = LuminanceUnitObjects.apo_stilb
  def sk: LuminanceUnit = LuminanceUnitObjects.skot
  def bril: LuminanceUnit = LuminanceUnitObjects.bril

  def getSIUnit: LuminanceUnit = LuminanceUnitObjects.getSIUnit
  def getUnits: Seq[LuminanceUnit] = LuminanceUnitObjects.getUnits
}
