package org.waman.multiverse.unit.angle

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class SolidAngle[A: Fractional](val value: A, val unit: SolidAngleUnit)
    extends LinearQuantity[SolidAngle[A], A, SolidAngleUnit] {

  override protected def newQuantity(value: A, unit: SolidAngleUnit): SolidAngle[A] = new SolidAngle(value, unit)

}

trait SolidAngleUnit extends LinearUnit[SolidAngleUnit]{

  override def getSIUnit: SolidAngleUnit = SolidAngleUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = SolidAngleUnit.dimension

}

object SolidAngleUnit{
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int]().withDefaultValue(0)

  def getSIUnit: SolidAngleUnit = SolidAngleUnitObjects.steradian

  import SolidAngleUnitObjects._
  def getUnits: Seq[SolidAngleUnit] =
    Seq(steradian, yoctosteradian, zeptosteradian, attosteradian, femtosteradian, picosteradian, nanosteradian, microsteradian, millisteradian, centisteradian, decisteradian, decasteradian, spat, square_degree)
}

class DefaultSolidAngleUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends SolidAngleUnit

object SolidAngleUnitObjects{
  import org.waman.multiverse.unit.Constants

  final object steradian extends DefaultSolidAngleUnit("steradian", "sr", Nil, 1)
  final object yoctosteradian extends DefaultSolidAngleUnit("yoctosteradian", "ysr", Nil, 1 * r"1e-24")
  final object zeptosteradian extends DefaultSolidAngleUnit("zeptosteradian", "zsr", Nil, 1 * r"1e-21")
  final object attosteradian extends DefaultSolidAngleUnit("attosteradian", "asr", Nil, 1 * r"1e-18")
  final object femtosteradian extends DefaultSolidAngleUnit("femtosteradian", "fsr", Nil, 1 * r"1e-15")
  final object picosteradian extends DefaultSolidAngleUnit("picosteradian", "psr", Nil, 1 * r"1e-12")
  final object nanosteradian extends DefaultSolidAngleUnit("nanosteradian", "nsr", Nil, 1 * r"1e-9")
  final object microsteradian extends DefaultSolidAngleUnit("microsteradian", "μsr", Seq("mcsr"), 1 * r"1e-6")
  final object millisteradian extends DefaultSolidAngleUnit("millisteradian", "msr", Nil, 1 * r"1e-3")
  final object centisteradian extends DefaultSolidAngleUnit("centisteradian", "csr", Nil, 1 * r"1e-2")
  final object decisteradian extends DefaultSolidAngleUnit("decisteradian", "dsr", Nil, 1 * r"1e-1")
  final object decasteradian extends DefaultSolidAngleUnit("decasteradian", "dasr", Nil, 1 * r"1e1")
  final object spat extends DefaultSolidAngleUnit("spat", "spat", Nil, r"4" * Constants.Pi * steradian.interval)
  final object square_degree extends DefaultSolidAngleUnit("square degree", "deg²", Seq("deg2"), AngleUnitObjects.degree.interval * AngleUnitObjects.degree.interval)
}

object SolidAngleUnits{
  def sr: SolidAngleUnit = SolidAngleUnitObjects.steradian
  def ysr: SolidAngleUnit = SolidAngleUnitObjects.yoctosteradian
  def zsr: SolidAngleUnit = SolidAngleUnitObjects.zeptosteradian
  def asr: SolidAngleUnit = SolidAngleUnitObjects.attosteradian
  def fsr: SolidAngleUnit = SolidAngleUnitObjects.femtosteradian
  def psr: SolidAngleUnit = SolidAngleUnitObjects.picosteradian
  def nsr: SolidAngleUnit = SolidAngleUnitObjects.nanosteradian
  def μsr: SolidAngleUnit = SolidAngleUnitObjects.microsteradian
  def mcsr: SolidAngleUnit = SolidAngleUnitObjects.microsteradian
  def msr: SolidAngleUnit = SolidAngleUnitObjects.millisteradian
  def csr: SolidAngleUnit = SolidAngleUnitObjects.centisteradian
  def dsr: SolidAngleUnit = SolidAngleUnitObjects.decisteradian
  def dasr: SolidAngleUnit = SolidAngleUnitObjects.decasteradian
  def spat: SolidAngleUnit = SolidAngleUnitObjects.spat
  def `deg²`: SolidAngleUnit = SolidAngleUnitObjects.square_degree
  def deg2: SolidAngleUnit = SolidAngleUnitObjects.square_degree
}