package org.waman.multiverse.unit.angle

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class SolidAngle[A: Fractional](val value: A, val unit: SolidAngleUnit)
    extends LinearQuantity[SolidAngle[A], A, SolidAngleUnit] {

  override protected def newQuantity(value: A, unit: SolidAngleUnit): SolidAngle[A] = new SolidAngle(value, unit)
}

/** null */
trait SolidAngleUnit extends LinearUnit[SolidAngleUnit]{

  override def getSIUnit: SolidAngleUnit = SolidAngleUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = SolidAngleUnit.dimension
}

object SolidAngleUnit extends UnitInfo[SolidAngleUnit]{
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int]().withDefaultValue(0)

  def getSIUnit: SolidAngleUnit = SolidAngleUnitObjects.steradian

  import SolidAngleUnitObjects._
  def getUnits: Seq[SolidAngleUnit] =
    Seq(steradian, yoctosteradian, zeptosteradian, attosteradian, femtosteradian, picosteradian, nanosteradian, microsteradian, millisteradian, centisteradian, decisteradian, decasteradian, spat, square_degree)
}

/** For no aliase or user defined units */
class SimpleSolidAngleUnit(val name: String, val symbol: String, val interval: Real) extends SolidAngleUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultSolidAngleUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends SolidAngleUnit

object SolidAngleUnitObjects{
  import org.waman.multiverse.unit.Constants
  import org.waman.multiverse.unit.angle.AngleUnitObjects._

  final case object steradian extends SimpleSolidAngleUnit("steradian", "sr", 1)
  final case object yoctosteradian extends SimpleSolidAngleUnit("yoctosteradian", "ysr", r"1e-24")
  final case object zeptosteradian extends SimpleSolidAngleUnit("zeptosteradian", "zsr", r"1e-21")
  final case object attosteradian extends SimpleSolidAngleUnit("attosteradian", "asr", r"1e-18")
  final case object femtosteradian extends SimpleSolidAngleUnit("femtosteradian", "fsr", r"1e-15")
  final case object picosteradian extends SimpleSolidAngleUnit("picosteradian", "psr", r"1e-12")
  final case object nanosteradian extends SimpleSolidAngleUnit("nanosteradian", "nsr", r"1e-9")
  final case object microsteradian extends DefaultSolidAngleUnit("microsteradian", "μsr", Seq("mcsr"), r"1e-6")
  final case object millisteradian extends SimpleSolidAngleUnit("millisteradian", "msr", r"1e-3")
  final case object centisteradian extends SimpleSolidAngleUnit("centisteradian", "csr", r"1e-2")
  final case object decisteradian extends SimpleSolidAngleUnit("decisteradian", "dsr", r"1e-1")
  final case object decasteradian extends SimpleSolidAngleUnit("decasteradian", "dasr", r"1e1")
  final case object spat extends SimpleSolidAngleUnit("spat", "spat", r"4" * Constants.Pi * steradian.interval)
  final case object square_degree extends DefaultSolidAngleUnit("square degree", "deg²", Seq("deg2"), degree.interval * degree.interval)
}

object SolidAngleUnits{
  def sr: SolidAngleUnit = SolidAngleUnitObjects.steradian
  def ysr: SolidAngleUnit = SolidAngleUnitObjects.yoctosteradian
  def zsr: SolidAngleUnit = SolidAngleUnitObjects.zeptosteradian
  def asr: SolidAngleUnit = SolidAngleUnitObjects.attosteradian
  def fsr: SolidAngleUnit = SolidAngleUnitObjects.femtosteradian
  def psr: SolidAngleUnit = SolidAngleUnitObjects.picosteradian
  def nsr: SolidAngleUnit = SolidAngleUnitObjects.nanosteradian
  def `μsr`: SolidAngleUnit = SolidAngleUnitObjects.microsteradian
  def mcsr: SolidAngleUnit = SolidAngleUnitObjects.microsteradian
  def msr: SolidAngleUnit = SolidAngleUnitObjects.millisteradian
  def csr: SolidAngleUnit = SolidAngleUnitObjects.centisteradian
  def dsr: SolidAngleUnit = SolidAngleUnitObjects.decisteradian
  def dasr: SolidAngleUnit = SolidAngleUnitObjects.decasteradian
  def spat: SolidAngleUnit = SolidAngleUnitObjects.spat
  def `deg²`: SolidAngleUnit = SolidAngleUnitObjects.square_degree
  def deg2: SolidAngleUnit = SolidAngleUnitObjects.square_degree
}