package org.waman.multiverse.unit.radiation

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class AbsorbedDose[A: Fractional](val value: A, val unit: AbsorbedDoseUnit)
    extends LinearQuantity[AbsorbedDose[A], A, AbsorbedDoseUnit] {

  override protected def newQuantity(value: A, unit: AbsorbedDoseUnit): AbsorbedDose[A] = new AbsorbedDose(value, unit)
}

trait AbsorbedDoseUnit extends LinearUnit[AbsorbedDoseUnit]{

  override def getSIUnit: AbsorbedDoseUnit = AbsorbedDoseUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AbsorbedDoseUnit.dimension
}

object AbsorbedDoseUnit extends UnitInfo[AbsorbedDoseUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, L -> 2).withDefaultValue(0)

  def getSIUnit: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.gray

  import AbsorbedDoseUnitObjects._
  def getUnits: Seq[AbsorbedDoseUnit] =
    Seq(gray, yoctogray, zeptogray, attogray, femtogray, picogray, nanogray, microgray, milligray, centigray, decigray, decagray, hectogray, kilogray, megagray, gigagray, teragray, petagray, exagray, zettagray, yottagray)
}

/** For no aliase or user defined units */
class SimpleAbsorbedDoseUnit(val name: String, val symbol: String, val interval: Real) extends AbsorbedDoseUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAbsorbedDoseUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AbsorbedDoseUnit

object AbsorbedDoseUnitObjects{

  final case object gray extends SimpleAbsorbedDoseUnit("gray", "Gy", 1)
  final case object yoctogray extends SimpleAbsorbedDoseUnit("yoctogray", "yGy", r"1e-24")
  final case object zeptogray extends SimpleAbsorbedDoseUnit("zeptogray", "zGy", r"1e-21")
  final case object attogray extends SimpleAbsorbedDoseUnit("attogray", "aGy", r"1e-18")
  final case object femtogray extends SimpleAbsorbedDoseUnit("femtogray", "fGy", r"1e-15")
  final case object picogray extends SimpleAbsorbedDoseUnit("picogray", "pGy", r"1e-12")
  final case object nanogray extends SimpleAbsorbedDoseUnit("nanogray", "nGy", r"1e-9")
  final case object microgray extends DefaultAbsorbedDoseUnit("microgray", "μGy", Seq("mcGy"), r"1e-6")
  final case object milligray extends SimpleAbsorbedDoseUnit("milligray", "mGy", r"1e-3")
  final case object centigray extends SimpleAbsorbedDoseUnit("centigray", "cGy", r"1e-2")
  final case object decigray extends SimpleAbsorbedDoseUnit("decigray", "dGy", r"1e-1")
  final case object decagray extends SimpleAbsorbedDoseUnit("decagray", "daGy", r"1e1")
  final case object hectogray extends SimpleAbsorbedDoseUnit("hectogray", "hGy", r"1e2")
  final case object kilogray extends DefaultAbsorbedDoseUnit("kilogray", "kGy", Seq("KGy"), r"1e3")
  final case object megagray extends SimpleAbsorbedDoseUnit("megagray", "MGy", r"1e6")
  final case object gigagray extends SimpleAbsorbedDoseUnit("gigagray", "GGy", r"1e9")
  final case object teragray extends SimpleAbsorbedDoseUnit("teragray", "TGy", r"1e12")
  final case object petagray extends SimpleAbsorbedDoseUnit("petagray", "PGy", r"1e15")
  final case object exagray extends SimpleAbsorbedDoseUnit("exagray", "EGy", r"1e18")
  final case object zettagray extends SimpleAbsorbedDoseUnit("zettagray", "ZGy", r"1e21")
  final case object yottagray extends SimpleAbsorbedDoseUnit("yottagray", "YGy", r"1e24")
}

object AbsorbedDoseUnits{
  def Gy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.gray
  def yGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.yoctogray
  def zGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.zeptogray
  def aGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.attogray
  def fGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.femtogray
  def pGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.picogray
  def nGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.nanogray
  def `μGy`: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.microgray
  def mcGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.microgray
  def mGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.milligray
  def cGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.centigray
  def dGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.decigray
  def daGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.decagray
  def hGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.hectogray
  def kGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.kilogray
  def KGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.kilogray
  def MGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.megagray
  def GGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.gigagray
  def TGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.teragray
  def PGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.petagray
  def EGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.exagray
  def ZGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.zettagray
  def YGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.yottagray
}