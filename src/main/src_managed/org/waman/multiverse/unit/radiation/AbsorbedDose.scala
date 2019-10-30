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

object AbsorbedDoseUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, L -> 2).withDefaultValue(0)

  def getSIUnit: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.gray

import AbsorbedDoseUnitObjects._
  def getUnits: Seq[AbsorbedDoseUnit] =
    Seq(gray, yoctogray, zeptogray, attogray, femtogray, picogray, nanogray, microgray, milligray, centigray, decigray, decagray, hectogray, kilogray, megagray, gigagray, teragray, petagray, exagray, zettagray, yottagray)
}



class DefaultAbsorbedDoseUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AbsorbedDoseUnit

object AbsorbedDoseUnitObjects{

  final object gray extends DefaultAbsorbedDoseUnit("gray", "Gy", Nil, r"1")
  final object yoctogray extends DefaultAbsorbedDoseUnit("yoctogray", "yGy", Nil, r"1" * r"1e-24")
  final object zeptogray extends DefaultAbsorbedDoseUnit("zeptogray", "zGy", Nil, r"1" * r"1e-21")
  final object attogray extends DefaultAbsorbedDoseUnit("attogray", "aGy", Nil, r"1" * r"1e-18")
  final object femtogray extends DefaultAbsorbedDoseUnit("femtogray", "fGy", Nil, r"1" * r"1e-15")
  final object picogray extends DefaultAbsorbedDoseUnit("picogray", "pGy", Nil, r"1" * r"1e-12")
  final object nanogray extends DefaultAbsorbedDoseUnit("nanogray", "nGy", Nil, r"1" * r"1e-9")
  final object microgray extends DefaultAbsorbedDoseUnit("microgray", "μGy", Seq("mcGy"), r"1" * r"1e-6")
  final object milligray extends DefaultAbsorbedDoseUnit("milligray", "mGy", Nil, r"1" * r"1e-3")
  final object centigray extends DefaultAbsorbedDoseUnit("centigray", "cGy", Nil, r"1" * r"1e-2")
  final object decigray extends DefaultAbsorbedDoseUnit("decigray", "dGy", Nil, r"1" * r"1e-1")
  final object decagray extends DefaultAbsorbedDoseUnit("decagray", "daGy", Nil, r"1" * r"1e1")
  final object hectogray extends DefaultAbsorbedDoseUnit("hectogray", "hGy", Nil, r"1" * r"1e2")
  final object kilogray extends DefaultAbsorbedDoseUnit("kilogray", "kGy", Seq("KGy"), r"1" * r"1e3")
  final object megagray extends DefaultAbsorbedDoseUnit("megagray", "MGy", Nil, r"1" * r"1e6")
  final object gigagray extends DefaultAbsorbedDoseUnit("gigagray", "GGy", Nil, r"1" * r"1e9")
  final object teragray extends DefaultAbsorbedDoseUnit("teragray", "TGy", Nil, r"1" * r"1e12")
  final object petagray extends DefaultAbsorbedDoseUnit("petagray", "PGy", Nil, r"1" * r"1e15")
  final object exagray extends DefaultAbsorbedDoseUnit("exagray", "EGy", Nil, r"1" * r"1e18")
  final object zettagray extends DefaultAbsorbedDoseUnit("zettagray", "ZGy", Nil, r"1" * r"1e21")
  final object yottagray extends DefaultAbsorbedDoseUnit("yottagray", "YGy", Nil, r"1" * r"1e24")
}

object AbsorbedDoseUnits{
  def Gy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.gray
  def yGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.yoctogray
  def zGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.zeptogray
  def aGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.attogray
  def fGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.femtogray
  def pGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.picogray
  def nGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.nanogray
  def μGy: AbsorbedDoseUnit = AbsorbedDoseUnitObjects.microgray
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