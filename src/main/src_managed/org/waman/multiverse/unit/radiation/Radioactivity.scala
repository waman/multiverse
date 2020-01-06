package org.waman.multiverse.unit.radiation

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class Radioactivity[A: Fractional](val value: A, val unit: RadioactivityUnit)
    extends LinearQuantity[Radioactivity[A], A, RadioactivityUnit] {

  override protected def newQuantity(value: A, unit: RadioactivityUnit): Radioactivity[A] = new Radioactivity(value, unit)

}

trait RadioactivityUnit extends LinearUnit[RadioactivityUnit]{

  override def getSIUnit: RadioactivityUnit = RadioactivityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = RadioactivityUnit.dimension

}

/** For user defined units */
class SimpleRadioactivityUnit(val name: String, val symbol: String, val interval: Real) extends RadioactivityUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultRadioactivityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends RadioactivityUnit

object RadioactivityUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1).withDefaultValue(0)

  def getSIUnit: RadioactivityUnit = RadioactivityUnitObjects.becquerel

  import RadioactivityUnitObjects._
  def getUnits: Seq[RadioactivityUnit] =
    Seq(becquerel, yoctobecquerel, zeptobecquerel, attobecquerel, femtobecquerel, picobecquerel, nanobecquerel, microbecquerel, millibecquerel, centibecquerel, decibecquerel, decabecquerel, hectobecquerel, kilobecquerel, megabecquerel, gigabecquerel, terabecquerel, petabecquerel, exabecquerel, zettabecquerel, yottabecquerel, curie, yoctocurie, zeptocurie, attocurie, femtocurie, picocurie, nanocurie, microcurie, millicurie, centicurie, decicurie, decacurie, hectocurie, kilocurie, megacurie, gigacurie, teracurie, petacurie, exacurie, zettacurie, yottacurie, rutherford)
}

object RadioactivityUnitObjects{

  final case object becquerel extends DefaultRadioactivityUnit("becquerel", "Bq", Nil, 1)
  final case object yoctobecquerel extends DefaultRadioactivityUnit("yoctobecquerel", "yBq", Nil, r"1e-24")
  final case object zeptobecquerel extends DefaultRadioactivityUnit("zeptobecquerel", "zBq", Nil, r"1e-21")
  final case object attobecquerel extends DefaultRadioactivityUnit("attobecquerel", "aBq", Nil, r"1e-18")
  final case object femtobecquerel extends DefaultRadioactivityUnit("femtobecquerel", "fBq", Nil, r"1e-15")
  final case object picobecquerel extends DefaultRadioactivityUnit("picobecquerel", "pBq", Nil, r"1e-12")
  final case object nanobecquerel extends DefaultRadioactivityUnit("nanobecquerel", "nBq", Nil, r"1e-9")
  final case object microbecquerel extends DefaultRadioactivityUnit("microbecquerel", "μBq", Seq("mcBq"), r"1e-6")
  final case object millibecquerel extends DefaultRadioactivityUnit("millibecquerel", "mBq", Nil, r"1e-3")
  final case object centibecquerel extends DefaultRadioactivityUnit("centibecquerel", "cBq", Nil, r"1e-2")
  final case object decibecquerel extends DefaultRadioactivityUnit("decibecquerel", "dBq", Nil, r"1e-1")
  final case object decabecquerel extends DefaultRadioactivityUnit("decabecquerel", "daBq", Nil, r"1e1")
  final case object hectobecquerel extends DefaultRadioactivityUnit("hectobecquerel", "hBq", Nil, r"1e2")
  final case object kilobecquerel extends DefaultRadioactivityUnit("kilobecquerel", "kBq", Seq("KBq"), r"1e3")
  final case object megabecquerel extends DefaultRadioactivityUnit("megabecquerel", "MBq", Nil, r"1e6")
  final case object gigabecquerel extends DefaultRadioactivityUnit("gigabecquerel", "GBq", Nil, r"1e9")
  final case object terabecquerel extends DefaultRadioactivityUnit("terabecquerel", "TBq", Nil, r"1e12")
  final case object petabecquerel extends DefaultRadioactivityUnit("petabecquerel", "PBq", Nil, r"1e15")
  final case object exabecquerel extends DefaultRadioactivityUnit("exabecquerel", "EBq", Nil, r"1e18")
  final case object zettabecquerel extends DefaultRadioactivityUnit("zettabecquerel", "ZBq", Nil, r"1e21")
  final case object yottabecquerel extends DefaultRadioactivityUnit("yottabecquerel", "YBq", Nil, r"1e24")
  final case object curie extends DefaultRadioactivityUnit("curie", "Ci", Nil, r"3.7e10")
  final case object yoctocurie extends DefaultRadioactivityUnit("yoctocurie", "yCi", Nil, r"3.7e10" * r"1e-24")
  final case object zeptocurie extends DefaultRadioactivityUnit("zeptocurie", "zCi", Nil, r"3.7e10" * r"1e-21")
  final case object attocurie extends DefaultRadioactivityUnit("attocurie", "aCi", Nil, r"3.7e10" * r"1e-18")
  final case object femtocurie extends DefaultRadioactivityUnit("femtocurie", "fCi", Nil, r"3.7e10" * r"1e-15")
  final case object picocurie extends DefaultRadioactivityUnit("picocurie", "pCi", Nil, r"3.7e10" * r"1e-12")
  final case object nanocurie extends DefaultRadioactivityUnit("nanocurie", "nCi", Nil, r"3.7e10" * r"1e-9")
  final case object microcurie extends DefaultRadioactivityUnit("microcurie", "μCi", Seq("mcCi"), r"3.7e10" * r"1e-6")
  final case object millicurie extends DefaultRadioactivityUnit("millicurie", "mCi", Nil, r"3.7e10" * r"1e-3")
  final case object centicurie extends DefaultRadioactivityUnit("centicurie", "cCi", Nil, r"3.7e10" * r"1e-2")
  final case object decicurie extends DefaultRadioactivityUnit("decicurie", "dCi", Nil, r"3.7e10" * r"1e-1")
  final case object decacurie extends DefaultRadioactivityUnit("decacurie", "daCi", Nil, r"3.7e10" * r"1e1")
  final case object hectocurie extends DefaultRadioactivityUnit("hectocurie", "hCi", Nil, r"3.7e10" * r"1e2")
  final case object kilocurie extends DefaultRadioactivityUnit("kilocurie", "kCi", Seq("KCi"), r"3.7e10" * r"1e3")
  final case object megacurie extends DefaultRadioactivityUnit("megacurie", "MCi", Nil, r"3.7e10" * r"1e6")
  final case object gigacurie extends DefaultRadioactivityUnit("gigacurie", "GCi", Nil, r"3.7e10" * r"1e9")
  final case object teracurie extends DefaultRadioactivityUnit("teracurie", "TCi", Nil, r"3.7e10" * r"1e12")
  final case object petacurie extends DefaultRadioactivityUnit("petacurie", "PCi", Nil, r"3.7e10" * r"1e15")
  final case object exacurie extends DefaultRadioactivityUnit("exacurie", "ECi", Nil, r"3.7e10" * r"1e18")
  final case object zettacurie extends DefaultRadioactivityUnit("zettacurie", "ZCi", Nil, r"3.7e10" * r"1e21")
  final case object yottacurie extends DefaultRadioactivityUnit("yottacurie", "YCi", Nil, r"3.7e10" * r"1e24")
  final case object rutherford extends DefaultRadioactivityUnit("rutherford", "Rd", Nil, megabecquerel.interval)
}

object RadioactivityUnits{
  def Bq: RadioactivityUnit = RadioactivityUnitObjects.becquerel
  def yBq: RadioactivityUnit = RadioactivityUnitObjects.yoctobecquerel
  def zBq: RadioactivityUnit = RadioactivityUnitObjects.zeptobecquerel
  def aBq: RadioactivityUnit = RadioactivityUnitObjects.attobecquerel
  def fBq: RadioactivityUnit = RadioactivityUnitObjects.femtobecquerel
  def pBq: RadioactivityUnit = RadioactivityUnitObjects.picobecquerel
  def nBq: RadioactivityUnit = RadioactivityUnitObjects.nanobecquerel
  def `μBq`: RadioactivityUnit = RadioactivityUnitObjects.microbecquerel
  def mcBq: RadioactivityUnit = RadioactivityUnitObjects.microbecquerel
  def mBq: RadioactivityUnit = RadioactivityUnitObjects.millibecquerel
  def cBq: RadioactivityUnit = RadioactivityUnitObjects.centibecquerel
  def dBq: RadioactivityUnit = RadioactivityUnitObjects.decibecquerel
  def daBq: RadioactivityUnit = RadioactivityUnitObjects.decabecquerel
  def hBq: RadioactivityUnit = RadioactivityUnitObjects.hectobecquerel
  def kBq: RadioactivityUnit = RadioactivityUnitObjects.kilobecquerel
  def KBq: RadioactivityUnit = RadioactivityUnitObjects.kilobecquerel
  def MBq: RadioactivityUnit = RadioactivityUnitObjects.megabecquerel
  def GBq: RadioactivityUnit = RadioactivityUnitObjects.gigabecquerel
  def TBq: RadioactivityUnit = RadioactivityUnitObjects.terabecquerel
  def PBq: RadioactivityUnit = RadioactivityUnitObjects.petabecquerel
  def EBq: RadioactivityUnit = RadioactivityUnitObjects.exabecquerel
  def ZBq: RadioactivityUnit = RadioactivityUnitObjects.zettabecquerel
  def YBq: RadioactivityUnit = RadioactivityUnitObjects.yottabecquerel
  def Ci: RadioactivityUnit = RadioactivityUnitObjects.curie
  def yCi: RadioactivityUnit = RadioactivityUnitObjects.yoctocurie
  def zCi: RadioactivityUnit = RadioactivityUnitObjects.zeptocurie
  def aCi: RadioactivityUnit = RadioactivityUnitObjects.attocurie
  def fCi: RadioactivityUnit = RadioactivityUnitObjects.femtocurie
  def pCi: RadioactivityUnit = RadioactivityUnitObjects.picocurie
  def nCi: RadioactivityUnit = RadioactivityUnitObjects.nanocurie
  def `μCi`: RadioactivityUnit = RadioactivityUnitObjects.microcurie
  def mcCi: RadioactivityUnit = RadioactivityUnitObjects.microcurie
  def mCi: RadioactivityUnit = RadioactivityUnitObjects.millicurie
  def cCi: RadioactivityUnit = RadioactivityUnitObjects.centicurie
  def dCi: RadioactivityUnit = RadioactivityUnitObjects.decicurie
  def daCi: RadioactivityUnit = RadioactivityUnitObjects.decacurie
  def hCi: RadioactivityUnit = RadioactivityUnitObjects.hectocurie
  def kCi: RadioactivityUnit = RadioactivityUnitObjects.kilocurie
  def KCi: RadioactivityUnit = RadioactivityUnitObjects.kilocurie
  def MCi: RadioactivityUnit = RadioactivityUnitObjects.megacurie
  def GCi: RadioactivityUnit = RadioactivityUnitObjects.gigacurie
  def TCi: RadioactivityUnit = RadioactivityUnitObjects.teracurie
  def PCi: RadioactivityUnit = RadioactivityUnitObjects.petacurie
  def ECi: RadioactivityUnit = RadioactivityUnitObjects.exacurie
  def ZCi: RadioactivityUnit = RadioactivityUnitObjects.zettacurie
  def YCi: RadioactivityUnit = RadioactivityUnitObjects.yottacurie
  def Rd: RadioactivityUnit = RadioactivityUnitObjects.rutherford
}