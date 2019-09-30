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
  override def getSIUnit: RadioactivityUnit = RadioactivityUnitObjects.getSIUnit

}

class DefaultRadioactivityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends RadioactivityUnit


object RadioactivityUnitObjects{

  def getSIUnit: RadioactivityUnit = becquerel

  final object becquerel extends DefaultRadioactivityUnit("becquerel", "Bq", Nil, r"1")
  final object yoctobecquerel extends DefaultRadioactivityUnit("yoctobecquerel", "yBq", Nil, r"1" * r"1e-24")
  final object zeptobecquerel extends DefaultRadioactivityUnit("zeptobecquerel", "zBq", Nil, r"1" * r"1e-21")
  final object attobecquerel extends DefaultRadioactivityUnit("attobecquerel", "aBq", Nil, r"1" * r"1e-18")
  final object femtobecquerel extends DefaultRadioactivityUnit("femtobecquerel", "fBq", Nil, r"1" * r"1e-15")
  final object picobecquerel extends DefaultRadioactivityUnit("picobecquerel", "pBq", Nil, r"1" * r"1e-12")
  final object nanobecquerel extends DefaultRadioactivityUnit("nanobecquerel", "nBq", Nil, r"1" * r"1e-9")
  final object microbecquerel extends DefaultRadioactivityUnit("microbecquerel", "μBq", Seq("mcBq"), r"1" * r"1e-6")
  final object millibecquerel extends DefaultRadioactivityUnit("millibecquerel", "mBq", Nil, r"1" * r"1e-3")
  final object centibecquerel extends DefaultRadioactivityUnit("centibecquerel", "cBq", Nil, r"1" * r"1e-2")
  final object decibecquerel extends DefaultRadioactivityUnit("decibecquerel", "dBq", Nil, r"1" * r"1e-1")
  final object decabecquerel extends DefaultRadioactivityUnit("decabecquerel", "daBq", Nil, r"1" * r"1e1")
  final object hectobecquerel extends DefaultRadioactivityUnit("hectobecquerel", "hBq", Nil, r"1" * r"1e2")
  final object kilobecquerel extends DefaultRadioactivityUnit("kilobecquerel", "kBq", Seq("KBq"), r"1" * r"1e3")
  final object megabecquerel extends DefaultRadioactivityUnit("megabecquerel", "MBq", Nil, r"1" * r"1e6")
  final object gigabecquerel extends DefaultRadioactivityUnit("gigabecquerel", "GBq", Nil, r"1" * r"1e9")
  final object terabecquerel extends DefaultRadioactivityUnit("terabecquerel", "TBq", Nil, r"1" * r"1e12")
  final object petabecquerel extends DefaultRadioactivityUnit("petabecquerel", "PBq", Nil, r"1" * r"1e15")
  final object exabecquerel extends DefaultRadioactivityUnit("exabecquerel", "EBq", Nil, r"1" * r"1e18")
  final object zettabecquerel extends DefaultRadioactivityUnit("zettabecquerel", "ZBq", Nil, r"1" * r"1e21")
  final object yottabecquerel extends DefaultRadioactivityUnit("yottabecquerel", "YBq", Nil, r"1" * r"1e24")
  final object curie extends DefaultRadioactivityUnit("curie", "Ci", Nil, r"3.7e10")
  final object yoctocurie extends DefaultRadioactivityUnit("yoctocurie", "yCi", Nil, r"3.7e10" * r"1e-24")
  final object zeptocurie extends DefaultRadioactivityUnit("zeptocurie", "zCi", Nil, r"3.7e10" * r"1e-21")
  final object attocurie extends DefaultRadioactivityUnit("attocurie", "aCi", Nil, r"3.7e10" * r"1e-18")
  final object femtocurie extends DefaultRadioactivityUnit("femtocurie", "fCi", Nil, r"3.7e10" * r"1e-15")
  final object picocurie extends DefaultRadioactivityUnit("picocurie", "pCi", Nil, r"3.7e10" * r"1e-12")
  final object nanocurie extends DefaultRadioactivityUnit("nanocurie", "nCi", Nil, r"3.7e10" * r"1e-9")
  final object microcurie extends DefaultRadioactivityUnit("microcurie", "μCi", Seq("mcCi"), r"3.7e10" * r"1e-6")
  final object millicurie extends DefaultRadioactivityUnit("millicurie", "mCi", Nil, r"3.7e10" * r"1e-3")
  final object centicurie extends DefaultRadioactivityUnit("centicurie", "cCi", Nil, r"3.7e10" * r"1e-2")
  final object decicurie extends DefaultRadioactivityUnit("decicurie", "dCi", Nil, r"3.7e10" * r"1e-1")
  final object decacurie extends DefaultRadioactivityUnit("decacurie", "daCi", Nil, r"3.7e10" * r"1e1")
  final object hectocurie extends DefaultRadioactivityUnit("hectocurie", "hCi", Nil, r"3.7e10" * r"1e2")
  final object kilocurie extends DefaultRadioactivityUnit("kilocurie", "kCi", Seq("KCi"), r"3.7e10" * r"1e3")
  final object megacurie extends DefaultRadioactivityUnit("megacurie", "MCi", Nil, r"3.7e10" * r"1e6")
  final object gigacurie extends DefaultRadioactivityUnit("gigacurie", "GCi", Nil, r"3.7e10" * r"1e9")
  final object teracurie extends DefaultRadioactivityUnit("teracurie", "TCi", Nil, r"3.7e10" * r"1e12")
  final object petacurie extends DefaultRadioactivityUnit("petacurie", "PCi", Nil, r"3.7e10" * r"1e15")
  final object exacurie extends DefaultRadioactivityUnit("exacurie", "ECi", Nil, r"3.7e10" * r"1e18")
  final object zettacurie extends DefaultRadioactivityUnit("zettacurie", "ZCi", Nil, r"3.7e10" * r"1e21")
  final object yottacurie extends DefaultRadioactivityUnit("yottacurie", "YCi", Nil, r"3.7e10" * r"1e24")
  final object rutherford extends DefaultRadioactivityUnit("rutherford", "Rd", Nil, r"1" * megabecquerel.interval)

  def getUnits: Seq[RadioactivityUnit] =
    Seq(becquerel, yoctobecquerel, zeptobecquerel, attobecquerel, femtobecquerel, picobecquerel, nanobecquerel, microbecquerel, millibecquerel, centibecquerel, decibecquerel, decabecquerel, hectobecquerel, kilobecquerel, megabecquerel, gigabecquerel, terabecquerel, petabecquerel, exabecquerel, zettabecquerel, yottabecquerel, curie, yoctocurie, zeptocurie, attocurie, femtocurie, picocurie, nanocurie, microcurie, millicurie, centicurie, decicurie, decacurie, hectocurie, kilocurie, megacurie, gigacurie, teracurie, petacurie, exacurie, zettacurie, yottacurie, rutherford)
}


object RadioactivityUnits{
  def Bq: RadioactivityUnit = RadioactivityUnitObjects.becquerel
  def yBq: RadioactivityUnit = RadioactivityUnitObjects.yoctobecquerel
  def zBq: RadioactivityUnit = RadioactivityUnitObjects.zeptobecquerel
  def aBq: RadioactivityUnit = RadioactivityUnitObjects.attobecquerel
  def fBq: RadioactivityUnit = RadioactivityUnitObjects.femtobecquerel
  def pBq: RadioactivityUnit = RadioactivityUnitObjects.picobecquerel
  def nBq: RadioactivityUnit = RadioactivityUnitObjects.nanobecquerel
  def μBq: RadioactivityUnit = RadioactivityUnitObjects.microbecquerel
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
  def μCi: RadioactivityUnit = RadioactivityUnitObjects.microcurie
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

  def getSIUnit: RadioactivityUnit = RadioactivityUnitObjects.getSIUnit
  def getUnits: Seq[RadioactivityUnit] = RadioactivityUnitObjects.getUnits
}
