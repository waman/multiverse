package org.waman.multiverse.unit.defs.ra

import spire.math._
import spire.implicits._

import org.waman.multiverse._

class Radioactivity[A: Fractional](val value: A, val unit: RadioactivityUnit)
    extends LinearQuantity[Radioactivity[A], A, RadioactivityUnit] {

  override protected def newQuantity(value: A, unit: RadioactivityUnit): Radioactivity[A] = new Radioactivity(value, unit)
}

/** None */
trait RadioactivityUnit extends LinearUnit[RadioactivityUnit]{

  override def getSIUnit: RadioactivityUnit = RadioactivityUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = RadioactivityUnit.dimension
}

object RadioactivityUnit extends UnitInfo[RadioactivityUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -1).withDefaultValue(0)

  def getSIUnit: RadioactivityUnit = RadioactivityUnitObjects.becquerel

  import RadioactivityUnitObjects._

  def getUnits: Seq[RadioactivityUnit] =
    Seq(becquerel, yoctobecquerel, zeptobecquerel, attobecquerel, femtobecquerel, picobecquerel, nanobecquerel, microbecquerel, millibecquerel, centibecquerel, decibecquerel, decabecquerel, hectobecquerel, kilobecquerel, megabecquerel, gigabecquerel, terabecquerel, petabecquerel, exabecquerel, zettabecquerel, yottabecquerel, curie, yoctocurie, zeptocurie, attocurie, femtocurie, picocurie, nanocurie, microcurie, millicurie, centicurie, decicurie, decacurie, hectocurie, kilocurie, megacurie, gigacurie, teracurie, petacurie, exacurie, zettacurie, yottacurie, rutherford)
}


/** For no alias or user defined units */
class SimpleRadioactivityUnit(val name: String, val symbol: String, val interval: Real) extends RadioactivityUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultRadioactivityUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends RadioactivityUnit
  
object RadioactivityUnitObjects{

  final case object becquerel extends SimpleRadioactivityUnit("becquerel", "Bq", 1)
  final case object yoctobecquerel extends SimpleRadioactivityUnit("yoctobecquerel", "yBq", r"1e-24")
  final case object zeptobecquerel extends SimpleRadioactivityUnit("zeptobecquerel", "zBq", r"1e-21")
  final case object attobecquerel extends SimpleRadioactivityUnit("attobecquerel", "aBq", r"1e-18")
  final case object femtobecquerel extends SimpleRadioactivityUnit("femtobecquerel", "fBq", r"1e-15")
  final case object picobecquerel extends SimpleRadioactivityUnit("picobecquerel", "pBq", r"1e-12")
  final case object nanobecquerel extends SimpleRadioactivityUnit("nanobecquerel", "nBq", r"1e-9")
  final case object microbecquerel extends DefaultRadioactivityUnit("microbecquerel", "μBq", Seq("mcBq"), r"1e-6")
  final case object millibecquerel extends SimpleRadioactivityUnit("millibecquerel", "mBq", r"1e-3")
  final case object centibecquerel extends SimpleRadioactivityUnit("centibecquerel", "cBq", r"1e-2")
  final case object decibecquerel extends SimpleRadioactivityUnit("decibecquerel", "dBq", r"1e-1")
  final case object decabecquerel extends SimpleRadioactivityUnit("decabecquerel", "daBq", r"1e1")
  final case object hectobecquerel extends SimpleRadioactivityUnit("hectobecquerel", "hBq", r"1e2")
  final case object kilobecquerel extends DefaultRadioactivityUnit("kilobecquerel", "kBq", Seq("KBq"), r"1e3")
  final case object megabecquerel extends SimpleRadioactivityUnit("megabecquerel", "MBq", r"1e6")
  final case object gigabecquerel extends SimpleRadioactivityUnit("gigabecquerel", "GBq", r"1e9")
  final case object terabecquerel extends SimpleRadioactivityUnit("terabecquerel", "TBq", r"1e12")
  final case object petabecquerel extends SimpleRadioactivityUnit("petabecquerel", "PBq", r"1e15")
  final case object exabecquerel extends SimpleRadioactivityUnit("exabecquerel", "EBq", r"1e18")
  final case object zettabecquerel extends SimpleRadioactivityUnit("zettabecquerel", "ZBq", r"1e21")
  final case object yottabecquerel extends SimpleRadioactivityUnit("yottabecquerel", "YBq", r"1e24")
  final case object curie extends SimpleRadioactivityUnit("curie", "Ci", r"3.7e10")
  final case object yoctocurie extends SimpleRadioactivityUnit("yoctocurie", "yCi", r"3.7e10" * r"1e-24")
  final case object zeptocurie extends SimpleRadioactivityUnit("zeptocurie", "zCi", r"3.7e10" * r"1e-21")
  final case object attocurie extends SimpleRadioactivityUnit("attocurie", "aCi", r"3.7e10" * r"1e-18")
  final case object femtocurie extends SimpleRadioactivityUnit("femtocurie", "fCi", r"3.7e10" * r"1e-15")
  final case object picocurie extends SimpleRadioactivityUnit("picocurie", "pCi", r"3.7e10" * r"1e-12")
  final case object nanocurie extends SimpleRadioactivityUnit("nanocurie", "nCi", r"3.7e10" * r"1e-9")
  final case object microcurie extends DefaultRadioactivityUnit("microcurie", "μCi", Seq("mcCi"), r"3.7e10" * r"1e-6")
  final case object millicurie extends SimpleRadioactivityUnit("millicurie", "mCi", r"3.7e10" * r"1e-3")
  final case object centicurie extends SimpleRadioactivityUnit("centicurie", "cCi", r"3.7e10" * r"1e-2")
  final case object decicurie extends SimpleRadioactivityUnit("decicurie", "dCi", r"3.7e10" * r"1e-1")
  final case object decacurie extends SimpleRadioactivityUnit("decacurie", "daCi", r"3.7e10" * r"1e1")
  final case object hectocurie extends SimpleRadioactivityUnit("hectocurie", "hCi", r"3.7e10" * r"1e2")
  final case object kilocurie extends DefaultRadioactivityUnit("kilocurie", "kCi", Seq("KCi"), r"3.7e10" * r"1e3")
  final case object megacurie extends SimpleRadioactivityUnit("megacurie", "MCi", r"3.7e10" * r"1e6")
  final case object gigacurie extends SimpleRadioactivityUnit("gigacurie", "GCi", r"3.7e10" * r"1e9")
  final case object teracurie extends SimpleRadioactivityUnit("teracurie", "TCi", r"3.7e10" * r"1e12")
  final case object petacurie extends SimpleRadioactivityUnit("petacurie", "PCi", r"3.7e10" * r"1e15")
  final case object exacurie extends SimpleRadioactivityUnit("exacurie", "ECi", r"3.7e10" * r"1e18")
  final case object zettacurie extends SimpleRadioactivityUnit("zettacurie", "ZCi", r"3.7e10" * r"1e21")
  final case object yottacurie extends SimpleRadioactivityUnit("yottacurie", "YCi", r"3.7e10" * r"1e24")
  final case object rutherford extends SimpleRadioactivityUnit("rutherford", "Rd", megabecquerel.interval)
}


object RadioactivityUnits{

  /** becquerel */
  def Bq: RadioactivityUnit = RadioactivityUnitObjects.becquerel
  /** yoctobecquerel */
  def yBq: RadioactivityUnit = RadioactivityUnitObjects.yoctobecquerel
  /** zeptobecquerel */
  def zBq: RadioactivityUnit = RadioactivityUnitObjects.zeptobecquerel
  /** attobecquerel */
  def aBq: RadioactivityUnit = RadioactivityUnitObjects.attobecquerel
  /** femtobecquerel */
  def fBq: RadioactivityUnit = RadioactivityUnitObjects.femtobecquerel
  /** picobecquerel */
  def pBq: RadioactivityUnit = RadioactivityUnitObjects.picobecquerel
  /** nanobecquerel */
  def nBq: RadioactivityUnit = RadioactivityUnitObjects.nanobecquerel
  /** microbecquerel */
  def μBq: RadioactivityUnit = RadioactivityUnitObjects.microbecquerel
  /** microbecquerel */
  def mcBq: RadioactivityUnit = RadioactivityUnitObjects.microbecquerel
  /** millibecquerel */
  def mBq: RadioactivityUnit = RadioactivityUnitObjects.millibecquerel
  /** centibecquerel */
  def cBq: RadioactivityUnit = RadioactivityUnitObjects.centibecquerel
  /** decibecquerel */
  def dBq: RadioactivityUnit = RadioactivityUnitObjects.decibecquerel
  /** decabecquerel */
  def daBq: RadioactivityUnit = RadioactivityUnitObjects.decabecquerel
  /** hectobecquerel */
  def hBq: RadioactivityUnit = RadioactivityUnitObjects.hectobecquerel
  /** kilobecquerel */
  def kBq: RadioactivityUnit = RadioactivityUnitObjects.kilobecquerel
  /** kilobecquerel */
  def KBq: RadioactivityUnit = RadioactivityUnitObjects.kilobecquerel
  /** megabecquerel */
  def MBq: RadioactivityUnit = RadioactivityUnitObjects.megabecquerel
  /** gigabecquerel */
  def GBq: RadioactivityUnit = RadioactivityUnitObjects.gigabecquerel
  /** terabecquerel */
  def TBq: RadioactivityUnit = RadioactivityUnitObjects.terabecquerel
  /** petabecquerel */
  def PBq: RadioactivityUnit = RadioactivityUnitObjects.petabecquerel
  /** exabecquerel */
  def EBq: RadioactivityUnit = RadioactivityUnitObjects.exabecquerel
  /** zettabecquerel */
  def ZBq: RadioactivityUnit = RadioactivityUnitObjects.zettabecquerel
  /** yottabecquerel */
  def YBq: RadioactivityUnit = RadioactivityUnitObjects.yottabecquerel
  /** curie */
  def Ci: RadioactivityUnit = RadioactivityUnitObjects.curie
  /** yoctocurie */
  def yCi: RadioactivityUnit = RadioactivityUnitObjects.yoctocurie
  /** zeptocurie */
  def zCi: RadioactivityUnit = RadioactivityUnitObjects.zeptocurie
  /** attocurie */
  def aCi: RadioactivityUnit = RadioactivityUnitObjects.attocurie
  /** femtocurie */
  def fCi: RadioactivityUnit = RadioactivityUnitObjects.femtocurie
  /** picocurie */
  def pCi: RadioactivityUnit = RadioactivityUnitObjects.picocurie
  /** nanocurie */
  def nCi: RadioactivityUnit = RadioactivityUnitObjects.nanocurie
  /** microcurie */
  def μCi: RadioactivityUnit = RadioactivityUnitObjects.microcurie
  /** microcurie */
  def mcCi: RadioactivityUnit = RadioactivityUnitObjects.microcurie
  /** millicurie */
  def mCi: RadioactivityUnit = RadioactivityUnitObjects.millicurie
  /** centicurie */
  def cCi: RadioactivityUnit = RadioactivityUnitObjects.centicurie
  /** decicurie */
  def dCi: RadioactivityUnit = RadioactivityUnitObjects.decicurie
  /** decacurie */
  def daCi: RadioactivityUnit = RadioactivityUnitObjects.decacurie
  /** hectocurie */
  def hCi: RadioactivityUnit = RadioactivityUnitObjects.hectocurie
  /** kilocurie */
  def kCi: RadioactivityUnit = RadioactivityUnitObjects.kilocurie
  /** kilocurie */
  def KCi: RadioactivityUnit = RadioactivityUnitObjects.kilocurie
  /** megacurie */
  def MCi: RadioactivityUnit = RadioactivityUnitObjects.megacurie
  /** gigacurie */
  def GCi: RadioactivityUnit = RadioactivityUnitObjects.gigacurie
  /** teracurie */
  def TCi: RadioactivityUnit = RadioactivityUnitObjects.teracurie
  /** petacurie */
  def PCi: RadioactivityUnit = RadioactivityUnitObjects.petacurie
  /** exacurie */
  def ECi: RadioactivityUnit = RadioactivityUnitObjects.exacurie
  /** zettacurie */
  def ZCi: RadioactivityUnit = RadioactivityUnitObjects.zettacurie
  /** yottacurie */
  def YCi: RadioactivityUnit = RadioactivityUnitObjects.yottacurie
  /** rutherford */
  def Rd: RadioactivityUnit = RadioactivityUnitObjects.rutherford
}