package org.waman.multiverse.unit.defs.ra

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs._

class EquivalentDose[A: Fractional](val value: A, val unit: EquivalentDoseUnit)
    extends LinearQuantity[EquivalentDose[A], A, EquivalentDoseUnit] {

  override protected def newQuantity(value: A, unit: EquivalentDoseUnit): EquivalentDose[A] = new EquivalentDose(value, unit)

  def /(time: Time[A]): EquivalentDoseRate[A] = new EquivalentDoseRate(this.value / time.value, this.unit / time.unit)
}

/** None */
trait EquivalentDoseUnit extends LinearUnit[EquivalentDoseUnit]{

  override def getSIUnit: EquivalentDoseUnit = EquivalentDoseUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = EquivalentDoseUnit.dimension

  def /(timeUnit: TimeUnit): EquivalentDoseRateUnit =
    new QuotientUnit[EquivalentDoseRateUnit, EquivalentDoseUnit, TimeUnit](EquivalentDoseUnit.this, timeUnit) with EquivalentDoseRateUnit
}

object EquivalentDoseUnit extends UnitInfo[EquivalentDoseUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, L -> 2).withDefaultValue(0)

  def getSIUnit: EquivalentDoseUnit = EquivalentDoseUnitObjects.sievert

  import EquivalentDoseUnitObjects._

  def getUnits: Seq[EquivalentDoseUnit] =
    Seq(sievert, yoctosievert, zeptosievert, attosievert, femtosievert, picosievert, nanosievert, microsievert, millisievert, centisievert, decisievert, decasievert, hectosievert, kilosievert, megasievert, gigasievert, terasievert, petasievert, exasievert, zettasievert, yottasievert, roentgen_equivalent_man, yoctoroentgen_equivalent_man, zeptoroentgen_equivalent_man, attoroentgen_equivalent_man, femtoroentgen_equivalent_man, picoroentgen_equivalent_man, nanoroentgen_equivalent_man, microroentgen_equivalent_man, milliroentgen_equivalent_man, centiroentgen_equivalent_man, deciroentgen_equivalent_man, decaroentgen_equivalent_man, hectoroentgen_equivalent_man, kiloroentgen_equivalent_man, megaroentgen_equivalent_man, gigaroentgen_equivalent_man, teraroentgen_equivalent_man, petaroentgen_equivalent_man, exaroentgen_equivalent_man, zettaroentgen_equivalent_man, yottaroentgen_equivalent_man)
}


/** For no alias or user defined units */
class SimpleEquivalentDoseUnit(val name: String, val symbol: String, val interval: Real) extends EquivalentDoseUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultEquivalentDoseUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends EquivalentDoseUnit
  
object EquivalentDoseUnitObjects{

  final case object sievert extends SimpleEquivalentDoseUnit("sievert", "Sv", 1)
  final case object yoctosievert extends SimpleEquivalentDoseUnit("yoctosievert", "ySv", r"1e-24")
  final case object zeptosievert extends SimpleEquivalentDoseUnit("zeptosievert", "zSv", r"1e-21")
  final case object attosievert extends SimpleEquivalentDoseUnit("attosievert", "aSv", r"1e-18")
  final case object femtosievert extends SimpleEquivalentDoseUnit("femtosievert", "fSv", r"1e-15")
  final case object picosievert extends SimpleEquivalentDoseUnit("picosievert", "pSv", r"1e-12")
  final case object nanosievert extends SimpleEquivalentDoseUnit("nanosievert", "nSv", r"1e-9")
  final case object microsievert extends DefaultEquivalentDoseUnit("microsievert", "μSv", Seq("mcSv"), r"1e-6")
  final case object millisievert extends SimpleEquivalentDoseUnit("millisievert", "mSv", r"1e-3")
  final case object centisievert extends SimpleEquivalentDoseUnit("centisievert", "cSv", r"1e-2")
  final case object decisievert extends SimpleEquivalentDoseUnit("decisievert", "dSv", r"1e-1")
  final case object decasievert extends SimpleEquivalentDoseUnit("decasievert", "daSv", r"1e1")
  final case object hectosievert extends SimpleEquivalentDoseUnit("hectosievert", "hSv", r"1e2")
  final case object kilosievert extends DefaultEquivalentDoseUnit("kilosievert", "kSv", Seq("KSv"), r"1e3")
  final case object megasievert extends SimpleEquivalentDoseUnit("megasievert", "MSv", r"1e6")
  final case object gigasievert extends SimpleEquivalentDoseUnit("gigasievert", "GSv", r"1e9")
  final case object terasievert extends SimpleEquivalentDoseUnit("terasievert", "TSv", r"1e12")
  final case object petasievert extends SimpleEquivalentDoseUnit("petasievert", "PSv", r"1e15")
  final case object exasievert extends SimpleEquivalentDoseUnit("exasievert", "ESv", r"1e18")
  final case object zettasievert extends SimpleEquivalentDoseUnit("zettasievert", "ZSv", r"1e21")
  final case object yottasievert extends SimpleEquivalentDoseUnit("yottasievert", "YSv", r"1e24")
  final case object roentgen_equivalent_man extends SimpleEquivalentDoseUnit("roentgen equivalent man", "rem", r"1e-2")
  final case object yoctoroentgen_equivalent_man extends SimpleEquivalentDoseUnit("yoctoroentgen equivalent man", "yrem", r"1e-2" * r"1e-24")
  final case object zeptoroentgen_equivalent_man extends SimpleEquivalentDoseUnit("zeptoroentgen equivalent man", "zrem", r"1e-2" * r"1e-21")
  final case object attoroentgen_equivalent_man extends SimpleEquivalentDoseUnit("attoroentgen equivalent man", "arem", r"1e-2" * r"1e-18")
  final case object femtoroentgen_equivalent_man extends SimpleEquivalentDoseUnit("femtoroentgen equivalent man", "frem", r"1e-2" * r"1e-15")
  final case object picoroentgen_equivalent_man extends SimpleEquivalentDoseUnit("picoroentgen equivalent man", "prem", r"1e-2" * r"1e-12")
  final case object nanoroentgen_equivalent_man extends SimpleEquivalentDoseUnit("nanoroentgen equivalent man", "nrem", r"1e-2" * r"1e-9")
  final case object microroentgen_equivalent_man extends DefaultEquivalentDoseUnit("microroentgen equivalent man", "μrem", Seq("mcrem"), r"1e-2" * r"1e-6")
  final case object milliroentgen_equivalent_man extends SimpleEquivalentDoseUnit("milliroentgen equivalent man", "mrem", r"1e-2" * r"1e-3")
  final case object centiroentgen_equivalent_man extends SimpleEquivalentDoseUnit("centiroentgen equivalent man", "crem", r"1e-2" * r"1e-2")
  final case object deciroentgen_equivalent_man extends SimpleEquivalentDoseUnit("deciroentgen equivalent man", "drem", r"1e-2" * r"1e-1")
  final case object decaroentgen_equivalent_man extends SimpleEquivalentDoseUnit("decaroentgen equivalent man", "darem", r"1e-2" * r"1e1")
  final case object hectoroentgen_equivalent_man extends SimpleEquivalentDoseUnit("hectoroentgen equivalent man", "hrem", r"1e-2" * r"1e2")
  final case object kiloroentgen_equivalent_man extends DefaultEquivalentDoseUnit("kiloroentgen equivalent man", "krem", Seq("Krem"), r"1e-2" * r"1e3")
  final case object megaroentgen_equivalent_man extends SimpleEquivalentDoseUnit("megaroentgen equivalent man", "Mrem", r"1e-2" * r"1e6")
  final case object gigaroentgen_equivalent_man extends SimpleEquivalentDoseUnit("gigaroentgen equivalent man", "Grem", r"1e-2" * r"1e9")
  final case object teraroentgen_equivalent_man extends SimpleEquivalentDoseUnit("teraroentgen equivalent man", "Trem", r"1e-2" * r"1e12")
  final case object petaroentgen_equivalent_man extends SimpleEquivalentDoseUnit("petaroentgen equivalent man", "Prem", r"1e-2" * r"1e15")
  final case object exaroentgen_equivalent_man extends SimpleEquivalentDoseUnit("exaroentgen equivalent man", "Erem", r"1e-2" * r"1e18")
  final case object zettaroentgen_equivalent_man extends SimpleEquivalentDoseUnit("zettaroentgen equivalent man", "Zrem", r"1e-2" * r"1e21")
  final case object yottaroentgen_equivalent_man extends SimpleEquivalentDoseUnit("yottaroentgen equivalent man", "Yrem", r"1e-2" * r"1e24")
}


object EquivalentDoseUnits{

  /** sievert */
  def Sv: EquivalentDoseUnit = EquivalentDoseUnitObjects.sievert
  /** yoctosievert */
  def ySv: EquivalentDoseUnit = EquivalentDoseUnitObjects.yoctosievert
  /** zeptosievert */
  def zSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.zeptosievert
  /** attosievert */
  def aSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.attosievert
  /** femtosievert */
  def fSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.femtosievert
  /** picosievert */
  def pSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.picosievert
  /** nanosievert */
  def nSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.nanosievert
  /** microsievert */
  def μSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.microsievert
  /** microsievert */
  def mcSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.microsievert
  /** millisievert */
  def mSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.millisievert
  /** centisievert */
  def cSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.centisievert
  /** decisievert */
  def dSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.decisievert
  /** decasievert */
  def daSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.decasievert
  /** hectosievert */
  def hSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.hectosievert
  /** kilosievert */
  def kSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.kilosievert
  /** kilosievert */
  def KSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.kilosievert
  /** megasievert */
  def MSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.megasievert
  /** gigasievert */
  def GSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.gigasievert
  /** terasievert */
  def TSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.terasievert
  /** petasievert */
  def PSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.petasievert
  /** exasievert */
  def ESv: EquivalentDoseUnit = EquivalentDoseUnitObjects.exasievert
  /** zettasievert */
  def ZSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.zettasievert
  /** yottasievert */
  def YSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.yottasievert
  /** roentgen equivalent man */
  def rem: EquivalentDoseUnit = EquivalentDoseUnitObjects.roentgen_equivalent_man
  /** yoctoroentgen equivalent man */
  def yrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.yoctoroentgen_equivalent_man
  /** zeptoroentgen equivalent man */
  def zrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.zeptoroentgen_equivalent_man
  /** attoroentgen equivalent man */
  def arem: EquivalentDoseUnit = EquivalentDoseUnitObjects.attoroentgen_equivalent_man
  /** femtoroentgen equivalent man */
  def frem: EquivalentDoseUnit = EquivalentDoseUnitObjects.femtoroentgen_equivalent_man
  /** picoroentgen equivalent man */
  def prem: EquivalentDoseUnit = EquivalentDoseUnitObjects.picoroentgen_equivalent_man
  /** nanoroentgen equivalent man */
  def nrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.nanoroentgen_equivalent_man
  /** microroentgen equivalent man */
  def μrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.microroentgen_equivalent_man
  /** microroentgen equivalent man */
  def mcrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.microroentgen_equivalent_man
  /** milliroentgen equivalent man */
  def mrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.milliroentgen_equivalent_man
  /** centiroentgen equivalent man */
  def crem: EquivalentDoseUnit = EquivalentDoseUnitObjects.centiroentgen_equivalent_man
  /** deciroentgen equivalent man */
  def drem: EquivalentDoseUnit = EquivalentDoseUnitObjects.deciroentgen_equivalent_man
  /** decaroentgen equivalent man */
  def darem: EquivalentDoseUnit = EquivalentDoseUnitObjects.decaroentgen_equivalent_man
  /** hectoroentgen equivalent man */
  def hrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.hectoroentgen_equivalent_man
  /** kiloroentgen equivalent man */
  def krem: EquivalentDoseUnit = EquivalentDoseUnitObjects.kiloroentgen_equivalent_man
  /** kiloroentgen equivalent man */
  def Krem: EquivalentDoseUnit = EquivalentDoseUnitObjects.kiloroentgen_equivalent_man
  /** megaroentgen equivalent man */
  def Mrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.megaroentgen_equivalent_man
  /** gigaroentgen equivalent man */
  def Grem: EquivalentDoseUnit = EquivalentDoseUnitObjects.gigaroentgen_equivalent_man
  /** teraroentgen equivalent man */
  def Trem: EquivalentDoseUnit = EquivalentDoseUnitObjects.teraroentgen_equivalent_man
  /** petaroentgen equivalent man */
  def Prem: EquivalentDoseUnit = EquivalentDoseUnitObjects.petaroentgen_equivalent_man
  /** exaroentgen equivalent man */
  def Erem: EquivalentDoseUnit = EquivalentDoseUnitObjects.exaroentgen_equivalent_man
  /** zettaroentgen equivalent man */
  def Zrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.zettaroentgen_equivalent_man
  /** yottaroentgen equivalent man */
  def Yrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.yottaroentgen_equivalent_man
}