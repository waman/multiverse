package org.waman.multiverse.unit.radiation

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.basic.Time
import org.waman.multiverse.unit.basic.TimeUnit

class EquivalentDose[A: Fractional](val value: A, val unit: EquivalentDoseUnit)
    extends LinearQuantity[EquivalentDose[A], A, EquivalentDoseUnit] {

  override protected def newQuantity(value: A, unit: EquivalentDoseUnit): EquivalentDose[A] = new EquivalentDose(value, unit)
           
  def /(time: Time[A]): EquivalentDoseRate[A] = new EquivalentDoseRate(this.value / time.value, this.unit / time.unit)

}

trait EquivalentDoseUnit extends LinearUnit[EquivalentDoseUnit]{
  override def getSIUnit: EquivalentDoseUnit = EquivalentDoseUnitObjects.getSIUnit


  def /(timeUnit: TimeUnit): EquivalentDoseRateUnit =
    new QuotientUnit[EquivalentDoseRateUnit, EquivalentDoseUnit, TimeUnit](EquivalentDoseUnit.this, timeUnit) with EquivalentDoseRateUnit
}

class DefaultEquivalentDoseUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends EquivalentDoseUnit


object EquivalentDoseUnitObjects{

  def getSIUnit: EquivalentDoseUnit = sievert

  final object sievert extends DefaultEquivalentDoseUnit("sievert", "Sv", Nil, r"1")
  final object yoctosievert extends DefaultEquivalentDoseUnit("yoctosievert", "ySv", Nil, r"1" * r"1e-24")
  final object zeptosievert extends DefaultEquivalentDoseUnit("zeptosievert", "zSv", Nil, r"1" * r"1e-21")
  final object attosievert extends DefaultEquivalentDoseUnit("attosievert", "aSv", Nil, r"1" * r"1e-18")
  final object femtosievert extends DefaultEquivalentDoseUnit("femtosievert", "fSv", Nil, r"1" * r"1e-15")
  final object picosievert extends DefaultEquivalentDoseUnit("picosievert", "pSv", Nil, r"1" * r"1e-12")
  final object nanosievert extends DefaultEquivalentDoseUnit("nanosievert", "nSv", Nil, r"1" * r"1e-9")
  final object microsievert extends DefaultEquivalentDoseUnit("microsievert", "μSv", Seq("mcSv"), r"1" * r"1e-6")
  final object millisievert extends DefaultEquivalentDoseUnit("millisievert", "mSv", Nil, r"1" * r"1e-3")
  final object centisievert extends DefaultEquivalentDoseUnit("centisievert", "cSv", Nil, r"1" * r"1e-2")
  final object decisievert extends DefaultEquivalentDoseUnit("decisievert", "dSv", Nil, r"1" * r"1e-1")
  final object decasievert extends DefaultEquivalentDoseUnit("decasievert", "daSv", Nil, r"1" * r"1e1")
  final object hectosievert extends DefaultEquivalentDoseUnit("hectosievert", "hSv", Nil, r"1" * r"1e2")
  final object kilosievert extends DefaultEquivalentDoseUnit("kilosievert", "kSv", Seq("KSv"), r"1" * r"1e3")
  final object megasievert extends DefaultEquivalentDoseUnit("megasievert", "MSv", Nil, r"1" * r"1e6")
  final object gigasievert extends DefaultEquivalentDoseUnit("gigasievert", "GSv", Nil, r"1" * r"1e9")
  final object terasievert extends DefaultEquivalentDoseUnit("terasievert", "TSv", Nil, r"1" * r"1e12")
  final object petasievert extends DefaultEquivalentDoseUnit("petasievert", "PSv", Nil, r"1" * r"1e15")
  final object exasievert extends DefaultEquivalentDoseUnit("exasievert", "ESv", Nil, r"1" * r"1e18")
  final object zettasievert extends DefaultEquivalentDoseUnit("zettasievert", "ZSv", Nil, r"1" * r"1e21")
  final object yottasievert extends DefaultEquivalentDoseUnit("yottasievert", "YSv", Nil, r"1" * r"1e24")
  final object rem extends DefaultEquivalentDoseUnit("rem", "rem", Nil, r"1e-2")
  final object yoctorem extends DefaultEquivalentDoseUnit("yoctorem", "yrem", Nil, r"1e-2" * r"1e-24")
  final object zeptorem extends DefaultEquivalentDoseUnit("zeptorem", "zrem", Nil, r"1e-2" * r"1e-21")
  final object attorem extends DefaultEquivalentDoseUnit("attorem", "arem", Nil, r"1e-2" * r"1e-18")
  final object femtorem extends DefaultEquivalentDoseUnit("femtorem", "frem", Nil, r"1e-2" * r"1e-15")
  final object picorem extends DefaultEquivalentDoseUnit("picorem", "prem", Nil, r"1e-2" * r"1e-12")
  final object nanorem extends DefaultEquivalentDoseUnit("nanorem", "nrem", Nil, r"1e-2" * r"1e-9")
  final object microrem extends DefaultEquivalentDoseUnit("microrem", "μrem", Seq("mcrem"), r"1e-2" * r"1e-6")
  final object millirem extends DefaultEquivalentDoseUnit("millirem", "mrem", Nil, r"1e-2" * r"1e-3")
  final object centirem extends DefaultEquivalentDoseUnit("centirem", "crem", Nil, r"1e-2" * r"1e-2")
  final object decirem extends DefaultEquivalentDoseUnit("decirem", "drem", Nil, r"1e-2" * r"1e-1")
  final object decarem extends DefaultEquivalentDoseUnit("decarem", "darem", Nil, r"1e-2" * r"1e1")
  final object hectorem extends DefaultEquivalentDoseUnit("hectorem", "hrem", Nil, r"1e-2" * r"1e2")
  final object kilorem extends DefaultEquivalentDoseUnit("kilorem", "krem", Seq("Krem"), r"1e-2" * r"1e3")
  final object megarem extends DefaultEquivalentDoseUnit("megarem", "Mrem", Nil, r"1e-2" * r"1e6")
  final object gigarem extends DefaultEquivalentDoseUnit("gigarem", "Grem", Nil, r"1e-2" * r"1e9")
  final object terarem extends DefaultEquivalentDoseUnit("terarem", "Trem", Nil, r"1e-2" * r"1e12")
  final object petarem extends DefaultEquivalentDoseUnit("petarem", "Prem", Nil, r"1e-2" * r"1e15")
  final object exarem extends DefaultEquivalentDoseUnit("exarem", "Erem", Nil, r"1e-2" * r"1e18")
  final object zettarem extends DefaultEquivalentDoseUnit("zettarem", "Zrem", Nil, r"1e-2" * r"1e21")
  final object yottarem extends DefaultEquivalentDoseUnit("yottarem", "Yrem", Nil, r"1e-2" * r"1e24")

  def getUnits: Seq[EquivalentDoseUnit] =
    Seq(sievert, yoctosievert, zeptosievert, attosievert, femtosievert, picosievert, nanosievert, microsievert, millisievert, centisievert, decisievert, decasievert, hectosievert, kilosievert, megasievert, gigasievert, terasievert, petasievert, exasievert, zettasievert, yottasievert, rem, yoctorem, zeptorem, attorem, femtorem, picorem, nanorem, microrem, millirem, centirem, decirem, decarem, hectorem, kilorem, megarem, gigarem, terarem, petarem, exarem, zettarem, yottarem)
}


object EquivalentDoseUnits{
  def Sv: EquivalentDoseUnit = EquivalentDoseUnitObjects.sievert
  def ySv: EquivalentDoseUnit = EquivalentDoseUnitObjects.yoctosievert
  def zSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.zeptosievert
  def aSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.attosievert
  def fSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.femtosievert
  def pSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.picosievert
  def nSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.nanosievert
  def μSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.microsievert
  def mcSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.microsievert
  def mSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.millisievert
  def cSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.centisievert
  def dSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.decisievert
  def daSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.decasievert
  def hSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.hectosievert
  def kSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.kilosievert
  def KSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.kilosievert
  def MSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.megasievert
  def GSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.gigasievert
  def TSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.terasievert
  def PSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.petasievert
  def ESv: EquivalentDoseUnit = EquivalentDoseUnitObjects.exasievert
  def ZSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.zettasievert
  def YSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.yottasievert
  def rem: EquivalentDoseUnit = EquivalentDoseUnitObjects.rem
  def yrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.yoctorem
  def zrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.zeptorem
  def arem: EquivalentDoseUnit = EquivalentDoseUnitObjects.attorem
  def frem: EquivalentDoseUnit = EquivalentDoseUnitObjects.femtorem
  def prem: EquivalentDoseUnit = EquivalentDoseUnitObjects.picorem
  def nrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.nanorem
  def μrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.microrem
  def mcrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.microrem
  def mrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.millirem
  def crem: EquivalentDoseUnit = EquivalentDoseUnitObjects.centirem
  def drem: EquivalentDoseUnit = EquivalentDoseUnitObjects.decirem
  def darem: EquivalentDoseUnit = EquivalentDoseUnitObjects.decarem
  def hrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.hectorem
  def krem: EquivalentDoseUnit = EquivalentDoseUnitObjects.kilorem
  def Krem: EquivalentDoseUnit = EquivalentDoseUnitObjects.kilorem
  def Mrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.megarem
  def Grem: EquivalentDoseUnit = EquivalentDoseUnitObjects.gigarem
  def Trem: EquivalentDoseUnit = EquivalentDoseUnitObjects.terarem
  def Prem: EquivalentDoseUnit = EquivalentDoseUnitObjects.petarem
  def Erem: EquivalentDoseUnit = EquivalentDoseUnitObjects.exarem
  def Zrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.zettarem
  def Yrem: EquivalentDoseUnit = EquivalentDoseUnitObjects.yottarem

  def getSIUnit: EquivalentDoseUnit = EquivalentDoseUnitObjects.getSIUnit
  def getUnits: Seq[EquivalentDoseUnit] = EquivalentDoseUnitObjects.getUnits
}
