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

  override def getSIUnit: EquivalentDoseUnit = EquivalentDoseUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = EquivalentDoseUnit.dimension

  def /(timeUnit: TimeUnit): EquivalentDoseRateUnit =
    new AbstractQuotientUnit[EquivalentDoseRateUnit, EquivalentDoseUnit, TimeUnit](EquivalentDoseUnit.this, timeUnit) with EquivalentDoseRateUnit
}

object EquivalentDoseUnit extends UnitInfo[EquivalentDoseUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, L -> 2).withDefaultValue(0)

  def getSIUnit: EquivalentDoseUnit = EquivalentDoseUnitObjects.sievert

  import EquivalentDoseUnitObjects._
  def getUnits: Seq[EquivalentDoseUnit] =
    Seq(sievert, yoctosievert, zeptosievert, attosievert, femtosievert, picosievert, nanosievert, microsievert, millisievert, centisievert, decisievert, decasievert, hectosievert, kilosievert, megasievert, gigasievert, terasievert, petasievert, exasievert, zettasievert, yottasievert, rem, yoctorem, zeptorem, attorem, femtorem, picorem, nanorem, microrem, millirem, centirem, decirem, decarem, hectorem, kilorem, megarem, gigarem, terarem, petarem, exarem, zettarem, yottarem)
}

/** For no aliase or user defined units */
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
  final case object rem extends SimpleEquivalentDoseUnit("rem", "rem", r"1e-2")
  final case object yoctorem extends SimpleEquivalentDoseUnit("yoctorem", "yrem", r"1e-2" * r"1e-24")
  final case object zeptorem extends SimpleEquivalentDoseUnit("zeptorem", "zrem", r"1e-2" * r"1e-21")
  final case object attorem extends SimpleEquivalentDoseUnit("attorem", "arem", r"1e-2" * r"1e-18")
  final case object femtorem extends SimpleEquivalentDoseUnit("femtorem", "frem", r"1e-2" * r"1e-15")
  final case object picorem extends SimpleEquivalentDoseUnit("picorem", "prem", r"1e-2" * r"1e-12")
  final case object nanorem extends SimpleEquivalentDoseUnit("nanorem", "nrem", r"1e-2" * r"1e-9")
  final case object microrem extends DefaultEquivalentDoseUnit("microrem", "μrem", Seq("mcrem"), r"1e-2" * r"1e-6")
  final case object millirem extends SimpleEquivalentDoseUnit("millirem", "mrem", r"1e-2" * r"1e-3")
  final case object centirem extends SimpleEquivalentDoseUnit("centirem", "crem", r"1e-2" * r"1e-2")
  final case object decirem extends SimpleEquivalentDoseUnit("decirem", "drem", r"1e-2" * r"1e-1")
  final case object decarem extends SimpleEquivalentDoseUnit("decarem", "darem", r"1e-2" * r"1e1")
  final case object hectorem extends SimpleEquivalentDoseUnit("hectorem", "hrem", r"1e-2" * r"1e2")
  final case object kilorem extends DefaultEquivalentDoseUnit("kilorem", "krem", Seq("Krem"), r"1e-2" * r"1e3")
  final case object megarem extends SimpleEquivalentDoseUnit("megarem", "Mrem", r"1e-2" * r"1e6")
  final case object gigarem extends SimpleEquivalentDoseUnit("gigarem", "Grem", r"1e-2" * r"1e9")
  final case object terarem extends SimpleEquivalentDoseUnit("terarem", "Trem", r"1e-2" * r"1e12")
  final case object petarem extends SimpleEquivalentDoseUnit("petarem", "Prem", r"1e-2" * r"1e15")
  final case object exarem extends SimpleEquivalentDoseUnit("exarem", "Erem", r"1e-2" * r"1e18")
  final case object zettarem extends SimpleEquivalentDoseUnit("zettarem", "Zrem", r"1e-2" * r"1e21")
  final case object yottarem extends SimpleEquivalentDoseUnit("yottarem", "Yrem", r"1e-2" * r"1e24")
}

object EquivalentDoseUnits{
  def Sv: EquivalentDoseUnit = EquivalentDoseUnitObjects.sievert
  def ySv: EquivalentDoseUnit = EquivalentDoseUnitObjects.yoctosievert
  def zSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.zeptosievert
  def aSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.attosievert
  def fSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.femtosievert
  def pSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.picosievert
  def nSv: EquivalentDoseUnit = EquivalentDoseUnitObjects.nanosievert
  def `μSv`: EquivalentDoseUnit = EquivalentDoseUnitObjects.microsievert
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
  def `μrem`: EquivalentDoseUnit = EquivalentDoseUnitObjects.microrem
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
}