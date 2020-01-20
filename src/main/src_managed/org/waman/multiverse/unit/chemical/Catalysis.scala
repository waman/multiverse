package org.waman.multiverse.unit.chemical

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


class Catalysis[A: Fractional](val value: A, val unit: CatalysisUnit)
    extends LinearQuantity[Catalysis[A], A, CatalysisUnit] {

  override protected def newQuantity(value: A, unit: CatalysisUnit): Catalysis[A] = new Catalysis(value, unit)

}

trait CatalysisUnit extends LinearUnit[CatalysisUnit]{

  override def getSIUnit: CatalysisUnit = CatalysisUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = CatalysisUnit.dimension

}

/** For user defined units */
class SimpleCatalysisUnit(val name: String, val symbol: String, val interval: Real) extends CatalysisUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultCatalysisUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends CatalysisUnit

object CatalysisUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](N -> 1, T -> -1).withDefaultValue(0)

  def getSIUnit: CatalysisUnit = CatalysisUnitObjects.katal

  import CatalysisUnitObjects._
  def getUnits: Seq[CatalysisUnit] =
    Seq(katal, yoctokatal, zeptokatal, attokatal, femtokatal, picokatal, nanokatal, microkatal, millikatal, centikatal, decikatal, decakatal, hectokatal, kilokatal, megakatal, gigakatal, terakatal, petakatal, exakatal, zettakatal, yottakatal, enzyme_unit)
}

object CatalysisUnitObjects{
  import org.waman.multiverse.unit.basic.TimeUnitObjects

  final case object katal extends DefaultCatalysisUnit("katal", "kat", Nil, 1)
  final case object yoctokatal extends DefaultCatalysisUnit("yoctokatal", "ykat", Nil, r"1e-24")
  final case object zeptokatal extends DefaultCatalysisUnit("zeptokatal", "zkat", Nil, r"1e-21")
  final case object attokatal extends DefaultCatalysisUnit("attokatal", "akat", Nil, r"1e-18")
  final case object femtokatal extends DefaultCatalysisUnit("femtokatal", "fkat", Nil, r"1e-15")
  final case object picokatal extends DefaultCatalysisUnit("picokatal", "pkat", Nil, r"1e-12")
  final case object nanokatal extends DefaultCatalysisUnit("nanokatal", "nkat", Nil, r"1e-9")
  final case object microkatal extends DefaultCatalysisUnit("microkatal", "μkat", Seq("mckat"), r"1e-6")
  final case object millikatal extends DefaultCatalysisUnit("millikatal", "mkat", Nil, r"1e-3")
  final case object centikatal extends DefaultCatalysisUnit("centikatal", "ckat", Nil, r"1e-2")
  final case object decikatal extends DefaultCatalysisUnit("decikatal", "dkat", Nil, r"1e-1")
  final case object decakatal extends DefaultCatalysisUnit("decakatal", "dakat", Nil, r"1e1")
  final case object hectokatal extends DefaultCatalysisUnit("hectokatal", "hkat", Nil, r"1e2")
  final case object kilokatal extends DefaultCatalysisUnit("kilokatal", "kkat", Seq("Kkat"), r"1e3")
  final case object megakatal extends DefaultCatalysisUnit("megakatal", "Mkat", Nil, r"1e6")
  final case object gigakatal extends DefaultCatalysisUnit("gigakatal", "Gkat", Nil, r"1e9")
  final case object terakatal extends DefaultCatalysisUnit("terakatal", "Tkat", Nil, r"1e12")
  final case object petakatal extends DefaultCatalysisUnit("petakatal", "Pkat", Nil, r"1e15")
  final case object exakatal extends DefaultCatalysisUnit("exakatal", "Ekat", Nil, r"1e18")
  final case object zettakatal extends DefaultCatalysisUnit("zettakatal", "Zkat", Nil, r"1e21")
  final case object yottakatal extends DefaultCatalysisUnit("yottakatal", "Ykat", Nil, r"1e24")
  final case object enzyme_unit extends DefaultCatalysisUnit("enzyme unit", "U", Seq("IU"), AmountOfSubstanceUnitObjects.micromole.interval / TimeUnitObjects.minute.interval)
}

object CatalysisUnits{
  def kat: CatalysisUnit = CatalysisUnitObjects.katal
  def ykat: CatalysisUnit = CatalysisUnitObjects.yoctokatal
  def zkat: CatalysisUnit = CatalysisUnitObjects.zeptokatal
  def akat: CatalysisUnit = CatalysisUnitObjects.attokatal
  def fkat: CatalysisUnit = CatalysisUnitObjects.femtokatal
  def pkat: CatalysisUnit = CatalysisUnitObjects.picokatal
  def nkat: CatalysisUnit = CatalysisUnitObjects.nanokatal
  def `μkat`: CatalysisUnit = CatalysisUnitObjects.microkatal
  def mckat: CatalysisUnit = CatalysisUnitObjects.microkatal
  def mkat: CatalysisUnit = CatalysisUnitObjects.millikatal
  def ckat: CatalysisUnit = CatalysisUnitObjects.centikatal
  def dkat: CatalysisUnit = CatalysisUnitObjects.decikatal
  def dakat: CatalysisUnit = CatalysisUnitObjects.decakatal
  def hkat: CatalysisUnit = CatalysisUnitObjects.hectokatal
  def kkat: CatalysisUnit = CatalysisUnitObjects.kilokatal
  def Kkat: CatalysisUnit = CatalysisUnitObjects.kilokatal
  def Mkat: CatalysisUnit = CatalysisUnitObjects.megakatal
  def Gkat: CatalysisUnit = CatalysisUnitObjects.gigakatal
  def Tkat: CatalysisUnit = CatalysisUnitObjects.terakatal
  def Pkat: CatalysisUnit = CatalysisUnitObjects.petakatal
  def Ekat: CatalysisUnit = CatalysisUnitObjects.exakatal
  def Zkat: CatalysisUnit = CatalysisUnitObjects.zettakatal
  def Ykat: CatalysisUnit = CatalysisUnitObjects.yottakatal
  def U: CatalysisUnit = CatalysisUnitObjects.enzyme_unit
  def IU: CatalysisUnit = CatalysisUnitObjects.enzyme_unit
}