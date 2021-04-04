package org.waman.multiverse.unit.defs.chem

import spire.math._
import spire.implicits._

import org.waman.multiverse._

import org.waman.multiverse.unit.defs._

class Catalysis[A: Fractional](val value: A, val unit: CatalysisUnit)
    extends LinearQuantity[Catalysis[A], A, CatalysisUnit] {

  override protected def newQuantity(value: A, unit: CatalysisUnit): Catalysis[A] = new Catalysis(value, unit)
}

trait CatalysisUnit extends LinearUnit[CatalysisUnit]{

  override def getSIUnit: CatalysisUnit = CatalysisUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = CatalysisUnit.dimension
}

object CatalysisUnit extends UnitInfo[CatalysisUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](N -> 1, T -> -1).withDefaultValue(0)

  def getSIUnit: CatalysisUnit = CatalysisUnitObjects.katal
  import CatalysisUnitObjects._

  def getUnits: Seq[CatalysisUnit] =
    Seq(katal, yoctokatal, zeptokatal, attokatal, femtokatal, picokatal, nanokatal, microkatal, millikatal, centikatal, decikatal, decakatal, hectokatal, kilokatal, megakatal, gigakatal, terakatal, petakatal, exakatal, zettakatal, yottakatal, enzyme_unit)
}


/** For no aliase or user defined units */
class SimpleCatalysisUnit(val name: String, val symbol: String, val interval: Real) extends CatalysisUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultCatalysisUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends CatalysisUnit
  
object CatalysisUnitObjects{
  import spire.implicits._

  final case object katal extends SimpleCatalysisUnit("katal", "kat", 1)
  final case object yoctokatal extends SimpleCatalysisUnit("yoctokatal", "ykat", r"1e-24")
  final case object zeptokatal extends SimpleCatalysisUnit("zeptokatal", "zkat", r"1e-21")
  final case object attokatal extends SimpleCatalysisUnit("attokatal", "akat", r"1e-18")
  final case object femtokatal extends SimpleCatalysisUnit("femtokatal", "fkat", r"1e-15")
  final case object picokatal extends SimpleCatalysisUnit("picokatal", "pkat", r"1e-12")
  final case object nanokatal extends SimpleCatalysisUnit("nanokatal", "nkat", r"1e-9")
  final case object microkatal extends DefaultCatalysisUnit("microkatal", "μkat", Seq("mckat"), r"1e-6")
  final case object millikatal extends SimpleCatalysisUnit("millikatal", "mkat", r"1e-3")
  final case object centikatal extends SimpleCatalysisUnit("centikatal", "ckat", r"1e-2")
  final case object decikatal extends SimpleCatalysisUnit("decikatal", "dkat", r"1e-1")
  final case object decakatal extends SimpleCatalysisUnit("decakatal", "dakat", r"1e1")
  final case object hectokatal extends SimpleCatalysisUnit("hectokatal", "hkat", r"1e2")
  final case object kilokatal extends DefaultCatalysisUnit("kilokatal", "kkat", Seq("Kkat"), r"1e3")
  final case object megakatal extends SimpleCatalysisUnit("megakatal", "Mkat", r"1e6")
  final case object gigakatal extends SimpleCatalysisUnit("gigakatal", "Gkat", r"1e9")
  final case object terakatal extends SimpleCatalysisUnit("terakatal", "Tkat", r"1e12")
  final case object petakatal extends SimpleCatalysisUnit("petakatal", "Pkat", r"1e15")
  final case object exakatal extends SimpleCatalysisUnit("exakatal", "Ekat", r"1e18")
  final case object zettakatal extends SimpleCatalysisUnit("zettakatal", "Zkat", r"1e21")
  final case object yottakatal extends SimpleCatalysisUnit("yottakatal", "Ykat", r"1e24")
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
  def μkat: CatalysisUnit = CatalysisUnitObjects.microkatal
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