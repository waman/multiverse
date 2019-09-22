package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

class Mass[A: Fractional](val value: A, val unit: MassUnit)
    extends LinearQuantity[Mass[A], A, MassUnit] {

  override protected def newQuantity(value: A, unit: MassUnit): Mass[A] = new Mass(value, unit)
}

trait MassUnit extends LinearUnit[MassUnit]{
  override def getSIUnit: MassUnit = MassUnitObjects.getSIUnit


  def /(volumeUnit: VolumeUnit): DensityUnit =
    new QuotientUnit[DensityUnit, MassUnit, VolumeUnit](MassUnit.this, volumeUnit) with DensityUnit

}

class DefaultMassUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends MassUnit


object MassUnitObjects{
  final object kilogram extends DefaultMassUnit("kilogram", "kg", Seq("Kg"), r"1")
  final object gram extends DefaultMassUnit("gram", "g", Nil, r"1e-3")
  final object yoctogram extends DefaultMassUnit("yoctogram", "yg", Nil, r"1e-3" * r"1e-24")
  final object zeptogram extends DefaultMassUnit("zeptogram", "zg", Nil, r"1e-3" * r"1e-21")
  final object attogram extends DefaultMassUnit("attogram", "ag", Nil, r"1e-3" * r"1e-18")
  final object femtogram extends DefaultMassUnit("femtogram", "fg", Nil, r"1e-3" * r"1e-15")
  final object picogram extends DefaultMassUnit("picogram", "pg", Nil, r"1e-3" * r"1e-12")
  final object nanogram extends DefaultMassUnit("nanogram", "ng", Nil, r"1e-3" * r"1e-9")
  final object microgram extends DefaultMassUnit("microgram", "μg", Seq("mcg"), r"1e-3" * r"1e-6")
  final object milligram extends DefaultMassUnit("milligram", "mg", Nil, r"1e-3" * r"1e-3")
  final object centigram extends DefaultMassUnit("centigram", "cg", Nil, r"1e-3" * r"1e-2")
  final object decigram extends DefaultMassUnit("decigram", "dg", Nil, r"1e-3" * r"1e-1")
  final object decagram extends DefaultMassUnit("decagram", "dag", Nil, r"1e-3" * r"1e1")
  final object hectogram extends DefaultMassUnit("hectogram", "hg", Nil, r"1e-3" * r"1e2")
  final object megagram extends DefaultMassUnit("megagram", "Mg", Nil, r"1e-3" * r"1e6")
  final object gigagram extends DefaultMassUnit("gigagram", "Gg", Nil, r"1e-3" * r"1e9")
  final object teragram extends DefaultMassUnit("teragram", "Tg", Nil, r"1e-3" * r"1e12")
  final object petagram extends DefaultMassUnit("petagram", "Pg", Nil, r"1e-3" * r"1e15")
  final object exagram extends DefaultMassUnit("exagram", "Eg", Nil, r"1e-3" * r"1e18")
  final object zettagram extends DefaultMassUnit("zettagram", "Zg", Nil, r"1e-3" * r"1e21")
  final object yottagram extends DefaultMassUnit("yottagram", "Yg", Nil, r"1e-3" * r"1e24")
  final object tonne extends DefaultMassUnit("tonne", "t", Nil, r"1000")
  final object grave extends DefaultMassUnit("grave", "gv", Nil, r"1")
  final object gamma extends DefaultMassUnit("gamma", "γ", Nil, r"1" * microgram.interval)
  final object quintal extends DefaultMassUnit("quintal", "q", Nil, r"100" * kilogram.interval)


  def getSIUnit: MassUnit = kilogram

  def getUnits: Seq[MassUnit] =
    Seq(kilogram, gram, yoctogram, zeptogram, attogram, femtogram, picogram, nanogram, microgram, milligram, centigram, decigram, decagram, hectogram, megagram, gigagram, teragram, petagram, exagram, zettagram, yottagram, tonne, grave, gamma, quintal)
}

object MassUnits{
  def kg: MassUnit = MassUnitObjects.kilogram
  def Kg: MassUnit = MassUnitObjects.kilogram
  def g: MassUnit = MassUnitObjects.gram
  def yg: MassUnit = MassUnitObjects.yoctogram
  def zg: MassUnit = MassUnitObjects.zeptogram
  def ag: MassUnit = MassUnitObjects.attogram
  def fg: MassUnit = MassUnitObjects.femtogram
  def pg: MassUnit = MassUnitObjects.picogram
  def ng: MassUnit = MassUnitObjects.nanogram
  def μg: MassUnit = MassUnitObjects.microgram
  def mcg: MassUnit = MassUnitObjects.microgram
  def mg: MassUnit = MassUnitObjects.milligram
  def cg: MassUnit = MassUnitObjects.centigram
  def dg: MassUnit = MassUnitObjects.decigram
  def dag: MassUnit = MassUnitObjects.decagram
  def hg: MassUnit = MassUnitObjects.hectogram
  def Mg: MassUnit = MassUnitObjects.megagram
  def Gg: MassUnit = MassUnitObjects.gigagram
  def Tg: MassUnit = MassUnitObjects.teragram
  def Pg: MassUnit = MassUnitObjects.petagram
  def Eg: MassUnit = MassUnitObjects.exagram
  def Zg: MassUnit = MassUnitObjects.zettagram
  def Yg: MassUnit = MassUnitObjects.yottagram
  def t: MassUnit = MassUnitObjects.tonne
  def gv: MassUnit = MassUnitObjects.grave
  def γ: MassUnit = MassUnitObjects.gamma
  def q: MassUnit = MassUnitObjects.quintal

  def getSIUnit: MassUnit = MassUnitObjects.getSIUnit
  def getUnits: Seq[MassUnit] = MassUnitObjects.getUnits
}
