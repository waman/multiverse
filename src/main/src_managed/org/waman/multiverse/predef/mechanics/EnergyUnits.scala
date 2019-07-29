package org.waman.multiverse.predef.mechanics

import spire.math.Real
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.predef._
import org.waman.multiverse.units.mechanics.EnergyUnit

class SimpleEnergyUnit(val name: String, val symbol: String, val aliases: Seq[String], val intervalInSIUnit: Real) extends EnergyUnit

object EnergyUnitObjects{
  final object joule extends SimpleEnergyUnit("joule", "J", Nil, r"1")
  final object yoctojoule extends SimpleEnergyUnit("yoctojoule", "yJ", Nil, r"1" * r"1e-24")
  final object zeptojoule extends SimpleEnergyUnit("zeptojoule", "zJ", Nil, r"1" * r"1e-21")
  final object attojoule extends SimpleEnergyUnit("attojoule", "aJ", Nil, r"1" * r"1e-18")
  final object femtojoule extends SimpleEnergyUnit("femtojoule", "fJ", Nil, r"1" * r"1e-15")
  final object picojoule extends SimpleEnergyUnit("picojoule", "pJ", Nil, r"1" * r"1e-12")
  final object nanojoule extends SimpleEnergyUnit("nanojoule", "nJ", Nil, r"1" * r"1e-9")
  final object microjoule extends SimpleEnergyUnit("microjoule", "μJ", Nil, r"1" * r"1e-6")
  final object millijoule extends SimpleEnergyUnit("millijoule", "mJ", Nil, r"1" * r"1e-3")
  final object centijoule extends SimpleEnergyUnit("centijoule", "cJ", Nil, r"1" * r"1e-2")
  final object decijoule extends SimpleEnergyUnit("decijoule", "dJ", Nil, r"1" * r"1e-1")
  final object decajoule extends SimpleEnergyUnit("decajoule", "daJ", Nil, r"1" * r"1e1")
  final object hectojoule extends SimpleEnergyUnit("hectojoule", "hJ", Nil, r"1" * r"1e2")
  final object kilojoule extends SimpleEnergyUnit("kilojoule", "kJ", Nil, r"1" * r"1e3")
  final object megajoule extends SimpleEnergyUnit("megajoule", "MJ", Nil, r"1" * r"1e6")
  final object gigajoule extends SimpleEnergyUnit("gigajoule", "GJ", Nil, r"1" * r"1e9")
  final object terajoule extends SimpleEnergyUnit("terajoule", "TJ", Nil, r"1" * r"1e12")
  final object petajoule extends SimpleEnergyUnit("petajoule", "PJ", Nil, r"1" * r"1e15")
  final object exajoule extends SimpleEnergyUnit("exajoule", "EJ", Nil, r"1" * r"1e18")
  final object zettajoule extends SimpleEnergyUnit("zettajoule", "ZJ", Nil, r"1" * r"1e21")
  final object yottajoule extends SimpleEnergyUnit("yottajoule", "YJ", Nil, r"1" * r"1e24")
  final object erg extends SimpleEnergyUnit("erg", "erg", Nil, r"1e-7")
  final object electronvolt extends SimpleEnergyUnit("electronvolt", "eV", Nil, Constants.ElementaryCharge) with NotExact
  final object yoctoelectronvolt extends SimpleEnergyUnit("yoctoelectronvolt", "yeV", Nil, Constants.ElementaryCharge * r"1e-24") with NotExact
  final object zeptoelectronvolt extends SimpleEnergyUnit("zeptoelectronvolt", "zeV", Nil, Constants.ElementaryCharge * r"1e-21") with NotExact
  final object attoelectronvolt extends SimpleEnergyUnit("attoelectronvolt", "aeV", Nil, Constants.ElementaryCharge * r"1e-18") with NotExact
  final object femtoelectronvolt extends SimpleEnergyUnit("femtoelectronvolt", "feV", Nil, Constants.ElementaryCharge * r"1e-15") with NotExact
  final object picoelectronvolt extends SimpleEnergyUnit("picoelectronvolt", "peV", Nil, Constants.ElementaryCharge * r"1e-12") with NotExact
  final object nanoelectronvolt extends SimpleEnergyUnit("nanoelectronvolt", "neV", Nil, Constants.ElementaryCharge * r"1e-9") with NotExact
  final object microelectronvolt extends SimpleEnergyUnit("microelectronvolt", "μeV", Nil, Constants.ElementaryCharge * r"1e-6") with NotExact
  final object millielectronvolt extends SimpleEnergyUnit("millielectronvolt", "meV", Nil, Constants.ElementaryCharge * r"1e-3") with NotExact
  final object centielectronvolt extends SimpleEnergyUnit("centielectronvolt", "ceV", Nil, Constants.ElementaryCharge * r"1e-2") with NotExact
  final object decielectronvolt extends SimpleEnergyUnit("decielectronvolt", "deV", Nil, Constants.ElementaryCharge * r"1e-1") with NotExact
  final object decaelectronvolt extends SimpleEnergyUnit("decaelectronvolt", "daeV", Nil, Constants.ElementaryCharge * r"1e1") with NotExact
  final object hectoelectronvolt extends SimpleEnergyUnit("hectoelectronvolt", "heV", Nil, Constants.ElementaryCharge * r"1e2") with NotExact
  final object kiloelectronvolt extends SimpleEnergyUnit("kiloelectronvolt", "keV", Nil, Constants.ElementaryCharge * r"1e3") with NotExact
  final object megaelectronvolt extends SimpleEnergyUnit("megaelectronvolt", "MeV", Nil, Constants.ElementaryCharge * r"1e6") with NotExact
  final object gigaelectronvolt extends SimpleEnergyUnit("gigaelectronvolt", "GeV", Nil, Constants.ElementaryCharge * r"1e9") with NotExact
  final object teraelectronvolt extends SimpleEnergyUnit("teraelectronvolt", "TeV", Nil, Constants.ElementaryCharge * r"1e12") with NotExact
  final object petaelectronvolt extends SimpleEnergyUnit("petaelectronvolt", "PeV", Nil, Constants.ElementaryCharge * r"1e15") with NotExact
  final object exaelectronvolt extends SimpleEnergyUnit("exaelectronvolt", "EeV", Nil, Constants.ElementaryCharge * r"1e18") with NotExact
  final object zettaelectronvolt extends SimpleEnergyUnit("zettaelectronvolt", "ZeV", Nil, Constants.ElementaryCharge * r"1e21") with NotExact
  final object yottaelectronvolt extends SimpleEnergyUnit("yottaelectronvolt", "YeV", Nil, Constants.ElementaryCharge * r"1e24") with NotExact
  final object rydberg extends SimpleEnergyUnit("rydberg", "Ry", Nil, r"13.6056925330" * electronvolt.intervalInSIUnit) with NotExact
  final object atomic_unit_of_energy extends SimpleEnergyUnit("atomic unit of energy", "E_h", Nil, r"2" * rydberg.intervalInSIUnit) with NotExact
  final object calorie extends SimpleEnergyUnit("calorie", "cal", Seq("cal(IT)", "cal_IT"), r"4.1868")

  def getUnits: Seq[EnergyUnit] = 
    Seq(joule, yoctojoule, zeptojoule, attojoule, femtojoule, picojoule, nanojoule, microjoule, millijoule, centijoule, decijoule, decajoule, hectojoule, kilojoule, megajoule, gigajoule, terajoule, petajoule, exajoule, zettajoule, yottajoule, erg, electronvolt, yoctoelectronvolt, zeptoelectronvolt, attoelectronvolt, femtoelectronvolt, picoelectronvolt, nanoelectronvolt, microelectronvolt, millielectronvolt, centielectronvolt, decielectronvolt, decaelectronvolt, hectoelectronvolt, kiloelectronvolt, megaelectronvolt, gigaelectronvolt, teraelectronvolt, petaelectronvolt, exaelectronvolt, zettaelectronvolt, yottaelectronvolt, rydberg, atomic_unit_of_energy, calorie)

}

object EnergyUnits{
  def J: EnergyUnit = EnergyUnitObjects.joule
  def yJ: EnergyUnit = EnergyUnitObjects.yoctojoule
  def zJ: EnergyUnit = EnergyUnitObjects.zeptojoule
  def aJ: EnergyUnit = EnergyUnitObjects.attojoule
  def fJ: EnergyUnit = EnergyUnitObjects.femtojoule
  def pJ: EnergyUnit = EnergyUnitObjects.picojoule
  def nJ: EnergyUnit = EnergyUnitObjects.nanojoule
  def μJ: EnergyUnit = EnergyUnitObjects.microjoule
  def mJ: EnergyUnit = EnergyUnitObjects.millijoule
  def cJ: EnergyUnit = EnergyUnitObjects.centijoule
  def dJ: EnergyUnit = EnergyUnitObjects.decijoule
  def daJ: EnergyUnit = EnergyUnitObjects.decajoule
  def hJ: EnergyUnit = EnergyUnitObjects.hectojoule
  def kJ: EnergyUnit = EnergyUnitObjects.kilojoule
  def MJ: EnergyUnit = EnergyUnitObjects.megajoule
  def GJ: EnergyUnit = EnergyUnitObjects.gigajoule
  def TJ: EnergyUnit = EnergyUnitObjects.terajoule
  def PJ: EnergyUnit = EnergyUnitObjects.petajoule
  def EJ: EnergyUnit = EnergyUnitObjects.exajoule
  def ZJ: EnergyUnit = EnergyUnitObjects.zettajoule
  def YJ: EnergyUnit = EnergyUnitObjects.yottajoule
  def erg: EnergyUnit = EnergyUnitObjects.erg
  def eV: EnergyUnit = EnergyUnitObjects.electronvolt
  def yeV: EnergyUnit = EnergyUnitObjects.yoctoelectronvolt
  def zeV: EnergyUnit = EnergyUnitObjects.zeptoelectronvolt
  def aeV: EnergyUnit = EnergyUnitObjects.attoelectronvolt
  def feV: EnergyUnit = EnergyUnitObjects.femtoelectronvolt
  def peV: EnergyUnit = EnergyUnitObjects.picoelectronvolt
  def neV: EnergyUnit = EnergyUnitObjects.nanoelectronvolt
  def μeV: EnergyUnit = EnergyUnitObjects.microelectronvolt
  def meV: EnergyUnit = EnergyUnitObjects.millielectronvolt
  def ceV: EnergyUnit = EnergyUnitObjects.centielectronvolt
  def deV: EnergyUnit = EnergyUnitObjects.decielectronvolt
  def daeV: EnergyUnit = EnergyUnitObjects.decaelectronvolt
  def heV: EnergyUnit = EnergyUnitObjects.hectoelectronvolt
  def keV: EnergyUnit = EnergyUnitObjects.kiloelectronvolt
  def MeV: EnergyUnit = EnergyUnitObjects.megaelectronvolt
  def GeV: EnergyUnit = EnergyUnitObjects.gigaelectronvolt
  def TeV: EnergyUnit = EnergyUnitObjects.teraelectronvolt
  def PeV: EnergyUnit = EnergyUnitObjects.petaelectronvolt
  def EeV: EnergyUnit = EnergyUnitObjects.exaelectronvolt
  def ZeV: EnergyUnit = EnergyUnitObjects.zettaelectronvolt
  def YeV: EnergyUnit = EnergyUnitObjects.yottaelectronvolt
  def Ry: EnergyUnit = EnergyUnitObjects.rydberg
  def E_h: EnergyUnit = EnergyUnitObjects.atomic_unit_of_energy
  def cal: EnergyUnit = EnergyUnitObjects.calorie
  def cal_IT: EnergyUnit = EnergyUnitObjects.calorie

  def getUnits: Seq[EnergyUnit] = EnergyUnitObjects.getUnits
}