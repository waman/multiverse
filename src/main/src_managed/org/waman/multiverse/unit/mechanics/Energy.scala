package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.basic.Time
import org.waman.multiverse.unit.basic.TimeUnit

import org.waman.multiverse.unit.basic.Mass
import org.waman.multiverse.unit.basic.MassUnit

import org.waman.multiverse.unit.radiation.AbsorbedDose
import org.waman.multiverse.unit.radiation.AbsorbedDoseUnit

import org.waman.multiverse.unit.thermal.AbsoluteTemperature
import org.waman.multiverse.unit.thermal.AbsoluteTemperatureUnit

import org.waman.multiverse.unit.thermal.Entropy
import org.waman.multiverse.unit.thermal.EntropyUnit

class Energy[A: Fractional](val value: A, val unit: EnergyUnit)
    extends LinearQuantity[Energy[A], A, EnergyUnit] {

  override protected def newQuantity(value: A, unit: EnergyUnit): Energy[A] = new Energy(value, unit)
  def *(time: Time[A]): Action[A] = new Action(this.value * time.value, this.unit * time.unit)

  def /(time: Time[A]): Power[A] = new Power(this.value / time.value, this.unit / time.unit)

  def /(mass: Mass[A]): AbsorbedDose[A] = new AbsorbedDose(this.value / mass.value, this.unit / mass.unit)

  def /(absoluteTemperature: AbsoluteTemperature[A]): Entropy[A] = new Entropy(this.value / absoluteTemperature.value, this.unit / absoluteTemperature.unit)

}

trait EnergyUnit extends LinearUnit[EnergyUnit]{

  override def getSIUnit: EnergyUnit = EnergyUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = EnergyUnit.dimension

  def *(timeUnit: TimeUnit): ActionUnit =
    new ProductUnit[ActionUnit, EnergyUnit, TimeUnit](EnergyUnit.this, timeUnit) with ActionUnit

  def /(timeUnit: TimeUnit): PowerUnit =
    new QuotientUnit[PowerUnit, EnergyUnit, TimeUnit](EnergyUnit.this, timeUnit) with PowerUnit

  def /(massUnit: MassUnit): AbsorbedDoseUnit =
    new QuotientUnit[AbsorbedDoseUnit, EnergyUnit, MassUnit](EnergyUnit.this, massUnit) with AbsorbedDoseUnit

  def /(absoluteTemperatureUnit: AbsoluteTemperatureUnit): EntropyUnit =
    new QuotientUnit[EntropyUnit, EnergyUnit, AbsoluteTemperatureUnit](EnergyUnit.this, absoluteTemperatureUnit) with EntropyUnit

}

object EnergyUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, L -> 2).withDefaultValue(0)

  def getSIUnit: EnergyUnit = EnergyUnitObjects.joule

  import EnergyUnitObjects._
  def getUnits: Seq[EnergyUnit] =
    Seq(joule, yoctojoule, zeptojoule, attojoule, femtojoule, picojoule, nanojoule, microjoule, millijoule, centijoule, decijoule, decajoule, hectojoule, kilojoule, megajoule, gigajoule, terajoule, petajoule, exajoule, zettajoule, yottajoule, erg, electronvolt, yoctoelectronvolt, zeptoelectronvolt, attoelectronvolt, femtoelectronvolt, picoelectronvolt, nanoelectronvolt, microelectronvolt, millielectronvolt, centielectronvolt, decielectronvolt, decaelectronvolt, hectoelectronvolt, kiloelectronvolt, megaelectronvolt, gigaelectronvolt, teraelectronvolt, petaelectronvolt, exaelectronvolt, zettaelectronvolt, yottaelectronvolt, rydberg, atomic_unit_of_energy, calorie, `calorie(IT)`)
}


sealed trait calorieAttribute

object EnergyAttributes{
  final object IT extends calorieAttribute
}

class DefaultEnergyUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends EnergyUnit

object EnergyUnitObjects{
  import org.waman.multiverse.unit.Constants

  final object joule extends DefaultEnergyUnit("joule", "J", Nil, 1)
  final object yoctojoule extends DefaultEnergyUnit("yoctojoule", "yJ", Nil, 1 * r"1e-24")
  final object zeptojoule extends DefaultEnergyUnit("zeptojoule", "zJ", Nil, 1 * r"1e-21")
  final object attojoule extends DefaultEnergyUnit("attojoule", "aJ", Nil, 1 * r"1e-18")
  final object femtojoule extends DefaultEnergyUnit("femtojoule", "fJ", Nil, 1 * r"1e-15")
  final object picojoule extends DefaultEnergyUnit("picojoule", "pJ", Nil, 1 * r"1e-12")
  final object nanojoule extends DefaultEnergyUnit("nanojoule", "nJ", Nil, 1 * r"1e-9")
  final object microjoule extends DefaultEnergyUnit("microjoule", "μJ", Seq("mcJ"), 1 * r"1e-6")
  final object millijoule extends DefaultEnergyUnit("millijoule", "mJ", Nil, 1 * r"1e-3")
  final object centijoule extends DefaultEnergyUnit("centijoule", "cJ", Nil, 1 * r"1e-2")
  final object decijoule extends DefaultEnergyUnit("decijoule", "dJ", Nil, 1 * r"1e-1")
  final object decajoule extends DefaultEnergyUnit("decajoule", "daJ", Nil, 1 * r"1e1")
  final object hectojoule extends DefaultEnergyUnit("hectojoule", "hJ", Nil, 1 * r"1e2")
  final object kilojoule extends DefaultEnergyUnit("kilojoule", "kJ", Seq("KJ"), 1 * r"1e3")
  final object megajoule extends DefaultEnergyUnit("megajoule", "MJ", Nil, 1 * r"1e6")
  final object gigajoule extends DefaultEnergyUnit("gigajoule", "GJ", Nil, 1 * r"1e9")
  final object terajoule extends DefaultEnergyUnit("terajoule", "TJ", Nil, 1 * r"1e12")
  final object petajoule extends DefaultEnergyUnit("petajoule", "PJ", Nil, 1 * r"1e15")
  final object exajoule extends DefaultEnergyUnit("exajoule", "EJ", Nil, 1 * r"1e18")
  final object zettajoule extends DefaultEnergyUnit("zettajoule", "ZJ", Nil, 1 * r"1e21")
  final object yottajoule extends DefaultEnergyUnit("yottajoule", "YJ", Nil, 1 * r"1e24")
  final object erg extends DefaultEnergyUnit("erg", "erg", Nil, r"1e-7")
  final object electronvolt extends DefaultEnergyUnit("electronvolt", "eV", Nil, Constants.ElementaryCharge) with NotExact
  final object yoctoelectronvolt extends DefaultEnergyUnit("yoctoelectronvolt", "yeV", Nil, Constants.ElementaryCharge * r"1e-24") with NotExact
  final object zeptoelectronvolt extends DefaultEnergyUnit("zeptoelectronvolt", "zeV", Nil, Constants.ElementaryCharge * r"1e-21") with NotExact
  final object attoelectronvolt extends DefaultEnergyUnit("attoelectronvolt", "aeV", Nil, Constants.ElementaryCharge * r"1e-18") with NotExact
  final object femtoelectronvolt extends DefaultEnergyUnit("femtoelectronvolt", "feV", Nil, Constants.ElementaryCharge * r"1e-15") with NotExact
  final object picoelectronvolt extends DefaultEnergyUnit("picoelectronvolt", "peV", Nil, Constants.ElementaryCharge * r"1e-12") with NotExact
  final object nanoelectronvolt extends DefaultEnergyUnit("nanoelectronvolt", "neV", Nil, Constants.ElementaryCharge * r"1e-9") with NotExact
  final object microelectronvolt extends DefaultEnergyUnit("microelectronvolt", "μeV", Seq("mceV"), Constants.ElementaryCharge * r"1e-6") with NotExact
  final object millielectronvolt extends DefaultEnergyUnit("millielectronvolt", "meV", Nil, Constants.ElementaryCharge * r"1e-3") with NotExact
  final object centielectronvolt extends DefaultEnergyUnit("centielectronvolt", "ceV", Nil, Constants.ElementaryCharge * r"1e-2") with NotExact
  final object decielectronvolt extends DefaultEnergyUnit("decielectronvolt", "deV", Nil, Constants.ElementaryCharge * r"1e-1") with NotExact
  final object decaelectronvolt extends DefaultEnergyUnit("decaelectronvolt", "daeV", Nil, Constants.ElementaryCharge * r"1e1") with NotExact
  final object hectoelectronvolt extends DefaultEnergyUnit("hectoelectronvolt", "heV", Nil, Constants.ElementaryCharge * r"1e2") with NotExact
  final object kiloelectronvolt extends DefaultEnergyUnit("kiloelectronvolt", "keV", Seq("KeV"), Constants.ElementaryCharge * r"1e3") with NotExact
  final object megaelectronvolt extends DefaultEnergyUnit("megaelectronvolt", "MeV", Nil, Constants.ElementaryCharge * r"1e6") with NotExact
  final object gigaelectronvolt extends DefaultEnergyUnit("gigaelectronvolt", "GeV", Nil, Constants.ElementaryCharge * r"1e9") with NotExact
  final object teraelectronvolt extends DefaultEnergyUnit("teraelectronvolt", "TeV", Nil, Constants.ElementaryCharge * r"1e12") with NotExact
  final object petaelectronvolt extends DefaultEnergyUnit("petaelectronvolt", "PeV", Nil, Constants.ElementaryCharge * r"1e15") with NotExact
  final object exaelectronvolt extends DefaultEnergyUnit("exaelectronvolt", "EeV", Nil, Constants.ElementaryCharge * r"1e18") with NotExact
  final object zettaelectronvolt extends DefaultEnergyUnit("zettaelectronvolt", "ZeV", Nil, Constants.ElementaryCharge * r"1e21") with NotExact
  final object yottaelectronvolt extends DefaultEnergyUnit("yottaelectronvolt", "YeV", Nil, Constants.ElementaryCharge * r"1e24") with NotExact
  final object rydberg extends DefaultEnergyUnit("rydberg", "Ry", Nil, r"13.6056925330" * electronvolt.interval) with NotExact
  final object atomic_unit_of_energy extends DefaultEnergyUnit("atomic unit of energy", "E_h", Nil, r"2" * rydberg.interval) with NotExact
  final object calorie extends DefaultEnergyUnit("calorie", "cal", Seq("cal_IT"), r"4.1868")
  final object `calorie(IT)` extends DefaultEnergyUnit("calorie(IT)", "cal(IT)", Seq("cal_IT(IT)"), r"4.1868")
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
  def mcJ: EnergyUnit = EnergyUnitObjects.microjoule
  def mJ: EnergyUnit = EnergyUnitObjects.millijoule
  def cJ: EnergyUnit = EnergyUnitObjects.centijoule
  def dJ: EnergyUnit = EnergyUnitObjects.decijoule
  def daJ: EnergyUnit = EnergyUnitObjects.decajoule
  def hJ: EnergyUnit = EnergyUnitObjects.hectojoule
  def kJ: EnergyUnit = EnergyUnitObjects.kilojoule
  def KJ: EnergyUnit = EnergyUnitObjects.kilojoule
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
  def mceV: EnergyUnit = EnergyUnitObjects.microelectronvolt
  def meV: EnergyUnit = EnergyUnitObjects.millielectronvolt
  def ceV: EnergyUnit = EnergyUnitObjects.centielectronvolt
  def deV: EnergyUnit = EnergyUnitObjects.decielectronvolt
  def daeV: EnergyUnit = EnergyUnitObjects.decaelectronvolt
  def heV: EnergyUnit = EnergyUnitObjects.hectoelectronvolt
  def keV: EnergyUnit = EnergyUnitObjects.kiloelectronvolt
  def KeV: EnergyUnit = EnergyUnitObjects.kiloelectronvolt
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
  def cal(a: calorieAttribute): EnergyUnit = a match { 
    case EnergyAttributes.IT => EnergyUnitObjects.`calorie(IT)`
  }
  def cal_IT: EnergyUnit = EnergyUnitObjects.calorie
}