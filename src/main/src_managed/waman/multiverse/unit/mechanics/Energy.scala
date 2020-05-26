package waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


import waman.multiverse.unit.basic.Time
import waman.multiverse.unit.basic.TimeUnit


import waman.multiverse.unit.basic.MassUnit


import waman.multiverse.unit.radioactivity.AbsorbedDose
import waman.multiverse.unit.radioactivity.AbsorbedDoseUnit


import waman.multiverse.unit.thermodynamics.AbsoluteTemperatureUnit


import waman.multiverse.unit.thermodynamics.Entropy
import waman.multiverse.unit.thermodynamics.EntropyUnit


class Energy[A: Fractional](val value: A, val unit: EnergyUnit)
    extends LinearQuantity[Energy[A], A, EnergyUnit] {

  import spire.implicits._

  import waman.multiverse.unit.Constants
  import waman.multiverse.unit.thermodynamics.AbsoluteTemperature
  import waman.multiverse.unit.thermodynamics.AbsoluteTemperatureUnitObjects

  def toAbsoluteTemperature: AbsoluteTemperature[A] = new AbsoluteTemperature(
      apply(EnergyUnitObjects.joule) * implicitly[Fractional[A]].fromReal(r"1" / Constants.BoltzmannConstant),
      AbsoluteTemperatureUnitObjects.kelvin)

  import waman.multiverse.unit.basic.Mass
  import waman.multiverse.unit.basic.MassUnitObjects

  def toMass: Mass[A] = new Mass(
      apply(EnergyUnitObjects.joule) * implicitly[Fractional[A]].fromReal(r"1" / (Constants.SpeedOfLight * Constants.SpeedOfLight)),
      MassUnitObjects.kilogram)

  override protected def newQuantity(value: A, unit: EnergyUnit): Energy[A] = new Energy(value, unit)

  def *(time: Time[A]): AngularMomentum[A] = new AngularMomentum(this.value * time.value, this.unit * time.unit)

  def /(time: Time[A]): Power[A] = new Power(this.value / time.value, this.unit / time.unit)

  def /(mass: Mass[A]): AbsorbedDose[A] = new AbsorbedDose(this.value / mass.value, this.unit / mass.unit)

  def /(absoluteTemperature: AbsoluteTemperature[A]): Entropy[A] = new Entropy(this.value / absoluteTemperature.value, this.unit / absoluteTemperature.unit)
}

trait EnergyUnit extends LinearUnit[EnergyUnit]{

  override def getSIUnit: EnergyUnit = EnergyUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = EnergyUnit.dimension

  def *(timeUnit: TimeUnit): AngularMomentumUnit =
    new ProductUnit[AngularMomentumUnit, EnergyUnit, TimeUnit](EnergyUnit.this, timeUnit) with AngularMomentumUnit

  def /(timeUnit: TimeUnit): PowerUnit =
    new QuotientUnit[PowerUnit, EnergyUnit, TimeUnit](EnergyUnit.this, timeUnit) with PowerUnit

  def /(massUnit: MassUnit): AbsorbedDoseUnit =
    new QuotientUnit[AbsorbedDoseUnit, EnergyUnit, MassUnit](EnergyUnit.this, massUnit) with AbsorbedDoseUnit

  def /(absoluteTemperatureUnit: AbsoluteTemperatureUnit): EntropyUnit =
    new QuotientUnit[EntropyUnit, EnergyUnit, AbsoluteTemperatureUnit](EnergyUnit.this, absoluteTemperatureUnit) with EntropyUnit
}

object EnergyUnit extends UnitInfo[EnergyUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> -2, M -> 1, L -> 2).withDefaultValue(0)

  def getSIUnit: EnergyUnit = EnergyUnitObjects.joule

  import EnergyUnitObjects._
  def getUnits: Seq[EnergyUnit] =
    Seq(joule, yoctojoule, zeptojoule, attojoule, femtojoule, picojoule, nanojoule, microjoule, millijoule, centijoule, decijoule, decajoule, hectojoule, kilojoule, megajoule, gigajoule, terajoule, petajoule, exajoule, zettajoule, yottajoule, erg, electronvolt, yoctoelectronvolt, zeptoelectronvolt, attoelectronvolt, femtoelectronvolt, picoelectronvolt, nanoelectronvolt, microelectronvolt, millielectronvolt, centielectronvolt, decielectronvolt, decaelectronvolt, hectoelectronvolt, kiloelectronvolt, megaelectronvolt, gigaelectronvolt, teraelectronvolt, petaelectronvolt, exaelectronvolt, zettaelectronvolt, yottaelectronvolt, rydberg, atomic_unit_of_energy, watt_hour, kilowatt_hour, litre_atmosphere, calorie, `calorie(th)`, `calorie(IT)`, `calorie(mean)`, `calorie_4℃`, `calorie_15℃`, `calorie_20℃`, kilocalorie, tonne_of_coal_equivalent, tonne_of_oil_equivalent, ton_of_TNT, british_thermal_unit, `british_thermal_unit(ISO)`, `british_thermal_unit(IT)`, `british_thermal_unit(mean)`, `british_thermal_unit(th)`, `british_thermal_unit_59℉`, quad)
}

/** For no aliase or user defined units */
class SimpleEnergyUnit(val name: String, val symbol: String, val interval: Real) extends EnergyUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultEnergyUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends EnergyUnit

sealed trait calorieAttribute
sealed trait british_thermal_unitAttribute

object EnergyUnitObjects{

  import spire.implicits._

  import waman.multiverse.unit.Constants
  import waman.multiverse.unit.mechanics.PowerUnitObjects._
  import waman.multiverse.unit.basic.TimeUnitObjects._
  import waman.multiverse.unit.basic.VolumeUnitObjects._
  import waman.multiverse.unit.fluid.PressureUnitObjects._

  final case object joule extends SimpleEnergyUnit("joule", "J", 1)
  final case object yoctojoule extends SimpleEnergyUnit("yoctojoule", "yJ", r"1e-24")
  final case object zeptojoule extends SimpleEnergyUnit("zeptojoule", "zJ", r"1e-21")
  final case object attojoule extends SimpleEnergyUnit("attojoule", "aJ", r"1e-18")
  final case object femtojoule extends SimpleEnergyUnit("femtojoule", "fJ", r"1e-15")
  final case object picojoule extends SimpleEnergyUnit("picojoule", "pJ", r"1e-12")
  final case object nanojoule extends SimpleEnergyUnit("nanojoule", "nJ", r"1e-9")
  final case object microjoule extends DefaultEnergyUnit("microjoule", "μJ", Seq("mcJ"), r"1e-6")
  final case object millijoule extends SimpleEnergyUnit("millijoule", "mJ", r"1e-3")
  final case object centijoule extends SimpleEnergyUnit("centijoule", "cJ", r"1e-2")
  final case object decijoule extends SimpleEnergyUnit("decijoule", "dJ", r"1e-1")
  final case object decajoule extends SimpleEnergyUnit("decajoule", "daJ", r"1e1")
  final case object hectojoule extends SimpleEnergyUnit("hectojoule", "hJ", r"1e2")
  final case object kilojoule extends DefaultEnergyUnit("kilojoule", "kJ", Seq("KJ"), r"1e3")
  final case object megajoule extends SimpleEnergyUnit("megajoule", "MJ", r"1e6")
  final case object gigajoule extends SimpleEnergyUnit("gigajoule", "GJ", r"1e9")
  final case object terajoule extends SimpleEnergyUnit("terajoule", "TJ", r"1e12")
  final case object petajoule extends SimpleEnergyUnit("petajoule", "PJ", r"1e15")
  final case object exajoule extends SimpleEnergyUnit("exajoule", "EJ", r"1e18")
  final case object zettajoule extends SimpleEnergyUnit("zettajoule", "ZJ", r"1e21")
  final case object yottajoule extends SimpleEnergyUnit("yottajoule", "YJ", r"1e24")
  final case object erg extends SimpleEnergyUnit("erg", "erg", r"1e-7")
  final case object electronvolt extends SimpleEnergyUnit("electronvolt", "eV", Constants.ElementaryCharge) with NotExact
  final case object yoctoelectronvolt extends SimpleEnergyUnit("yoctoelectronvolt", "yeV", Constants.ElementaryCharge * r"1e-24") with NotExact
  final case object zeptoelectronvolt extends SimpleEnergyUnit("zeptoelectronvolt", "zeV", Constants.ElementaryCharge * r"1e-21") with NotExact
  final case object attoelectronvolt extends SimpleEnergyUnit("attoelectronvolt", "aeV", Constants.ElementaryCharge * r"1e-18") with NotExact
  final case object femtoelectronvolt extends SimpleEnergyUnit("femtoelectronvolt", "feV", Constants.ElementaryCharge * r"1e-15") with NotExact
  final case object picoelectronvolt extends SimpleEnergyUnit("picoelectronvolt", "peV", Constants.ElementaryCharge * r"1e-12") with NotExact
  final case object nanoelectronvolt extends SimpleEnergyUnit("nanoelectronvolt", "neV", Constants.ElementaryCharge * r"1e-9") with NotExact
  final case object microelectronvolt extends DefaultEnergyUnit("microelectronvolt", "μeV", Seq("mceV"), Constants.ElementaryCharge * r"1e-6") with NotExact
  final case object millielectronvolt extends SimpleEnergyUnit("millielectronvolt", "meV", Constants.ElementaryCharge * r"1e-3") with NotExact
  final case object centielectronvolt extends SimpleEnergyUnit("centielectronvolt", "ceV", Constants.ElementaryCharge * r"1e-2") with NotExact
  final case object decielectronvolt extends SimpleEnergyUnit("decielectronvolt", "deV", Constants.ElementaryCharge * r"1e-1") with NotExact
  final case object decaelectronvolt extends SimpleEnergyUnit("decaelectronvolt", "daeV", Constants.ElementaryCharge * r"1e1") with NotExact
  final case object hectoelectronvolt extends SimpleEnergyUnit("hectoelectronvolt", "heV", Constants.ElementaryCharge * r"1e2") with NotExact
  final case object kiloelectronvolt extends DefaultEnergyUnit("kiloelectronvolt", "keV", Seq("KeV"), Constants.ElementaryCharge * r"1e3") with NotExact
  final case object megaelectronvolt extends SimpleEnergyUnit("megaelectronvolt", "MeV", Constants.ElementaryCharge * r"1e6") with NotExact
  final case object gigaelectronvolt extends SimpleEnergyUnit("gigaelectronvolt", "GeV", Constants.ElementaryCharge * r"1e9") with NotExact
  final case object teraelectronvolt extends SimpleEnergyUnit("teraelectronvolt", "TeV", Constants.ElementaryCharge * r"1e12") with NotExact
  final case object petaelectronvolt extends SimpleEnergyUnit("petaelectronvolt", "PeV", Constants.ElementaryCharge * r"1e15") with NotExact
  final case object exaelectronvolt extends SimpleEnergyUnit("exaelectronvolt", "EeV", Constants.ElementaryCharge * r"1e18") with NotExact
  final case object zettaelectronvolt extends SimpleEnergyUnit("zettaelectronvolt", "ZeV", Constants.ElementaryCharge * r"1e21") with NotExact
  final case object yottaelectronvolt extends SimpleEnergyUnit("yottaelectronvolt", "YeV", Constants.ElementaryCharge * r"1e24") with NotExact
  final case object rydberg extends SimpleEnergyUnit("rydberg", "Ry", r"13.6056925330" * electronvolt.interval) with NotExact
  final case object atomic_unit_of_energy extends SimpleEnergyUnit("atomic unit of energy", "E_h", r"2" * rydberg.interval) with NotExact
  final case object watt_hour extends SimpleEnergyUnit("watt hour", "Wh", watt.interval * hour.interval)
  final case object kilowatt_hour extends SimpleEnergyUnit("kilowatt hour", "kWh", kilowatt.interval * hour.interval)
  final case object litre_atmosphere extends SimpleEnergyUnit("litre atmosphere", "sl", litre.interval * atmosphere.interval)
  final case object calorie extends SimpleEnergyUnit("calorie", "cal", `calorie(th)`.interval)
  final case object `calorie(th)` extends DefaultEnergyUnit("calorie(th)", "cal(th)", Seq("cal_th"), r"4.184")
  final case object `calorie(IT)` extends DefaultEnergyUnit("calorie(IT)", "cal(IT)", Seq("cal_IT"), r"4.1868")
  final case object `calorie(mean)` extends DefaultEnergyUnit("calorie(mean)", "cal(mean)", Seq("cal_mean"), r"4.190") with NotExact
  final case object `calorie_4℃` extends SimpleEnergyUnit("calorie 4℃", "cal_4℃", r"4.204") with NotExact
  final case object `calorie_15℃` extends SimpleEnergyUnit("calorie 15℃", "cal_15℃", r"4.1855")
  final case object `calorie_20℃` extends SimpleEnergyUnit("calorie 20℃", "cal_20℃", r"4.182") with NotExact
  final case object kilocalorie extends DefaultEnergyUnit("kilocalorie", "kcal", Seq("Cal"), r"1000" * calorie.interval)
  final case object tonne_of_coal_equivalent extends SimpleEnergyUnit("tonne of coal equivalent", "TCE", r"7e9" * `calorie(th)`.interval)
  final case object tonne_of_oil_equivalent extends SimpleEnergyUnit("tonne of oil equivalent", "toe", r"1e10" * `calorie(IT)`.interval)
  final case object ton_of_TNT extends SimpleEnergyUnit("ton of TNT", "tTNT", r"1e9" * `calorie(th)`.interval)
  final case object british_thermal_unit extends SimpleEnergyUnit("british thermal unit", "BTU", `british_thermal_unit(IT)`.interval)
  final case object `british_thermal_unit(ISO)` extends DefaultEnergyUnit("british thermal unit(ISO)", "BTU(ISO)", Seq("BTU_ISO"), r"1.0545e3")
  final case object `british_thermal_unit(IT)` extends DefaultEnergyUnit("british thermal unit(IT)", "BTU(IT)", Seq("BTU_IT"), r"1.05505585262e3")
  final case object `british_thermal_unit(mean)` extends DefaultEnergyUnit("british thermal unit(mean)", "BTU(mean)", Seq("BTU_mean"), r"1.05587e3") with NotExact
  final case object `british_thermal_unit(th)` extends DefaultEnergyUnit("british thermal unit(th)", "BTU(th)", Seq("BTU_th"), r"1.054350e3") with NotExact
  final case object `british_thermal_unit_59℉` extends SimpleEnergyUnit("british thermal unit 59℉", "BTU_59℉", r"1.054804e3")
  final case object quad extends SimpleEnergyUnit("quad", "quad", r"1e15" * `british_thermal_unit(IT)`.interval)
}

object EnergyUnits{
  final object mean extends calorieAttribute with british_thermal_unitAttribute
  final object IT extends calorieAttribute with british_thermal_unitAttribute
  final object ISO extends british_thermal_unitAttribute
  final object th extends calorieAttribute with british_thermal_unitAttribute

  def J: EnergyUnit = EnergyUnitObjects.joule
  def yJ: EnergyUnit = EnergyUnitObjects.yoctojoule
  def zJ: EnergyUnit = EnergyUnitObjects.zeptojoule
  def aJ: EnergyUnit = EnergyUnitObjects.attojoule
  def fJ: EnergyUnit = EnergyUnitObjects.femtojoule
  def pJ: EnergyUnit = EnergyUnitObjects.picojoule
  def nJ: EnergyUnit = EnergyUnitObjects.nanojoule
  def `μJ`: EnergyUnit = EnergyUnitObjects.microjoule
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
  def `μeV`: EnergyUnit = EnergyUnitObjects.microelectronvolt
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
  def Wh: EnergyUnit = EnergyUnitObjects.watt_hour
  def kWh: EnergyUnit = EnergyUnitObjects.kilowatt_hour
  def sl: EnergyUnit = EnergyUnitObjects.litre_atmosphere
  def cal: EnergyUnit = EnergyUnitObjects.calorie
  def cal(a: calorieAttribute): EnergyUnit = a match { 
    case EnergyUnits.th => EnergyUnitObjects.`calorie(th)`
    case EnergyUnits.IT => EnergyUnitObjects.`calorie(IT)`
    case EnergyUnits.mean => EnergyUnitObjects.`calorie(mean)`
  }
  def `cal_4℃`: EnergyUnit = EnergyUnitObjects.`calorie_4℃`
  def `cal_15℃`: EnergyUnit = EnergyUnitObjects.`calorie_15℃`
  def `cal_20℃`: EnergyUnit = EnergyUnitObjects.`calorie_20℃`
  def kcal: EnergyUnit = EnergyUnitObjects.kilocalorie
  def Cal: EnergyUnit = EnergyUnitObjects.kilocalorie
  def TCE: EnergyUnit = EnergyUnitObjects.tonne_of_coal_equivalent
  def toe: EnergyUnit = EnergyUnitObjects.tonne_of_oil_equivalent
  def tTNT: EnergyUnit = EnergyUnitObjects.ton_of_TNT
  def BTU: EnergyUnit = EnergyUnitObjects.british_thermal_unit
  def BTU(a: british_thermal_unitAttribute): EnergyUnit = a match { 
    case EnergyUnits.ISO => EnergyUnitObjects.`british_thermal_unit(ISO)`
    case EnergyUnits.IT => EnergyUnitObjects.`british_thermal_unit(IT)`
    case EnergyUnits.mean => EnergyUnitObjects.`british_thermal_unit(mean)`
    case EnergyUnits.th => EnergyUnitObjects.`british_thermal_unit(th)`
  }
  def `BTU_59℉`: EnergyUnit = EnergyUnitObjects.`british_thermal_unit_59℉`
  def quad: EnergyUnit = EnergyUnitObjects.quad
}