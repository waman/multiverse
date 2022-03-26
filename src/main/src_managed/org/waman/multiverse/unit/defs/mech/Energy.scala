package org.waman.multiverse.unit.defs.mech

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs.therm._
import org.waman.multiverse.unit.defs._
import org.waman.multiverse.unit.defs.ra._
import org.waman.multiverse.unit.defs.fluid._
import org.waman.multiverse.Constants

class Energy[A: Fractional](val value: A, val unit: EnergyUnit)
    extends LinearQuantity[Energy[A], A, EnergyUnit] {


  def toAbsoluteTemperature: AbsoluteTemperature[A] = new AbsoluteTemperature(
      apply(EnergyUnitObjects.joule) * implicitly[Fractional[A]].fromReal(r"1" / Constants.BoltzmannConstant),
      AbsoluteTemperatureUnitObjects.kelvin)


  def toMass: Mass[A] = new Mass(
      apply(EnergyUnitObjects.joule) * implicitly[Fractional[A]].fromReal(r"1" / (Constants.SpeedOfLight * Constants.SpeedOfLight)),
      MassUnitObjects.kilogram)

  override protected def newQuantity(value: A, unit: EnergyUnit): Energy[A] = new Energy(value, unit)

  def *(time: Time[A]): AngularMomentum[A] = new AngularMomentum(this.value * time.value, this.unit * time.unit)

  def /(time: Time[A]): Power[A] = new Power(this.value / time.value, this.unit / time.unit)

  def /(mass: Mass[A]): AbsorbedDose[A] = new AbsorbedDose(this.value / mass.value, this.unit / mass.unit)

  def /(absoluteTemperature: AbsoluteTemperature[A]): Entropy[A] = new Entropy(this.value / absoluteTemperature.value, this.unit / absoluteTemperature.unit)
}

/** None */
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


/** For no alias or user defined units */
class SimpleEnergyUnit(val name: String, val symbol: String, val interval: Real) extends EnergyUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultEnergyUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends EnergyUnit
  
object EnergyUnitObjects{

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
  final case object rydberg extends SimpleEnergyUnit("rydberg", "Ry", Constants.RydbergConstant * Constants.PlanckConstant * Constants.SpeedOfLight) with NotExact
  final case object atomic_unit_of_energy extends SimpleEnergyUnit("atomic unit of energy", "E_h", r"2" * rydberg.interval) with NotExact
  final case object watt_hour extends SimpleEnergyUnit("watt hour", "Wh", PowerUnitObjects.watt.interval * TimeUnitObjects.hour.interval)
  final case object kilowatt_hour extends SimpleEnergyUnit("kilowatt hour", "kWh", PowerUnitObjects.kilowatt.interval * TimeUnitObjects.hour.interval)
  final case object litre_atmosphere extends SimpleEnergyUnit("litre atmosphere", "sl", VolumeUnitObjects.litre.interval * PressureUnitObjects.atmosphere.interval)
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

sealed trait calorieAttribute
sealed trait british_thermal_unitAttribute

object EnergyUnits{
  final object th extends calorieAttribute with british_thermal_unitAttribute
  final object IT extends calorieAttribute with british_thermal_unitAttribute
  final object mean extends calorieAttribute with british_thermal_unitAttribute
  final object ISO extends british_thermal_unitAttribute

  /** joule */
  def J: EnergyUnit = EnergyUnitObjects.joule
  /** yoctojoule */
  def yJ: EnergyUnit = EnergyUnitObjects.yoctojoule
  /** zeptojoule */
  def zJ: EnergyUnit = EnergyUnitObjects.zeptojoule
  /** attojoule */
  def aJ: EnergyUnit = EnergyUnitObjects.attojoule
  /** femtojoule */
  def fJ: EnergyUnit = EnergyUnitObjects.femtojoule
  /** picojoule */
  def pJ: EnergyUnit = EnergyUnitObjects.picojoule
  /** nanojoule */
  def nJ: EnergyUnit = EnergyUnitObjects.nanojoule
  /** microjoule */
  def μJ: EnergyUnit = EnergyUnitObjects.microjoule
  /** microjoule */
  def mcJ: EnergyUnit = EnergyUnitObjects.microjoule
  /** millijoule */
  def mJ: EnergyUnit = EnergyUnitObjects.millijoule
  /** centijoule */
  def cJ: EnergyUnit = EnergyUnitObjects.centijoule
  /** decijoule */
  def dJ: EnergyUnit = EnergyUnitObjects.decijoule
  /** decajoule */
  def daJ: EnergyUnit = EnergyUnitObjects.decajoule
  /** hectojoule */
  def hJ: EnergyUnit = EnergyUnitObjects.hectojoule
  /** kilojoule */
  def kJ: EnergyUnit = EnergyUnitObjects.kilojoule
  /** kilojoule */
  def KJ: EnergyUnit = EnergyUnitObjects.kilojoule
  /** megajoule */
  def MJ: EnergyUnit = EnergyUnitObjects.megajoule
  /** gigajoule */
  def GJ: EnergyUnit = EnergyUnitObjects.gigajoule
  /** terajoule */
  def TJ: EnergyUnit = EnergyUnitObjects.terajoule
  /** petajoule */
  def PJ: EnergyUnit = EnergyUnitObjects.petajoule
  /** exajoule */
  def EJ: EnergyUnit = EnergyUnitObjects.exajoule
  /** zettajoule */
  def ZJ: EnergyUnit = EnergyUnitObjects.zettajoule
  /** yottajoule */
  def YJ: EnergyUnit = EnergyUnitObjects.yottajoule
  /** erg */
  def erg: EnergyUnit = EnergyUnitObjects.erg
  /** electronvolt */
  def eV: EnergyUnit = EnergyUnitObjects.electronvolt
  /** yoctoelectronvolt */
  def yeV: EnergyUnit = EnergyUnitObjects.yoctoelectronvolt
  /** zeptoelectronvolt */
  def zeV: EnergyUnit = EnergyUnitObjects.zeptoelectronvolt
  /** attoelectronvolt */
  def aeV: EnergyUnit = EnergyUnitObjects.attoelectronvolt
  /** femtoelectronvolt */
  def feV: EnergyUnit = EnergyUnitObjects.femtoelectronvolt
  /** picoelectronvolt */
  def peV: EnergyUnit = EnergyUnitObjects.picoelectronvolt
  /** nanoelectronvolt */
  def neV: EnergyUnit = EnergyUnitObjects.nanoelectronvolt
  /** microelectronvolt */
  def μeV: EnergyUnit = EnergyUnitObjects.microelectronvolt
  /** microelectronvolt */
  def mceV: EnergyUnit = EnergyUnitObjects.microelectronvolt
  /** millielectronvolt */
  def meV: EnergyUnit = EnergyUnitObjects.millielectronvolt
  /** centielectronvolt */
  def ceV: EnergyUnit = EnergyUnitObjects.centielectronvolt
  /** decielectronvolt */
  def deV: EnergyUnit = EnergyUnitObjects.decielectronvolt
  /** decaelectronvolt */
  def daeV: EnergyUnit = EnergyUnitObjects.decaelectronvolt
  /** hectoelectronvolt */
  def heV: EnergyUnit = EnergyUnitObjects.hectoelectronvolt
  /** kiloelectronvolt */
  def keV: EnergyUnit = EnergyUnitObjects.kiloelectronvolt
  /** kiloelectronvolt */
  def KeV: EnergyUnit = EnergyUnitObjects.kiloelectronvolt
  /** megaelectronvolt */
  def MeV: EnergyUnit = EnergyUnitObjects.megaelectronvolt
  /** gigaelectronvolt */
  def GeV: EnergyUnit = EnergyUnitObjects.gigaelectronvolt
  /** teraelectronvolt */
  def TeV: EnergyUnit = EnergyUnitObjects.teraelectronvolt
  /** petaelectronvolt */
  def PeV: EnergyUnit = EnergyUnitObjects.petaelectronvolt
  /** exaelectronvolt */
  def EeV: EnergyUnit = EnergyUnitObjects.exaelectronvolt
  /** zettaelectronvolt */
  def ZeV: EnergyUnit = EnergyUnitObjects.zettaelectronvolt
  /** yottaelectronvolt */
  def YeV: EnergyUnit = EnergyUnitObjects.yottaelectronvolt
  /** rydberg */
  def Ry: EnergyUnit = EnergyUnitObjects.rydberg
  /** atomic unit of energy */
  def E_h: EnergyUnit = EnergyUnitObjects.atomic_unit_of_energy
  /** watt hour */
  def Wh: EnergyUnit = EnergyUnitObjects.watt_hour
  /** kilowatt hour */
  def kWh: EnergyUnit = EnergyUnitObjects.kilowatt_hour
  /** litre atmosphere */
  def sl: EnergyUnit = EnergyUnitObjects.litre_atmosphere
  /** calorie */
  def cal: EnergyUnit = EnergyUnitObjects.calorie
  /** calorie(th)<br/>calorie(IT)<br/>calorie(mean) */
  def cal(a: calorieAttribute): EnergyUnit = a match {
    case EnergyUnits.th => EnergyUnitObjects.`calorie(th)`
    case EnergyUnits.IT => EnergyUnitObjects.`calorie(IT)`
    case EnergyUnits.mean => EnergyUnitObjects.`calorie(mean)`
  }
  /** calorie(th) */
  def `cal(th)`: EnergyUnit = EnergyUnitObjects.`calorie(th)`
  /** calorie(th) */
  def cal_th: EnergyUnit = EnergyUnitObjects.`calorie(th)`
  /** calorie(IT) */
  def `cal(IT)`: EnergyUnit = EnergyUnitObjects.`calorie(IT)`
  /** calorie(IT) */
  def cal_IT: EnergyUnit = EnergyUnitObjects.`calorie(IT)`
  /** calorie(mean) */
  def `cal(mean)`: EnergyUnit = EnergyUnitObjects.`calorie(mean)`
  /** calorie(mean) */
  def cal_mean: EnergyUnit = EnergyUnitObjects.`calorie(mean)`
  /** calorie 4℃ */
  def `cal_4℃`: EnergyUnit = EnergyUnitObjects.`calorie_4℃`
  /** calorie 15℃ */
  def `cal_15℃`: EnergyUnit = EnergyUnitObjects.`calorie_15℃`
  /** calorie 20℃ */
  def `cal_20℃`: EnergyUnit = EnergyUnitObjects.`calorie_20℃`
  /** kilocalorie */
  def kcal: EnergyUnit = EnergyUnitObjects.kilocalorie
  /** kilocalorie */
  def Cal: EnergyUnit = EnergyUnitObjects.kilocalorie
  /** tonne of coal equivalent */
  def TCE: EnergyUnit = EnergyUnitObjects.tonne_of_coal_equivalent
  /** tonne of oil equivalent */
  def toe: EnergyUnit = EnergyUnitObjects.tonne_of_oil_equivalent
  /** ton of TNT */
  def tTNT: EnergyUnit = EnergyUnitObjects.ton_of_TNT
  /** british thermal unit */
  def BTU: EnergyUnit = EnergyUnitObjects.british_thermal_unit
  /** british_thermal_unit(ISO)<br/>british_thermal_unit(IT)<br/>british_thermal_unit(mean)<br/>british_thermal_unit(th) */
  def BTU(a: british_thermal_unitAttribute): EnergyUnit = a match {
    case EnergyUnits.ISO => EnergyUnitObjects.`british_thermal_unit(ISO)`
    case EnergyUnits.IT => EnergyUnitObjects.`british_thermal_unit(IT)`
    case EnergyUnits.mean => EnergyUnitObjects.`british_thermal_unit(mean)`
    case EnergyUnits.th => EnergyUnitObjects.`british_thermal_unit(th)`
  }
  /** british thermal unit(ISO) */
  def `BTU(ISO)`: EnergyUnit = EnergyUnitObjects.`british_thermal_unit(ISO)`
  /** british thermal unit(ISO) */
  def BTU_ISO: EnergyUnit = EnergyUnitObjects.`british_thermal_unit(ISO)`
  /** british thermal unit(IT) */
  def `BTU(IT)`: EnergyUnit = EnergyUnitObjects.`british_thermal_unit(IT)`
  /** british thermal unit(IT) */
  def BTU_IT: EnergyUnit = EnergyUnitObjects.`british_thermal_unit(IT)`
  /** british thermal unit(mean) */
  def `BTU(mean)`: EnergyUnit = EnergyUnitObjects.`british_thermal_unit(mean)`
  /** british thermal unit(mean) */
  def BTU_mean: EnergyUnit = EnergyUnitObjects.`british_thermal_unit(mean)`
  /** british thermal unit(th) */
  def `BTU(th)`: EnergyUnit = EnergyUnitObjects.`british_thermal_unit(th)`
  /** british thermal unit(th) */
  def BTU_th: EnergyUnit = EnergyUnitObjects.`british_thermal_unit(th)`
  /** british thermal unit 59℉ */
  def `BTU_59℉`: EnergyUnit = EnergyUnitObjects.`british_thermal_unit_59℉`
  /** quad */
  def quad: EnergyUnit = EnergyUnitObjects.quad
}