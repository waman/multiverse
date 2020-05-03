package waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


import waman.multiverse.unit.mechanics.Acceleration
import waman.multiverse.unit.mechanics.AccelerationUnit


import waman.multiverse.unit.mechanics.Force
import waman.multiverse.unit.mechanics.ForceUnit


import waman.multiverse.unit.mechanics.MassTorque
import waman.multiverse.unit.mechanics.MassTorqueUnit


import waman.multiverse.unit.density.Density
import waman.multiverse.unit.density.DensityUnit


import waman.multiverse.unit.density.LineDensity
import waman.multiverse.unit.density.LineDensityUnit


class Mass[A: Fractional](val value: A, val unit: MassUnit)
    extends LinearQuantity[Mass[A], A, MassUnit] {

  import spire.implicits._

  import waman.multiverse.unit.Constants
  import waman.multiverse.unit.mechanics.Energy
  import waman.multiverse.unit.mechanics.EnergyUnitObjects

  def toEnergy: Energy[A] = new Energy(
      apply(MassUnitObjects.kilogram) * implicitly[Fractional[A]].fromReal(Constants.SpeedOfLight * Constants.SpeedOfLight),
      EnergyUnitObjects.joule)

  override protected def newQuantity(value: A, unit: MassUnit): Mass[A] = new Mass(value, unit)

  def *(acceleration: Acceleration[A]): Force[A] = new Force(this.value * acceleration.value, this.unit * acceleration.unit)

  def *(length: Length[A]): MassTorque[A] = new MassTorque(this.value * length.value, this.unit * length.unit)

  def /(volume: Volume[A]): Density[A] = new Density(this.value / volume.value, this.unit / volume.unit)

  def /(length: Length[A]): LineDensity[A] = new LineDensity(this.value / length.value, this.unit / length.unit)
}

trait MassUnit extends LinearUnit[MassUnit]{

  override def getSIUnit: MassUnit = MassUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = MassUnit.dimension

  def *(accelerationUnit: AccelerationUnit): ForceUnit =
    new AbstractProductUnit[ForceUnit, MassUnit, AccelerationUnit](MassUnit.this, accelerationUnit) with ForceUnit

  def *(lengthUnit: LengthUnit): MassTorqueUnit =
    new AbstractProductUnit[MassTorqueUnit, MassUnit, LengthUnit](MassUnit.this, lengthUnit) with MassTorqueUnit

  def /(volumeUnit: VolumeUnit): DensityUnit =
    new AbstractQuotientUnit[DensityUnit, MassUnit, VolumeUnit](MassUnit.this, volumeUnit) with DensityUnit

  def /(lengthUnit: LengthUnit): LineDensityUnit =
    new AbstractQuotientUnit[LineDensityUnit, MassUnit, LengthUnit](MassUnit.this, lengthUnit) with LineDensityUnit
}

object MassUnit extends UnitInfo[MassUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](M -> 1).withDefaultValue(0)

  def getSIUnit: MassUnit = MassUnitObjects.kilogram

  import MassUnitObjects._
  def getUnits: Seq[MassUnit] =
    Seq(kilogram, gram, yoctogram, zeptogram, attogram, femtogram, picogram, nanogram, microgram, milligram, centigram, decigram, decagram, hectogram, megagram, gigagram, teragram, petagram, exagram, zettagram, yottagram, tonne, grave, gamma, quintal, atomic_mass_unit, electron_mass, grain, dram, `dram(avoirdupois)`, `dram(troy)`, ounce, `ounce(avoirdupois)`, `ounce(troy)`, pound, `pound(avoirdupois)`, `pound(troy)`, `pound(metric)`, long_ton, short_ton, scruple, carat, metric_carat, stone, short_hundredweight, long_hundredweight, kip, pennyweight, long_assay_ton, short_assay_ton, slug)
}

/** For no aliase or user defined units */
class SimpleMassUnit(val name: String, val symbol: String, val interval: Real) extends MassUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultMassUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends MassUnit

sealed trait dramAttribute
sealed trait ounceAttribute
sealed trait poundAttribute

object MassUnitObjects{

  import spire.implicits._

  import waman.multiverse.unit.mechanics.AccelerationUnitObjects._
  import waman.multiverse.unit.basic.LengthUnitObjects._

  final case object kilogram extends DefaultMassUnit("kilogram", "kg", Seq("Kg"), 1)
  final case object gram extends SimpleMassUnit("gram", "g", r"1e-3")
  final case object yoctogram extends SimpleMassUnit("yoctogram", "yg", r"1e-3" * r"1e-24")
  final case object zeptogram extends SimpleMassUnit("zeptogram", "zg", r"1e-3" * r"1e-21")
  final case object attogram extends SimpleMassUnit("attogram", "ag", r"1e-3" * r"1e-18")
  final case object femtogram extends SimpleMassUnit("femtogram", "fg", r"1e-3" * r"1e-15")
  final case object picogram extends SimpleMassUnit("picogram", "pg", r"1e-3" * r"1e-12")
  final case object nanogram extends SimpleMassUnit("nanogram", "ng", r"1e-3" * r"1e-9")
  final case object microgram extends DefaultMassUnit("microgram", "μg", Seq("mcg"), r"1e-3" * r"1e-6")
  final case object milligram extends SimpleMassUnit("milligram", "mg", r"1e-3" * r"1e-3")
  final case object centigram extends SimpleMassUnit("centigram", "cg", r"1e-3" * r"1e-2")
  final case object decigram extends SimpleMassUnit("decigram", "dg", r"1e-3" * r"1e-1")
  final case object decagram extends SimpleMassUnit("decagram", "dag", r"1e-3" * r"1e1")
  final case object hectogram extends SimpleMassUnit("hectogram", "hg", r"1e-3" * r"1e2")
  final case object megagram extends SimpleMassUnit("megagram", "Mg", r"1e-3" * r"1e6")
  final case object gigagram extends SimpleMassUnit("gigagram", "Gg", r"1e-3" * r"1e9")
  final case object teragram extends SimpleMassUnit("teragram", "Tg", r"1e-3" * r"1e12")
  final case object petagram extends SimpleMassUnit("petagram", "Pg", r"1e-3" * r"1e15")
  final case object exagram extends SimpleMassUnit("exagram", "Eg", r"1e-3" * r"1e18")
  final case object zettagram extends SimpleMassUnit("zettagram", "Zg", r"1e-3" * r"1e21")
  final case object yottagram extends SimpleMassUnit("yottagram", "Yg", r"1e-3" * r"1e24")
  final case object tonne extends SimpleMassUnit("tonne", "t", r"1000")
  final case object grave extends SimpleMassUnit("grave", "gv", 1)
  final case object gamma extends SimpleMassUnit("gamma", "γ", microgram.interval)
  final case object quintal extends SimpleMassUnit("quintal", "q", r"100" * kilogram.interval)
  final case object atomic_mass_unit extends DefaultMassUnit("atomic mass unit", "u", Seq("AMU", "Da"), r"1.66053892173e-27") with NotExact
  final case object electron_mass extends SimpleMassUnit("electron mass", "m_e", r"9.1093829140e-31") with NotExact
  final case object grain extends SimpleMassUnit("grain", "gr", r"1"/r"7000" * pound.interval)
  final case object dram extends SimpleMassUnit("dram", "dr", `dram(avoirdupois)`.interval)
  final case object `dram(avoirdupois)` extends DefaultMassUnit("dram(avoirdupois)", "dr(avoirdupois)", Seq("dr_av"), r"875"/r"32" * grain.interval)
  final case object `dram(troy)` extends DefaultMassUnit("dram(troy)", "dr(troy)", Seq("dr_t"), r"60" * grain.interval)
  final case object ounce extends SimpleMassUnit("ounce", "oz", `ounce(avoirdupois)`.interval)
  final case object `ounce(avoirdupois)` extends DefaultMassUnit("ounce(avoirdupois)", "oz(avoirdupois)", Seq("oz_av"), r"1"/r"16" * pound.interval)
  final case object `ounce(troy)` extends DefaultMassUnit("ounce(troy)", "oz(troy)", Seq("oz_t"), r"1"/r"12" * `pound(troy)`.interval)
  final case object pound extends SimpleMassUnit("pound", "lb", `pound(avoirdupois)`.interval)
  final case object `pound(avoirdupois)` extends DefaultMassUnit("pound(avoirdupois)", "lb(avoirdupois)", Seq("lb_av"), r"0.45359237")
  final case object `pound(troy)` extends DefaultMassUnit("pound(troy)", "lb(troy)", Seq("lb_t"), r"5760" * grain.interval)
  final case object `pound(metric)` extends SimpleMassUnit("pound(metric)", "lb(metric)", r"500" * gram.interval)
  final case object long_ton extends SimpleMassUnit("long ton", "long_tn", r"2240" * pound.interval)
  final case object short_ton extends SimpleMassUnit("short ton", "sh_tn", r"2000" * pound.interval)
  final case object scruple extends SimpleMassUnit("scruple", "s_ap", r"20" * grain.interval)
  final case object carat extends SimpleMassUnit("carat", "kt", r"19"/r"6" * grain.interval)
  final case object metric_carat extends SimpleMassUnit("metric carat", "ct", r"200" * milligram.interval)
  final case object stone extends SimpleMassUnit("stone", "st", r"14" * pound.interval)
  final case object short_hundredweight extends DefaultMassUnit("short hundredweight", "cwt", Seq("sh_cwt", "US_cwt", "cental"), r"100" * `pound(avoirdupois)`.interval)
  final case object long_hundredweight extends DefaultMassUnit("long hundredweight", "long_cwt", Seq(""), r"112" * `pound(avoirdupois)`.interval)
  final case object kip extends SimpleMassUnit("kip", "kip", r"1000" * `dram(avoirdupois)`.interval)
  final case object pennyweight extends DefaultMassUnit("pennyweight", "dwt", Seq("pwt"), r"1"/r"20" * `ounce(troy)`.interval)
  final case object long_assay_ton extends DefaultMassUnit("long assay ton", "long_AT", Seq("AT"), r"98"/r"3" * gram.interval)
  final case object short_assay_ton extends SimpleMassUnit("short assay ton", "sh_AT", r"175"/r"6" * gram.interval)
  final case object slug extends SimpleMassUnit("slug", "slug", pound.interval * standard_gravity.interval / foot.interval)
}

object MassUnits{
  final object avoirdupois extends dramAttribute with ounceAttribute with poundAttribute
  final object troy extends dramAttribute with ounceAttribute with poundAttribute
  final object metric extends poundAttribute

  def kg: MassUnit = MassUnitObjects.kilogram
  def Kg: MassUnit = MassUnitObjects.kilogram
  def g: MassUnit = MassUnitObjects.gram
  def yg: MassUnit = MassUnitObjects.yoctogram
  def zg: MassUnit = MassUnitObjects.zeptogram
  def ag: MassUnit = MassUnitObjects.attogram
  def fg: MassUnit = MassUnitObjects.femtogram
  def pg: MassUnit = MassUnitObjects.picogram
  def ng: MassUnit = MassUnitObjects.nanogram
  def `μg`: MassUnit = MassUnitObjects.microgram
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
  def `γ`: MassUnit = MassUnitObjects.gamma
  def q: MassUnit = MassUnitObjects.quintal
  def u: MassUnit = MassUnitObjects.atomic_mass_unit
  def AMU: MassUnit = MassUnitObjects.atomic_mass_unit
  def Da: MassUnit = MassUnitObjects.atomic_mass_unit
  def m_e: MassUnit = MassUnitObjects.electron_mass
  def gr: MassUnit = MassUnitObjects.grain
  def dr: MassUnit = MassUnitObjects.dram
  def dr(a: dramAttribute): MassUnit = a match { 
    case MassUnits.avoirdupois => MassUnitObjects.`dram(avoirdupois)`
    case MassUnits.troy => MassUnitObjects.`dram(troy)`
  }
  def oz: MassUnit = MassUnitObjects.ounce
  def oz(a: ounceAttribute): MassUnit = a match { 
    case MassUnits.avoirdupois => MassUnitObjects.`ounce(avoirdupois)`
    case MassUnits.troy => MassUnitObjects.`ounce(troy)`
  }
  def lb: MassUnit = MassUnitObjects.pound
  def lb(a: poundAttribute): MassUnit = a match { 
    case MassUnits.avoirdupois => MassUnitObjects.`pound(avoirdupois)`
    case MassUnits.troy => MassUnitObjects.`pound(troy)`
    case MassUnits.metric => MassUnitObjects.`pound(metric)`
  }
  def long_tn: MassUnit = MassUnitObjects.long_ton
  def sh_tn: MassUnit = MassUnitObjects.short_ton
  def s_ap: MassUnit = MassUnitObjects.scruple
  def kt: MassUnit = MassUnitObjects.carat
  def ct: MassUnit = MassUnitObjects.metric_carat
  def st: MassUnit = MassUnitObjects.stone
  def cwt: MassUnit = MassUnitObjects.short_hundredweight
  def sh_cwt: MassUnit = MassUnitObjects.short_hundredweight
  def US_cwt: MassUnit = MassUnitObjects.short_hundredweight
  def cental: MassUnit = MassUnitObjects.short_hundredweight
  def long_cwt: MassUnit = MassUnitObjects.long_hundredweight
  def kip: MassUnit = MassUnitObjects.kip
  def dwt: MassUnit = MassUnitObjects.pennyweight
  def pwt: MassUnit = MassUnitObjects.pennyweight
  def long_AT: MassUnit = MassUnitObjects.long_assay_ton
  def AT: MassUnit = MassUnitObjects.long_assay_ton
  def sh_AT: MassUnit = MassUnitObjects.short_assay_ton
  def slug: MassUnit = MassUnitObjects.slug
}