package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.mechanics.Acceleration
import org.waman.multiverse.unit.mechanics.AccelerationUnit
import org.waman.multiverse.unit.mechanics.Force
import org.waman.multiverse.unit.mechanics.ForceUnit
import org.waman.multiverse.unit.density.Density
import org.waman.multiverse.unit.density.DensityUnit
import org.waman.multiverse.unit.density.LineDensity
import org.waman.multiverse.unit.density.LineDensityUnit

class Mass[A: Fractional](val value: A, val unit: MassUnit)
    extends LinearQuantity[Mass[A], A, MassUnit] {

  override protected def newQuantity(value: A, unit: MassUnit): Mass[A] = new Mass(value, unit)

  def *(acceleration: Acceleration[A]): Force[A] = new Force(this.value * acceleration.value, this.unit * acceleration.unit)

  def /(volume: Volume[A]): Density[A] = new Density(this.value / volume.value, this.unit / volume.unit)

  def /(length: Length[A]): LineDensity[A] = new LineDensity(this.value / length.value, this.unit / length.unit)

  import org.waman.multiverse.unit.Constants
  import org.waman.multiverse.unit.mechanics.Energy
  import org.waman.multiverse.unit.mechanics.EnergyUnitObjects

  def toEnergy: Energy[A] = new Energy(
      apply(MassUnitObjects.kilogram) * implicitly[Fractional[A]].fromReal(Constants.SpeedOfLight * Constants.SpeedOfLight),
      EnergyUnitObjects.joule)

}

trait MassUnit extends LinearUnit[MassUnit]{

  override def getSIUnit: MassUnit = MassUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = MassUnit.dimension

  def *(accelerationUnit: AccelerationUnit): ForceUnit =
    new AbstractProductUnit[ForceUnit, MassUnit, AccelerationUnit](MassUnit.this, accelerationUnit) with ForceUnit

  def /(volumeUnit: VolumeUnit): DensityUnit =
    new AbstractQuotientUnit[DensityUnit, MassUnit, VolumeUnit](MassUnit.this, volumeUnit) with DensityUnit

  def /(lengthUnit: LengthUnit): LineDensityUnit =
    new AbstractQuotientUnit[LineDensityUnit, MassUnit, LengthUnit](MassUnit.this, lengthUnit) with LineDensityUnit

}

/** For user defined units */
class SimpleMassUnit(val name: String, val symbol: String, val interval: Real) extends MassUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultMassUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends MassUnit

object MassUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](M -> 1).withDefaultValue(0)

  def getSIUnit: MassUnit = MassUnitObjects.kilogram

  import MassUnitObjects._
  def getUnits: Seq[MassUnit] =
    Seq(kilogram, gram, yoctogram, zeptogram, attogram, femtogram, picogram, nanogram, microgram, milligram, centigram, decigram, decagram, hectogram, megagram, gigagram, teragram, petagram, exagram, zettagram, yottagram, tonne, grave, gamma, quintal, atomic_mass_unit, electron_mass, ounce, pound, long_ton, short_ton, scruple, carat, metric_carat, stone, dram_avoirdupois, grain, long_hundred_weight, short_hundred_weight, kip, ounce_avoirdupois, dram_troy, ounce_troy, pound_troy, pennyweight, long_assay_ton, short_assay_ton, slug)
}

object MassUnitObjects{
  import org.waman.multiverse.unit.mechanics.AccelerationUnitObjects

  final case object kilogram extends DefaultMassUnit("kilogram", "kg", Seq("Kg"), 1)
  final case object gram extends DefaultMassUnit("gram", "g", Nil, r"1e-3")
  final case object yoctogram extends DefaultMassUnit("yoctogram", "yg", Nil, r"1e-3" * r"1e-24")
  final case object zeptogram extends DefaultMassUnit("zeptogram", "zg", Nil, r"1e-3" * r"1e-21")
  final case object attogram extends DefaultMassUnit("attogram", "ag", Nil, r"1e-3" * r"1e-18")
  final case object femtogram extends DefaultMassUnit("femtogram", "fg", Nil, r"1e-3" * r"1e-15")
  final case object picogram extends DefaultMassUnit("picogram", "pg", Nil, r"1e-3" * r"1e-12")
  final case object nanogram extends DefaultMassUnit("nanogram", "ng", Nil, r"1e-3" * r"1e-9")
  final case object microgram extends DefaultMassUnit("microgram", "μg", Seq("mcg"), r"1e-3" * r"1e-6")
  final case object milligram extends DefaultMassUnit("milligram", "mg", Nil, r"1e-3" * r"1e-3")
  final case object centigram extends DefaultMassUnit("centigram", "cg", Nil, r"1e-3" * r"1e-2")
  final case object decigram extends DefaultMassUnit("decigram", "dg", Nil, r"1e-3" * r"1e-1")
  final case object decagram extends DefaultMassUnit("decagram", "dag", Nil, r"1e-3" * r"1e1")
  final case object hectogram extends DefaultMassUnit("hectogram", "hg", Nil, r"1e-3" * r"1e2")
  final case object megagram extends DefaultMassUnit("megagram", "Mg", Nil, r"1e-3" * r"1e6")
  final case object gigagram extends DefaultMassUnit("gigagram", "Gg", Nil, r"1e-3" * r"1e9")
  final case object teragram extends DefaultMassUnit("teragram", "Tg", Nil, r"1e-3" * r"1e12")
  final case object petagram extends DefaultMassUnit("petagram", "Pg", Nil, r"1e-3" * r"1e15")
  final case object exagram extends DefaultMassUnit("exagram", "Eg", Nil, r"1e-3" * r"1e18")
  final case object zettagram extends DefaultMassUnit("zettagram", "Zg", Nil, r"1e-3" * r"1e21")
  final case object yottagram extends DefaultMassUnit("yottagram", "Yg", Nil, r"1e-3" * r"1e24")
  final case object tonne extends DefaultMassUnit("tonne", "t", Nil, r"1000")
  final case object grave extends DefaultMassUnit("grave", "gv", Nil, 1)
  final case object gamma extends DefaultMassUnit("gamma", "γ", Nil, microgram.interval)
  final case object quintal extends DefaultMassUnit("quintal", "q", Nil, r"100" * kilogram.interval)
  final case object atomic_mass_unit extends DefaultMassUnit("atomic mass unit", "u", Seq("AMU", "Da"), r"1.66053892173e-27") with NotExact
  final case object electron_mass extends DefaultMassUnit("electron mass", "m_e", Nil, r"9.1093829140e-31") with NotExact
  final case object ounce extends DefaultMassUnit("ounce", "oz", Nil, r"28" * gram.interval)
  final case object pound extends DefaultMassUnit("pound", "lb", Seq("lb_av"), r"0.45359237")
  final case object long_ton extends DefaultMassUnit("long ton", "long_tn", Nil, r"2240" * pound.interval)
  final case object short_ton extends DefaultMassUnit("short ton", "sh_tn", Nil, r"2000" * pound.interval)
  final case object scruple extends DefaultMassUnit("scruple", "s_ap", Nil, r"20" * grain.interval)
  final case object carat extends DefaultMassUnit("carat", "kt", Nil, r"19"/r"6" * grain.interval)
  final case object metric_carat extends DefaultMassUnit("metric carat", "ct", Nil, r"200" * milligram.interval)
  final case object stone extends DefaultMassUnit("stone", "st", Nil, r"14" * pound.interval)
  final case object dram_avoirdupois extends DefaultMassUnit("dram avoirdupois", "dr_av", Nil, r"875"/r"32" * grain.interval)
  final case object grain extends DefaultMassUnit("grain", "gr", Nil, r"1"/r"7000" * pound.interval)
  final case object long_hundred_weight extends DefaultMassUnit("long hundred weight", "cwt", Seq("long_cwt"), r"112" * dram_avoirdupois.interval)
  final case object short_hundred_weight extends DefaultMassUnit("short hundred weight", "sh_cwt", Nil, r"100" * dram_avoirdupois.interval)
  final case object kip extends DefaultMassUnit("kip", "kip", Nil, r"1000" * dram_avoirdupois.interval)
  final case object ounce_avoirdupois extends DefaultMassUnit("ounce avoirdupois", "oz_av", Nil, r"1"/r"16" * pound.interval)
  final case object dram_troy extends DefaultMassUnit("dram troy", "dr_t", Nil, r"60" * grain.interval)
  final case object ounce_troy extends DefaultMassUnit("ounce troy", "oz_t", Nil, r"1"/r"12" * pound_troy.interval)
  final case object pound_troy extends DefaultMassUnit("pound troy", "lb_t", Nil, r"5760" * grain.interval)
  final case object pennyweight extends DefaultMassUnit("pennyweight", "dwt", Seq("pwt"), r"1"/r"20" * ounce_troy.interval)
  final case object long_assay_ton extends DefaultMassUnit("long assay ton", "long_AT", Seq("AT"), r"98"/r"3" * gram.interval)
  final case object short_assay_ton extends DefaultMassUnit("short assay ton", "sh_AT", Nil, r"175"/r"6" * gram.interval)
  final case object slug extends DefaultMassUnit("slug", "slug", Nil, pound.interval * AccelerationUnitObjects.standard_gravity.interval / LengthUnitObjects.foot.interval)
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
  def oz: MassUnit = MassUnitObjects.ounce
  def lb: MassUnit = MassUnitObjects.pound
  def lb_av: MassUnit = MassUnitObjects.pound
  def long_tn: MassUnit = MassUnitObjects.long_ton
  def sh_tn: MassUnit = MassUnitObjects.short_ton
  def s_ap: MassUnit = MassUnitObjects.scruple
  def kt: MassUnit = MassUnitObjects.carat
  def ct: MassUnit = MassUnitObjects.metric_carat
  def st: MassUnit = MassUnitObjects.stone
  def dr_av: MassUnit = MassUnitObjects.dram_avoirdupois
  def gr: MassUnit = MassUnitObjects.grain
  def cwt: MassUnit = MassUnitObjects.long_hundred_weight
  def long_cwt: MassUnit = MassUnitObjects.long_hundred_weight
  def sh_cwt: MassUnit = MassUnitObjects.short_hundred_weight
  def kip: MassUnit = MassUnitObjects.kip
  def oz_av: MassUnit = MassUnitObjects.ounce_avoirdupois
  def dr_t: MassUnit = MassUnitObjects.dram_troy
  def oz_t: MassUnit = MassUnitObjects.ounce_troy
  def lb_t: MassUnit = MassUnitObjects.pound_troy
  def dwt: MassUnit = MassUnitObjects.pennyweight
  def pwt: MassUnit = MassUnitObjects.pennyweight
  def long_AT: MassUnit = MassUnitObjects.long_assay_ton
  def AT: MassUnit = MassUnitObjects.long_assay_ton
  def sh_AT: MassUnit = MassUnitObjects.short_assay_ton
  def slug: MassUnit = MassUnitObjects.slug
}