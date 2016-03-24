package org.waman.multiverse.mass

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._
import org.waman.multiverse.mechanics._

sealed trait MassUnit extends PhysicalUnit[MassUnit]
  with MultiplicativeByAccelerationUnit[ForceUnit]
  with DivisibleByVolumeUnit[DensityUnit]{

  def unitInKiloGram: Real

  override def baseUnit = org.waman.multiverse.mass.MassUnit.KiloGram
  override def valueInBaseUnit = unitInKiloGram

  override def *(unit: AccelerationUnit) = ForceUnit(this, unit)

  override def /(unit: VolumeUnit) = DensityUnit(this, unit)
}

object MassUnit extends ConstantsDefined[MassUnit]{

  // intrinsic
  private[MassUnit]
  class IntrinsicMassUnit(name: String, val symbols: Seq[String], val unitInKiloGram: Real)
      extends MassUnit{

    def this(name: String, symbols: Seq[String], unit: MassUnit) =
      this(name, symbols, unit.unitInKiloGram)

    def this(name: String, symbols: Seq[String], factor: Real, unit: MassUnit) =
      this(name, symbols, factor * unit.unitInKiloGram)
  }


  case object YoctoGram extends IntrinsicMassUnit("YoctoGram", Seq("yg"), r"1e-24" * r"1e-3")
  case object ZeptoGram extends IntrinsicMassUnit("ZeptoGram", Seq("zg"), r"1e-21" * r"1e-3")
  case object AttoGram extends IntrinsicMassUnit("AttoGram", Seq("ag"), r"1e-18" * r"1e-3")
  case object FemtoGram extends IntrinsicMassUnit("FemtoGram", Seq("fg"), r"1e-15" * r"1e-3")
  case object PicoGram extends IntrinsicMassUnit("PicoGram", Seq("pg"), r"1e-12" * r"1e-3")
  case object NanoGram extends IntrinsicMassUnit("NanoGram", Seq("ng"), r"1e-9" * r"1e-3")
  case object MicroGram extends IntrinsicMassUnit("MicroGram", Seq("microGram", "μg"), r"1e-6" * r"1e-3")
  case object MilliGram extends IntrinsicMassUnit("MilliGram", Seq("mg"), r"1e-3" * r"1e-3")
  case object CentiGram extends IntrinsicMassUnit("CentiGram", Seq("cg"), r"1e-2" * r"1e-3")
  case object DeciGram extends IntrinsicMassUnit("DeciGram", Seq("dg"), r"1e-1" * r"1e-3")
  case object Gram extends IntrinsicMassUnit("Gram", Seq("g"), r"1" * r"1e-3")
  case object DecaGram extends IntrinsicMassUnit("DecaGram", Seq("dag"), r"1e1" * r"1e-3")
  case object HectoGram extends IntrinsicMassUnit("HectoGram", Seq("hg"), r"1e2" * r"1e-3")
  case object KiloGram extends IntrinsicMassUnit("KiloGram", Seq("kg"), r"1e3" * r"1e-3")
  case object MegaGram extends IntrinsicMassUnit("MegaGram", Seq("Mg"), r"1e6" * r"1e-3")
  case object GigaGram extends IntrinsicMassUnit("GigaGram", Seq("Gg"), r"1e9" * r"1e-3")
  case object TeraGram extends IntrinsicMassUnit("TeraGram", Seq("Tg"), r"1e12" * r"1e-3")
  case object PetaGram extends IntrinsicMassUnit("PetaGram", Seq("Pg"), r"1e15" * r"1e-3")
  case object ExaGram extends IntrinsicMassUnit("ExaGram", Seq("Eg"), r"1e18" * r"1e-3")
  case object ZettaGram extends IntrinsicMassUnit("ZettaGram", Seq("Zg"), r"1e21" * r"1e-3")
  case object YottaGram extends IntrinsicMassUnit("YottaGram", Seq("Yg"), r"1e24" * r"1e-3")
  case object Grave extends IntrinsicMassUnit("Grave", Seq("gv"), 1, KiloGram)
  case object Tonne extends IntrinsicMassUnit("Tonne", Seq("t"), 1000)
  case object Gamma extends IntrinsicMassUnit("Gamma", Seq("γ", "gamma"), 1, MicroGram)
  case object Quintal extends IntrinsicMassUnit("Quintal", Seq("q"), 100, KiloGram)
  case object AtomicMassUnit extends IntrinsicMassUnit("AtomicMassUnit", Seq("u", "AMU", "Da"), r"1.66053892173e-27") with NotExact
  case object ElectronMass extends IntrinsicMassUnit("ElectronMass", Seq("m_e"), r"9.1093829140e-31") with NotExact
  case object Ounce extends IntrinsicMassUnit("Ounce", Seq("oz"), 28, Gram)
  case object Pound extends IntrinsicMassUnit("Pound", Seq("lb", "lb_av"), r"0.45359237")
  case object LongTon extends IntrinsicMassUnit("LongTon", Seq("long_tn", "ton"), 2240, Pound)
  case object ShortTon extends IntrinsicMassUnit("ShortTon", Seq("sh_tn"), 2000, Pound)
  case object Scruple extends IntrinsicMassUnit("Scruple", Seq("s_ap"), 20, Grain)
  case object Carat extends IntrinsicMassUnit("Carat", Seq("kt"), r"19/6", Grain)
  case object MetricCarat extends IntrinsicMassUnit("MetricCarat", Seq("ct"), 200, MilliGram)
  case object Stone extends IntrinsicMassUnit("Stone", Seq("st"), 14, Pound)
  case object Dram_avoirdupois extends IntrinsicMassUnit("Dram_avoirdupois", Seq("dr_av"), 27 + r"11/32", Grain)
  case object Grain extends IntrinsicMassUnit("Grain", Seq("gr"), r"1/7000", Pound)
  case object LongHundredweight extends IntrinsicMassUnit("LongHundredweight", Seq("cwt", "long_cwt"), 112, Dram_avoirdupois)
  case object ShortHundredweight extends IntrinsicMassUnit("ShortHundredweight", Seq("sh_cwt"), 100, Dram_avoirdupois)
  case object Kip extends IntrinsicMassUnit("Kip", Seq("kip"), 1000, Dram_avoirdupois)
  case object Ounce_avoirdupois extends IntrinsicMassUnit("Ounce_avoirdupois", Seq("oz_av"), r"1/16", Pound)
  case object Dram_troy extends IntrinsicMassUnit("Dram_troy", Seq("dr_t"), 60, Grain)
  case object Ounce_troy extends IntrinsicMassUnit("Ounce_troy", Seq("oz_t"), r"1/12", Pound_troy)
  case object Pound_troy extends IntrinsicMassUnit("Pound_troy", Seq("lb_t"), 5760, Grain)
  case object Pennyweight extends IntrinsicMassUnit("Pennyweight", Seq("dwt", "pwt"), r"1/20", Ounce_troy)
  case object LongAssayTon extends IntrinsicMassUnit("LongAssayTon", Seq("long_AT", "AT"), r"98/3", Gram)
  case object ShortAssayTon extends IntrinsicMassUnit("ShortAssayTon", Seq("sh_AT"), r"175/6", Gram)
  case object Slug extends IntrinsicMassUnit("Slug", Seq("slug"), AccelerationUnit.StandardGravity.unitInMetrePerSecondSquared / LengthUnit.Foot.unitInMetre)

  override lazy val values = Seq(YoctoGram, ZeptoGram, AttoGram, FemtoGram, PicoGram, NanoGram, MicroGram, MilliGram, CentiGram, DeciGram, Gram, DecaGram, HectoGram, KiloGram, MegaGram, GigaGram, TeraGram, PetaGram, ExaGram, ZettaGram, YottaGram, Grave, Tonne, Gamma, Quintal, AtomicMassUnit, ElectronMass, Ounce, Pound, LongTon, ShortTon, Scruple, Carat, MetricCarat, Stone, Dram_avoirdupois, Grain, LongHundredweight, ShortHundredweight, Kip, Ounce_avoirdupois, Dram_troy, Ounce_troy, Pound_troy, Pennyweight, LongAssayTon, ShortAssayTon, Slug)
}

trait MultiplicativeByMassUnit[R]{
  def *(unit: MassUnit): R
}

trait DivisibleByMassUnit[R]{
  def /(unit: MassUnit): R
}

trait MassPostfixOps[A]{
  import MassUnit._

  protected def massPostfixOps(unit: MassUnit): A


  def yg : A = massPostfixOps(YoctoGram)
  def zg : A = massPostfixOps(ZeptoGram)
  def ag : A = massPostfixOps(AttoGram)
  def fg : A = massPostfixOps(FemtoGram)
  def pg : A = massPostfixOps(PicoGram)
  def ng : A = massPostfixOps(NanoGram)
  def microGram : A = massPostfixOps(MicroGram)
  def μg : A = massPostfixOps(MicroGram)
  def mg : A = massPostfixOps(MilliGram)
  def cg : A = massPostfixOps(CentiGram)
  def dg : A = massPostfixOps(DeciGram)
  def g : A = massPostfixOps(Gram)
  def dag : A = massPostfixOps(DecaGram)
  def hg : A = massPostfixOps(HectoGram)
  def kg : A = massPostfixOps(KiloGram)
  def Mg : A = massPostfixOps(MegaGram)
  def Gg : A = massPostfixOps(GigaGram)
  def Tg : A = massPostfixOps(TeraGram)
  def Pg : A = massPostfixOps(PetaGram)
  def Eg : A = massPostfixOps(ExaGram)
  def Zg : A = massPostfixOps(ZettaGram)
  def Yg : A = massPostfixOps(YottaGram)
  def gv : A = massPostfixOps(Grave)
  def t : A = massPostfixOps(Tonne)
  def γ : A = massPostfixOps(Gamma)
  def gamma : A = massPostfixOps(Gamma)
  def q : A = massPostfixOps(Quintal)
  def u : A = massPostfixOps(AtomicMassUnit)
  def AMU : A = massPostfixOps(AtomicMassUnit)
  def Da : A = massPostfixOps(AtomicMassUnit)
  def m_e : A = massPostfixOps(ElectronMass)
  def oz : A = massPostfixOps(Ounce)
  def lb : A = massPostfixOps(Pound)
  def lb_av : A = massPostfixOps(Pound)
  def long_tn : A = massPostfixOps(LongTon)
  def ton : A = massPostfixOps(LongTon)
  def sh_tn : A = massPostfixOps(ShortTon)
  def s_ap : A = massPostfixOps(Scruple)
  def kt : A = massPostfixOps(Carat)
  def ct : A = massPostfixOps(MetricCarat)
  def st : A = massPostfixOps(Stone)
  def dr_av : A = massPostfixOps(Dram_avoirdupois)
  def gr : A = massPostfixOps(Grain)
  def cwt : A = massPostfixOps(LongHundredweight)
  def long_cwt : A = massPostfixOps(LongHundredweight)
  def sh_cwt : A = massPostfixOps(ShortHundredweight)
  def kip : A = massPostfixOps(Kip)
  def oz_av : A = massPostfixOps(Ounce_avoirdupois)
  def dr_t : A = massPostfixOps(Dram_troy)
  def oz_t : A = massPostfixOps(Ounce_troy)
  def lb_t : A = massPostfixOps(Pound_troy)
  def dwt : A = massPostfixOps(Pennyweight)
  def pwt : A = massPostfixOps(Pennyweight)
  def long_AT : A = massPostfixOps(LongAssayTon)
  def AT : A = massPostfixOps(LongAssayTon)
  def sh_AT : A = massPostfixOps(ShortAssayTon)
  def slug : A = massPostfixOps(Slug)
}

trait MassDot[A]{
  import MassUnit._

  protected def massDot(unit: MassUnit): A

  def yg(dot: Dot): A = massDot(YoctoGram)
  def zg(dot: Dot): A = massDot(ZeptoGram)
  def ag(dot: Dot): A = massDot(AttoGram)
  def fg(dot: Dot): A = massDot(FemtoGram)
  def pg(dot: Dot): A = massDot(PicoGram)
  def ng(dot: Dot): A = massDot(NanoGram)
  def microGram(dot: Dot): A = massDot(MicroGram)
  def μg(dot: Dot): A = massDot(MicroGram)
  def mg(dot: Dot): A = massDot(MilliGram)
  def cg(dot: Dot): A = massDot(CentiGram)
  def dg(dot: Dot): A = massDot(DeciGram)
  def g(dot: Dot): A = massDot(Gram)
  def dag(dot: Dot): A = massDot(DecaGram)
  def hg(dot: Dot): A = massDot(HectoGram)
  def kg(dot: Dot): A = massDot(KiloGram)
  def Mg(dot: Dot): A = massDot(MegaGram)
  def Gg(dot: Dot): A = massDot(GigaGram)
  def Tg(dot: Dot): A = massDot(TeraGram)
  def Pg(dot: Dot): A = massDot(PetaGram)
  def Eg(dot: Dot): A = massDot(ExaGram)
  def Zg(dot: Dot): A = massDot(ZettaGram)
  def Yg(dot: Dot): A = massDot(YottaGram)
  def gv(dot: Dot): A = massDot(Grave)
  def t(dot: Dot): A = massDot(Tonne)
  def γ(dot: Dot): A = massDot(Gamma)
  def gamma(dot: Dot): A = massDot(Gamma)
  def q(dot: Dot): A = massDot(Quintal)
  def u(dot: Dot): A = massDot(AtomicMassUnit)
  def AMU(dot: Dot): A = massDot(AtomicMassUnit)
  def Da(dot: Dot): A = massDot(AtomicMassUnit)
  def m_e(dot: Dot): A = massDot(ElectronMass)
  def oz(dot: Dot): A = massDot(Ounce)
  def lb(dot: Dot): A = massDot(Pound)
  def lb_av(dot: Dot): A = massDot(Pound)
  def long_tn(dot: Dot): A = massDot(LongTon)
  def ton(dot: Dot): A = massDot(LongTon)
  def sh_tn(dot: Dot): A = massDot(ShortTon)
  def s_ap(dot: Dot): A = massDot(Scruple)
  def kt(dot: Dot): A = massDot(Carat)
  def ct(dot: Dot): A = massDot(MetricCarat)
  def st(dot: Dot): A = massDot(Stone)
  def dr_av(dot: Dot): A = massDot(Dram_avoirdupois)
  def gr(dot: Dot): A = massDot(Grain)
  def cwt(dot: Dot): A = massDot(LongHundredweight)
  def long_cwt(dot: Dot): A = massDot(LongHundredweight)
  def sh_cwt(dot: Dot): A = massDot(ShortHundredweight)
  def kip(dot: Dot): A = massDot(Kip)
  def oz_av(dot: Dot): A = massDot(Ounce_avoirdupois)
  def dr_t(dot: Dot): A = massDot(Dram_troy)
  def oz_t(dot: Dot): A = massDot(Ounce_troy)
  def lb_t(dot: Dot): A = massDot(Pound_troy)
  def dwt(dot: Dot): A = massDot(Pennyweight)
  def pwt(dot: Dot): A = massDot(Pennyweight)
  def long_AT(dot: Dot): A = massDot(LongAssayTon)
  def AT(dot: Dot): A = massDot(LongAssayTon)
  def sh_AT(dot: Dot): A = massDot(ShortAssayTon)
  def slug(dot: Dot): A = massDot(Slug)
}

trait MassPer[A]{
  import MassUnit._

  protected def massPer(unit: MassUnit): A

  def yg(per: Per): A = massPer(YoctoGram)
  def zg(per: Per): A = massPer(ZeptoGram)
  def ag(per: Per): A = massPer(AttoGram)
  def fg(per: Per): A = massPer(FemtoGram)
  def pg(per: Per): A = massPer(PicoGram)
  def ng(per: Per): A = massPer(NanoGram)
  def microGram(per: Per): A = massPer(MicroGram)
  def μg(per: Per): A = massPer(MicroGram)
  def mg(per: Per): A = massPer(MilliGram)
  def cg(per: Per): A = massPer(CentiGram)
  def dg(per: Per): A = massPer(DeciGram)
  def g(per: Per): A = massPer(Gram)
  def dag(per: Per): A = massPer(DecaGram)
  def hg(per: Per): A = massPer(HectoGram)
  def kg(per: Per): A = massPer(KiloGram)
  def Mg(per: Per): A = massPer(MegaGram)
  def Gg(per: Per): A = massPer(GigaGram)
  def Tg(per: Per): A = massPer(TeraGram)
  def Pg(per: Per): A = massPer(PetaGram)
  def Eg(per: Per): A = massPer(ExaGram)
  def Zg(per: Per): A = massPer(ZettaGram)
  def Yg(per: Per): A = massPer(YottaGram)
  def gv(per: Per): A = massPer(Grave)
  def t(per: Per): A = massPer(Tonne)
  def γ(per: Per): A = massPer(Gamma)
  def gamma(per: Per): A = massPer(Gamma)
  def q(per: Per): A = massPer(Quintal)
  def u(per: Per): A = massPer(AtomicMassUnit)
  def AMU(per: Per): A = massPer(AtomicMassUnit)
  def Da(per: Per): A = massPer(AtomicMassUnit)
  def m_e(per: Per): A = massPer(ElectronMass)
  def oz(per: Per): A = massPer(Ounce)
  def lb(per: Per): A = massPer(Pound)
  def lb_av(per: Per): A = massPer(Pound)
  def long_tn(per: Per): A = massPer(LongTon)
  def ton(per: Per): A = massPer(LongTon)
  def sh_tn(per: Per): A = massPer(ShortTon)
  def s_ap(per: Per): A = massPer(Scruple)
  def kt(per: Per): A = massPer(Carat)
  def ct(per: Per): A = massPer(MetricCarat)
  def st(per: Per): A = massPer(Stone)
  def dr_av(per: Per): A = massPer(Dram_avoirdupois)
  def gr(per: Per): A = massPer(Grain)
  def cwt(per: Per): A = massPer(LongHundredweight)
  def long_cwt(per: Per): A = massPer(LongHundredweight)
  def sh_cwt(per: Per): A = massPer(ShortHundredweight)
  def kip(per: Per): A = massPer(Kip)
  def oz_av(per: Per): A = massPer(Ounce_avoirdupois)
  def dr_t(per: Per): A = massPer(Dram_troy)
  def oz_t(per: Per): A = massPer(Ounce_troy)
  def lb_t(per: Per): A = massPer(Pound_troy)
  def dwt(per: Per): A = massPer(Pennyweight)
  def pwt(per: Per): A = massPer(Pennyweight)
  def long_AT(per: Per): A = massPer(LongAssayTon)
  def AT(per: Per): A = massPer(LongAssayTon)
  def sh_AT(per: Per): A = massPer(ShortAssayTon)
  def slug(per: Per): A = massPer(Slug)
}

trait PredefinedMassUnit extends MassPostfixOps[MassUnit]{
  override protected def massPostfixOps(unit: MassUnit) = unit
  
}

object PredefinedMassUnit extends PredefinedMassUnit
