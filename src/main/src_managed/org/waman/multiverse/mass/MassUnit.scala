package org.waman.multiverse.mass

import org.waman.multiverse._
import org.waman.multiverse.metric.VolumeUnit
import spire.implicits._
import spire.math.Real

sealed trait MassUnit extends PhysicalUnit[MassUnit]
  with DivisibleByVolumeUnit[DensityUnit]{

  def unitInKiloGram: Real

  override def baseUnit = org.waman.multiverse.mass.MassUnit.KiloGram
  override def valueInBaseUnit = unitInKiloGram

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
    
  case object DecaGram extends IntrinsicMassUnit("DecaGram", Seq("dag"), r"1e-1" * r"1e-3")
    
  case object HectoGram extends IntrinsicMassUnit("HectoGram", Seq("hg"), r"1e-2" * r"1e-3")
    
  case object KiloGram extends IntrinsicMassUnit("KiloGram", Seq("kg"), r"1e-3" * r"1e-3")
    
  case object MegaGram extends IntrinsicMassUnit("MegaGram", Seq("Mg"), r"1e-6" * r"1e-3")
    
  case object GigaGram extends IntrinsicMassUnit("GigaGram", Seq("Gg"), r"1e-9" * r"1e-3")
    
  case object TeraGram extends IntrinsicMassUnit("TeraGram", Seq("Tg"), r"1e-12" * r"1e-3")
    
  case object PetaGram extends IntrinsicMassUnit("PetaGram", Seq("Pg"), r"1e-15" * r"1e-3")
    
  case object ExaGram extends IntrinsicMassUnit("ExaGram", Seq("Eg"), r"1e-18" * r"1e-3")
    
  case object ZettaGram extends IntrinsicMassUnit("ZettaGram", Seq("Zg"), r"1e-21" * r"1e-3")
    
  case object YottaGram extends IntrinsicMassUnit("YottaGram", Seq("Yg"), r"1e-24" * r"1e-3")
    
  case object AtomicMassUnit extends IntrinsicMassUnit("AtomicMassUnit", Seq("u", "AMU", "Da"), r"1.66053892173e-27") with NotExact
    
  case object ElectronMass extends IntrinsicMassUnit("ElectronMass", Seq("m_e"), r"9.1093829140e-31") with NotExact
    
  case object Grain extends IntrinsicMassUnit("Grain", Seq("gr"), 6479891, MilliGram)
    
  case object Ounce extends IntrinsicMassUnit("Ounce", Seq("oz"), 28, Gram)
    
  case object Pound extends IntrinsicMassUnit("Pound", Seq("lb"), r"0.45359237")
    
  case object Tonne extends IntrinsicMassUnit("Tonne", Seq("t"), 1000)
    
  case object Carat extends IntrinsicMassUnit("Carat", Seq("kt"), r"19/6", Grain)
    
  case object MetricCarat extends IntrinsicMassUnit("MetricCarat", Seq("ct"), 200, MilliGram)
    

  override lazy val values = Seq(YoctoGram, ZeptoGram, AttoGram, FemtoGram, PicoGram, NanoGram, MicroGram, MilliGram, CentiGram, DeciGram, Gram, DecaGram, HectoGram, KiloGram, MegaGram, GigaGram, TeraGram, PetaGram, ExaGram, ZettaGram, YottaGram, AtomicMassUnit, ElectronMass, Grain, Ounce, Pound, Tonne, Carat, MetricCarat)
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
  def u : A = massPostfixOps(AtomicMassUnit)
  def AMU : A = massPostfixOps(AtomicMassUnit)
  def Da : A = massPostfixOps(AtomicMassUnit)
  def m_e : A = massPostfixOps(ElectronMass)
  def gr : A = massPostfixOps(Grain)
  def oz : A = massPostfixOps(Ounce)
  def lb : A = massPostfixOps(Pound)
  def t : A = massPostfixOps(Tonne)
  def kt : A = massPostfixOps(Carat)
  def ct : A = massPostfixOps(MetricCarat)
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
  def u(dot: Dot): A = massDot(AtomicMassUnit)
  def AMU(dot: Dot): A = massDot(AtomicMassUnit)
  def Da(dot: Dot): A = massDot(AtomicMassUnit)
  def m_e(dot: Dot): A = massDot(ElectronMass)
  def gr(dot: Dot): A = massDot(Grain)
  def oz(dot: Dot): A = massDot(Ounce)
  def lb(dot: Dot): A = massDot(Pound)
  def t(dot: Dot): A = massDot(Tonne)
  def kt(dot: Dot): A = massDot(Carat)
  def ct(dot: Dot): A = massDot(MetricCarat)
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
  def u(per: Per): A = massPer(AtomicMassUnit)
  def AMU(per: Per): A = massPer(AtomicMassUnit)
  def Da(per: Per): A = massPer(AtomicMassUnit)
  def m_e(per: Per): A = massPer(ElectronMass)
  def gr(per: Per): A = massPer(Grain)
  def oz(per: Per): A = massPer(Ounce)
  def lb(per: Per): A = massPer(Pound)
  def t(per: Per): A = massPer(Tonne)
  def kt(per: Per): A = massPer(Carat)
  def ct(per: Per): A = massPer(MetricCarat)
}

trait PredefinedMassUnit extends MassPostfixOps[MassUnit]{
  override protected def massPostfixOps(unit: MassUnit) = unit
  
}

object PredefinedMassUnit extends PredefinedMassUnit
