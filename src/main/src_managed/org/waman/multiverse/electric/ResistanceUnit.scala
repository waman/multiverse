package org.waman.multiverse.electric

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._


sealed trait ResistanceUnit extends PhysicalUnit[ResistanceUnit]{

  def unitInOhm: Real

  override def baseUnit = org.waman.multiverse.electric.ResistanceUnit.Ohm
  override def valueInBaseUnit = unitInOhm
}

object ResistanceUnit extends ConstantsDefined[ResistanceUnit]{

  // intrinsic
  private[ResistanceUnit]
  class IntrinsicResistanceUnit(name: String, val symbols: Seq[String], val unitInOhm: Real)
      extends ResistanceUnit{

    def this(name: String, symbols: Seq[String], unit: ResistanceUnit) =
      this(name, symbols, unit.unitInOhm)

    def this(name: String, symbols: Seq[String], factor: Real, unit: ResistanceUnit) =
      this(name, symbols, factor * unit.unitInOhm)
  }


  case object YoctoOhm extends IntrinsicResistanceUnit("YoctoOhm", Seq("yΩ"), r"1e-24")
  case object ZeptoOhm extends IntrinsicResistanceUnit("ZeptoOhm", Seq("zΩ"), r"1e-21")
  case object AttoOhm extends IntrinsicResistanceUnit("AttoOhm", Seq("aΩ"), r"1e-18")
  case object FemtoOhm extends IntrinsicResistanceUnit("FemtoOhm", Seq("fΩ"), r"1e-15")
  case object PicoOhm extends IntrinsicResistanceUnit("PicoOhm", Seq("pΩ"), r"1e-12")
  case object NanoOhm extends IntrinsicResistanceUnit("NanoOhm", Seq("nΩ"), r"1e-9")
  case object MicroOhm extends IntrinsicResistanceUnit("MicroOhm", Seq("microOhm", "microΩ", "μΩ"), r"1e-6")
  case object MilliOhm extends IntrinsicResistanceUnit("MilliOhm", Seq("mΩ"), r"1e-3")
  case object CentiOhm extends IntrinsicResistanceUnit("CentiOhm", Seq("cΩ"), r"1e-2")
  case object DeciOhm extends IntrinsicResistanceUnit("DeciOhm", Seq("dΩ"), r"1e-1")
  case object Ohm extends IntrinsicResistanceUnit("Ohm", Seq("Ω"), r"1")
  case object DecaOhm extends IntrinsicResistanceUnit("DecaOhm", Seq("daΩ"), r"1e1")
  case object HectoOhm extends IntrinsicResistanceUnit("HectoOhm", Seq("hΩ"), r"1e2")
  case object KiloOhm extends IntrinsicResistanceUnit("KiloOhm", Seq("kΩ"), r"1e3")
  case object MegaOhm extends IntrinsicResistanceUnit("MegaOhm", Seq("MΩ"), r"1e6")
  case object GigaOhm extends IntrinsicResistanceUnit("GigaOhm", Seq("GΩ"), r"1e9")
  case object TeraOhm extends IntrinsicResistanceUnit("TeraOhm", Seq("TΩ"), r"1e12")
  case object PetaOhm extends IntrinsicResistanceUnit("PetaOhm", Seq("PΩ"), r"1e15")
  case object ExaOhm extends IntrinsicResistanceUnit("ExaOhm", Seq("EΩ"), r"1e18")
  case object ZettaOhm extends IntrinsicResistanceUnit("ZettaOhm", Seq("ZΩ"), r"1e21")
  case object YottaOhm extends IntrinsicResistanceUnit("YottaOhm", Seq("YΩ"), r"1e24")

  override lazy val values = Seq(YoctoOhm, ZeptoOhm, AttoOhm, FemtoOhm, PicoOhm, NanoOhm, MicroOhm, MilliOhm, CentiOhm, DeciOhm, Ohm, DecaOhm, HectoOhm, KiloOhm, MegaOhm, GigaOhm, TeraOhm, PetaOhm, ExaOhm, ZettaOhm, YottaOhm)

  // VoltageUnit / CurrentUnit -> Resistance
  private[ResistanceUnit]
  class QuotientVoltagePerCurrentUnit(val numeratorUnit: VoltageUnit, val denominatorUnit: CurrentUnit)
      extends ResistanceUnit with QuotientUnit[ResistanceUnit, VoltageUnit, CurrentUnit]{

    override lazy val unitInOhm: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: VoltageUnit, dUnit: CurrentUnit): ResistanceUnit =
    new QuotientVoltagePerCurrentUnit(nUnit, dUnit)
}

trait MultiplicativeByResistanceUnit[R]{
  def *(unit: ResistanceUnit): R
}

trait DivisibleByResistanceUnit[R]{
  def /(unit: ResistanceUnit): R
}

trait ResistancePostfixOps[A]{
  import ResistanceUnit._

  protected def resistancePostfixOps(unit: ResistanceUnit): A


  def yΩ : A = resistancePostfixOps(YoctoOhm)
  def zΩ : A = resistancePostfixOps(ZeptoOhm)
  def aΩ : A = resistancePostfixOps(AttoOhm)
  def fΩ : A = resistancePostfixOps(FemtoOhm)
  def pΩ : A = resistancePostfixOps(PicoOhm)
  def nΩ : A = resistancePostfixOps(NanoOhm)
  def microOhm : A = resistancePostfixOps(MicroOhm)
  def microΩ : A = resistancePostfixOps(MicroOhm)
  def μΩ : A = resistancePostfixOps(MicroOhm)
  def mΩ : A = resistancePostfixOps(MilliOhm)
  def cΩ : A = resistancePostfixOps(CentiOhm)
  def dΩ : A = resistancePostfixOps(DeciOhm)
  def Ω : A = resistancePostfixOps(Ohm)
  def daΩ : A = resistancePostfixOps(DecaOhm)
  def hΩ : A = resistancePostfixOps(HectoOhm)
  def kΩ : A = resistancePostfixOps(KiloOhm)
  def MΩ : A = resistancePostfixOps(MegaOhm)
  def GΩ : A = resistancePostfixOps(GigaOhm)
  def TΩ : A = resistancePostfixOps(TeraOhm)
  def PΩ : A = resistancePostfixOps(PetaOhm)
  def EΩ : A = resistancePostfixOps(ExaOhm)
  def ZΩ : A = resistancePostfixOps(ZettaOhm)
  def YΩ : A = resistancePostfixOps(YottaOhm)
}

trait ResistanceDot[A]{
  import ResistanceUnit._

  protected def resistanceDot(unit: ResistanceUnit): A

  def yΩ(dot: Dot): A = resistanceDot(YoctoOhm)
  def zΩ(dot: Dot): A = resistanceDot(ZeptoOhm)
  def aΩ(dot: Dot): A = resistanceDot(AttoOhm)
  def fΩ(dot: Dot): A = resistanceDot(FemtoOhm)
  def pΩ(dot: Dot): A = resistanceDot(PicoOhm)
  def nΩ(dot: Dot): A = resistanceDot(NanoOhm)
  def microOhm(dot: Dot): A = resistanceDot(MicroOhm)
  def microΩ(dot: Dot): A = resistanceDot(MicroOhm)
  def μΩ(dot: Dot): A = resistanceDot(MicroOhm)
  def mΩ(dot: Dot): A = resistanceDot(MilliOhm)
  def cΩ(dot: Dot): A = resistanceDot(CentiOhm)
  def dΩ(dot: Dot): A = resistanceDot(DeciOhm)
  def Ω(dot: Dot): A = resistanceDot(Ohm)
  def daΩ(dot: Dot): A = resistanceDot(DecaOhm)
  def hΩ(dot: Dot): A = resistanceDot(HectoOhm)
  def kΩ(dot: Dot): A = resistanceDot(KiloOhm)
  def MΩ(dot: Dot): A = resistanceDot(MegaOhm)
  def GΩ(dot: Dot): A = resistanceDot(GigaOhm)
  def TΩ(dot: Dot): A = resistanceDot(TeraOhm)
  def PΩ(dot: Dot): A = resistanceDot(PetaOhm)
  def EΩ(dot: Dot): A = resistanceDot(ExaOhm)
  def ZΩ(dot: Dot): A = resistanceDot(ZettaOhm)
  def YΩ(dot: Dot): A = resistanceDot(YottaOhm)
}

trait ResistancePer[A]{
  import ResistanceUnit._

  protected def resistancePer(unit: ResistanceUnit): A

  def yΩ(per: Per): A = resistancePer(YoctoOhm)
  def zΩ(per: Per): A = resistancePer(ZeptoOhm)
  def aΩ(per: Per): A = resistancePer(AttoOhm)
  def fΩ(per: Per): A = resistancePer(FemtoOhm)
  def pΩ(per: Per): A = resistancePer(PicoOhm)
  def nΩ(per: Per): A = resistancePer(NanoOhm)
  def microOhm(per: Per): A = resistancePer(MicroOhm)
  def microΩ(per: Per): A = resistancePer(MicroOhm)
  def μΩ(per: Per): A = resistancePer(MicroOhm)
  def mΩ(per: Per): A = resistancePer(MilliOhm)
  def cΩ(per: Per): A = resistancePer(CentiOhm)
  def dΩ(per: Per): A = resistancePer(DeciOhm)
  def Ω(per: Per): A = resistancePer(Ohm)
  def daΩ(per: Per): A = resistancePer(DecaOhm)
  def hΩ(per: Per): A = resistancePer(HectoOhm)
  def kΩ(per: Per): A = resistancePer(KiloOhm)
  def MΩ(per: Per): A = resistancePer(MegaOhm)
  def GΩ(per: Per): A = resistancePer(GigaOhm)
  def TΩ(per: Per): A = resistancePer(TeraOhm)
  def PΩ(per: Per): A = resistancePer(PetaOhm)
  def EΩ(per: Per): A = resistancePer(ExaOhm)
  def ZΩ(per: Per): A = resistancePer(ZettaOhm)
  def YΩ(per: Per): A = resistancePer(YottaOhm)
}

trait PredefinedResistanceUnit extends ResistancePostfixOps[ResistanceUnit]{
  override protected def resistancePostfixOps(unit: ResistanceUnit) = unit
  
}

object PredefinedResistanceUnit extends PredefinedResistanceUnit
