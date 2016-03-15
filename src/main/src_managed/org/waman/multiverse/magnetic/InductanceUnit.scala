package org.waman.multiverse.magnetic

import org.waman.multiverse._
import org.waman.multiverse.electric.CurrentUnit
import spire.implicits._
import spire.math.Real

sealed trait InductanceUnit extends PhysicalUnit[InductanceUnit]{

  def unitInHenry: Real

  override def baseUnit = org.waman.multiverse.magnetic.InductanceUnit.Henry
  override def valueInBaseUnit = unitInHenry
}

object InductanceUnit extends ConstantsDefined[InductanceUnit]{

  // intrinsic
  private[InductanceUnit]
  class IntrinsicInductanceUnit(name: String, val symbols: Seq[String], val unitInHenry: Real)
      extends InductanceUnit{

    def this(name: String, symbols: Seq[String], unit: InductanceUnit) =
      this(name, symbols, unit.unitInHenry)

    def this(name: String, symbols: Seq[String], factor: Real, unit: InductanceUnit) =
      this(name, symbols, factor * unit.unitInHenry)
  }


  case object YoctoHenry extends IntrinsicInductanceUnit("YoctoHenry", Seq("yH"), r"1e-24")
  case object ZeptoHenry extends IntrinsicInductanceUnit("ZeptoHenry", Seq("zH"), r"1e-21")
  case object AttoHenry extends IntrinsicInductanceUnit("AttoHenry", Seq("aH"), r"1e-18")
  case object FemtoHenry extends IntrinsicInductanceUnit("FemtoHenry", Seq("fH"), r"1e-15")
  case object PicoHenry extends IntrinsicInductanceUnit("PicoHenry", Seq("pH"), r"1e-12")
  case object NanoHenry extends IntrinsicInductanceUnit("NanoHenry", Seq("nH"), r"1e-9")
  case object MicroHenry extends IntrinsicInductanceUnit("MicroHenry", Seq("microHenry", "microH", "μH"), r"1e-6")
  case object MilliHenry extends IntrinsicInductanceUnit("MilliHenry", Seq("mH"), r"1e-3")
  case object CentiHenry extends IntrinsicInductanceUnit("CentiHenry", Seq("cH"), r"1e-2")
  case object DeciHenry extends IntrinsicInductanceUnit("DeciHenry", Seq("dH"), r"1e-1")
  case object Henry extends IntrinsicInductanceUnit("Henry", Seq("H"), r"1")
  case object DecaHenry extends IntrinsicInductanceUnit("DecaHenry", Seq("daH"), r"1e1")
  case object HectoHenry extends IntrinsicInductanceUnit("HectoHenry", Seq("hH"), r"1e2")
  case object KiloHenry extends IntrinsicInductanceUnit("KiloHenry", Seq("kH"), r"1e3")
  case object MegaHenry extends IntrinsicInductanceUnit("MegaHenry", Seq("MH"), r"1e6")
  case object GigaHenry extends IntrinsicInductanceUnit("GigaHenry", Seq("GH"), r"1e9")
  case object TeraHenry extends IntrinsicInductanceUnit("TeraHenry", Seq("TH"), r"1e12")
  case object PetaHenry extends IntrinsicInductanceUnit("PetaHenry", Seq("PH"), r"1e15")
  case object ExaHenry extends IntrinsicInductanceUnit("ExaHenry", Seq("EH"), r"1e18")
  case object ZettaHenry extends IntrinsicInductanceUnit("ZettaHenry", Seq("ZH"), r"1e21")
  case object YottaHenry extends IntrinsicInductanceUnit("YottaHenry", Seq("YH"), r"1e24")

  override lazy val values = Seq(YoctoHenry, ZeptoHenry, AttoHenry, FemtoHenry, PicoHenry, NanoHenry, MicroHenry, MilliHenry, CentiHenry, DeciHenry, Henry, DecaHenry, HectoHenry, KiloHenry, MegaHenry, GigaHenry, TeraHenry, PetaHenry, ExaHenry, ZettaHenry, YottaHenry)

  // FluxUnit / CurrentUnit -> Inductance
  private[InductanceUnit]
  class QuotientFluxPerCurrentUnit(val numeratorUnit: FluxUnit, val denominatorUnit: CurrentUnit)
      extends InductanceUnit with QuotientUnit[InductanceUnit, FluxUnit, CurrentUnit]{

    override lazy val unitInHenry: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: FluxUnit, dUnit: CurrentUnit): InductanceUnit =
    new QuotientFluxPerCurrentUnit(nUnit, dUnit)
}

trait InductancePostfixOps[A]{
  import InductanceUnit._

  protected def inductancePostfixOps(unit: InductanceUnit): A

  def yH : A = inductancePostfixOps(YoctoHenry)
  def zH : A = inductancePostfixOps(ZeptoHenry)
  def aH : A = inductancePostfixOps(AttoHenry)
  def fH : A = inductancePostfixOps(FemtoHenry)
  def pH : A = inductancePostfixOps(PicoHenry)
  def nH : A = inductancePostfixOps(NanoHenry)
  def microHenry : A = inductancePostfixOps(MicroHenry)
  def microH : A = inductancePostfixOps(MicroHenry)
  def μH : A = inductancePostfixOps(MicroHenry)
  def mH : A = inductancePostfixOps(MilliHenry)
  def cH : A = inductancePostfixOps(CentiHenry)
  def dH : A = inductancePostfixOps(DeciHenry)
  def H : A = inductancePostfixOps(Henry)
  def daH : A = inductancePostfixOps(DecaHenry)
  def hH : A = inductancePostfixOps(HectoHenry)
  def kH : A = inductancePostfixOps(KiloHenry)
  def MH : A = inductancePostfixOps(MegaHenry)
  def GH : A = inductancePostfixOps(GigaHenry)
  def TH : A = inductancePostfixOps(TeraHenry)
  def PH : A = inductancePostfixOps(PetaHenry)
  def EH : A = inductancePostfixOps(ExaHenry)
  def ZH : A = inductancePostfixOps(ZettaHenry)
  def YH : A = inductancePostfixOps(YottaHenry)
}

trait InductanceDot[A]{
  import InductanceUnit._

  protected def inductanceDot(unit: InductanceUnit): A

  def yH(dot: Dot): A = inductanceDot(YoctoHenry)
  def zH(dot: Dot): A = inductanceDot(ZeptoHenry)
  def aH(dot: Dot): A = inductanceDot(AttoHenry)
  def fH(dot: Dot): A = inductanceDot(FemtoHenry)
  def pH(dot: Dot): A = inductanceDot(PicoHenry)
  def nH(dot: Dot): A = inductanceDot(NanoHenry)
  def microHenry(dot: Dot): A = inductanceDot(MicroHenry)
  def microH(dot: Dot): A = inductanceDot(MicroHenry)
  def μH(dot: Dot): A = inductanceDot(MicroHenry)
  def mH(dot: Dot): A = inductanceDot(MilliHenry)
  def cH(dot: Dot): A = inductanceDot(CentiHenry)
  def dH(dot: Dot): A = inductanceDot(DeciHenry)
  def H(dot: Dot): A = inductanceDot(Henry)
  def daH(dot: Dot): A = inductanceDot(DecaHenry)
  def hH(dot: Dot): A = inductanceDot(HectoHenry)
  def kH(dot: Dot): A = inductanceDot(KiloHenry)
  def MH(dot: Dot): A = inductanceDot(MegaHenry)
  def GH(dot: Dot): A = inductanceDot(GigaHenry)
  def TH(dot: Dot): A = inductanceDot(TeraHenry)
  def PH(dot: Dot): A = inductanceDot(PetaHenry)
  def EH(dot: Dot): A = inductanceDot(ExaHenry)
  def ZH(dot: Dot): A = inductanceDot(ZettaHenry)
  def YH(dot: Dot): A = inductanceDot(YottaHenry)
}

trait InductancePer[A]{
  import InductanceUnit._

  protected def inductancePer(unit: InductanceUnit): A

  def yH(per: Per): A = inductancePer(YoctoHenry)
  def zH(per: Per): A = inductancePer(ZeptoHenry)
  def aH(per: Per): A = inductancePer(AttoHenry)
  def fH(per: Per): A = inductancePer(FemtoHenry)
  def pH(per: Per): A = inductancePer(PicoHenry)
  def nH(per: Per): A = inductancePer(NanoHenry)
  def microHenry(per: Per): A = inductancePer(MicroHenry)
  def microH(per: Per): A = inductancePer(MicroHenry)
  def μH(per: Per): A = inductancePer(MicroHenry)
  def mH(per: Per): A = inductancePer(MilliHenry)
  def cH(per: Per): A = inductancePer(CentiHenry)
  def dH(per: Per): A = inductancePer(DeciHenry)
  def H(per: Per): A = inductancePer(Henry)
  def daH(per: Per): A = inductancePer(DecaHenry)
  def hH(per: Per): A = inductancePer(HectoHenry)
  def kH(per: Per): A = inductancePer(KiloHenry)
  def MH(per: Per): A = inductancePer(MegaHenry)
  def GH(per: Per): A = inductancePer(GigaHenry)
  def TH(per: Per): A = inductancePer(TeraHenry)
  def PH(per: Per): A = inductancePer(PetaHenry)
  def EH(per: Per): A = inductancePer(ExaHenry)
  def ZH(per: Per): A = inductancePer(ZettaHenry)
  def YH(per: Per): A = inductancePer(YottaHenry)
}

trait PredefinedInductanceUnit extends InductancePostfixOps[InductanceUnit]{
  override protected def inductancePostfixOps(unit: InductanceUnit) = unit
  
}

object PredefinedInductanceUnit extends PredefinedInductanceUnit
