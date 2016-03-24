package org.waman.multiverse.magnetic

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._

sealed trait FluxDensityUnit extends PhysicalUnit[FluxDensityUnit]{

  def unitInTesla: Real

  override def baseUnit = org.waman.multiverse.magnetic.FluxDensityUnit.Tesla
  override def valueInBaseUnit = unitInTesla
}

object FluxDensityUnit extends ConstantsDefined[FluxDensityUnit]{

  // intrinsic
  private[FluxDensityUnit]
  class IntrinsicFluxDensityUnit(name: String, val symbols: Seq[String], val unitInTesla: Real)
      extends FluxDensityUnit{

    def this(name: String, symbols: Seq[String], unit: FluxDensityUnit) =
      this(name, symbols, unit.unitInTesla)

    def this(name: String, symbols: Seq[String], factor: Real, unit: FluxDensityUnit) =
      this(name, symbols, factor * unit.unitInTesla)
  }


  case object YoctoTesla extends IntrinsicFluxDensityUnit("YoctoTesla", Seq("yT"), r"1e-24")
  case object ZeptoTesla extends IntrinsicFluxDensityUnit("ZeptoTesla", Seq("zT"), r"1e-21")
  case object AttoTesla extends IntrinsicFluxDensityUnit("AttoTesla", Seq("aT"), r"1e-18")
  case object FemtoTesla extends IntrinsicFluxDensityUnit("FemtoTesla", Seq("fT"), r"1e-15")
  case object PicoTesla extends IntrinsicFluxDensityUnit("PicoTesla", Seq("pT"), r"1e-12")
  case object NanoTesla extends IntrinsicFluxDensityUnit("NanoTesla", Seq("nT"), r"1e-9")
  case object MicroTesla extends IntrinsicFluxDensityUnit("MicroTesla", Seq("microTesla", "microT", "μT"), r"1e-6")
  case object MilliTesla extends IntrinsicFluxDensityUnit("MilliTesla", Seq("mT"), r"1e-3")
  case object CentiTesla extends IntrinsicFluxDensityUnit("CentiTesla", Seq("cT"), r"1e-2")
  case object DeciTesla extends IntrinsicFluxDensityUnit("DeciTesla", Seq("dT"), r"1e-1")
  case object Tesla extends IntrinsicFluxDensityUnit("Tesla", Seq("T"), r"1")
  case object DecaTesla extends IntrinsicFluxDensityUnit("DecaTesla", Seq("daT"), r"1e1")
  case object HectoTesla extends IntrinsicFluxDensityUnit("HectoTesla", Seq("hT"), r"1e2")
  case object KiloTesla extends IntrinsicFluxDensityUnit("KiloTesla", Seq("kT"), r"1e3")
  case object MegaTesla extends IntrinsicFluxDensityUnit("MegaTesla", Seq("MT"), r"1e6")
  case object GigaTesla extends IntrinsicFluxDensityUnit("GigaTesla", Seq("GT"), r"1e9")
  case object TeraTesla extends IntrinsicFluxDensityUnit("TeraTesla", Seq("TT"), r"1e12")
  case object PetaTesla extends IntrinsicFluxDensityUnit("PetaTesla", Seq("PT"), r"1e15")
  case object ExaTesla extends IntrinsicFluxDensityUnit("ExaTesla", Seq("ET"), r"1e18")
  case object ZettaTesla extends IntrinsicFluxDensityUnit("ZettaTesla", Seq("ZT"), r"1e21")
  case object YottaTesla extends IntrinsicFluxDensityUnit("YottaTesla", Seq("YT"), r"1e24")
  case object YoctoGauss extends IntrinsicFluxDensityUnit("YoctoGauss", Seq("yG"), r"1e-24" * r"1e-4")
  case object ZeptoGauss extends IntrinsicFluxDensityUnit("ZeptoGauss", Seq("zG"), r"1e-21" * r"1e-4")
  case object AttoGauss extends IntrinsicFluxDensityUnit("AttoGauss", Seq("aG"), r"1e-18" * r"1e-4")
  case object FemtoGauss extends IntrinsicFluxDensityUnit("FemtoGauss", Seq("fG"), r"1e-15" * r"1e-4")
  case object PicoGauss extends IntrinsicFluxDensityUnit("PicoGauss", Seq("pG"), r"1e-12" * r"1e-4")
  case object NanoGauss extends IntrinsicFluxDensityUnit("NanoGauss", Seq("nG"), r"1e-9" * r"1e-4")
  case object MicroGauss extends IntrinsicFluxDensityUnit("MicroGauss", Seq("microGauss", "microG", "μG"), r"1e-6" * r"1e-4")
  case object MilliGauss extends IntrinsicFluxDensityUnit("MilliGauss", Seq("mG"), r"1e-3" * r"1e-4")
  case object CentiGauss extends IntrinsicFluxDensityUnit("CentiGauss", Seq("cG"), r"1e-2" * r"1e-4")
  case object DeciGauss extends IntrinsicFluxDensityUnit("DeciGauss", Seq("dG"), r"1e-1" * r"1e-4")
  case object Gauss extends IntrinsicFluxDensityUnit("Gauss", Seq("G"), r"1" * r"1e-4")
  case object DecaGauss extends IntrinsicFluxDensityUnit("DecaGauss", Seq("daG"), r"1e1" * r"1e-4")
  case object HectoGauss extends IntrinsicFluxDensityUnit("HectoGauss", Seq("hG"), r"1e2" * r"1e-4")
  case object KiloGauss extends IntrinsicFluxDensityUnit("KiloGauss", Seq("kG"), r"1e3" * r"1e-4")
  case object MegaGauss extends IntrinsicFluxDensityUnit("MegaGauss", Seq("MG"), r"1e6" * r"1e-4")
  case object GigaGauss extends IntrinsicFluxDensityUnit("GigaGauss", Seq("GG"), r"1e9" * r"1e-4")
  case object TeraGauss extends IntrinsicFluxDensityUnit("TeraGauss", Seq("TG"), r"1e12" * r"1e-4")
  case object PetaGauss extends IntrinsicFluxDensityUnit("PetaGauss", Seq("PG"), r"1e15" * r"1e-4")
  case object ExaGauss extends IntrinsicFluxDensityUnit("ExaGauss", Seq("EG"), r"1e18" * r"1e-4")
  case object ZettaGauss extends IntrinsicFluxDensityUnit("ZettaGauss", Seq("ZG"), r"1e21" * r"1e-4")
  case object YottaGauss extends IntrinsicFluxDensityUnit("YottaGauss", Seq("YG"), r"1e24" * r"1e-4")

  override lazy val values = Seq(YoctoTesla, ZeptoTesla, AttoTesla, FemtoTesla, PicoTesla, NanoTesla, MicroTesla, MilliTesla, CentiTesla, DeciTesla, Tesla, DecaTesla, HectoTesla, KiloTesla, MegaTesla, GigaTesla, TeraTesla, PetaTesla, ExaTesla, ZettaTesla, YottaTesla, YoctoGauss, ZeptoGauss, AttoGauss, FemtoGauss, PicoGauss, NanoGauss, MicroGauss, MilliGauss, CentiGauss, DeciGauss, Gauss, DecaGauss, HectoGauss, KiloGauss, MegaGauss, GigaGauss, TeraGauss, PetaGauss, ExaGauss, ZettaGauss, YottaGauss)

  // FluxUnit / AreaUnit -> FluxDensity
  private[FluxDensityUnit]
  class QuotientFluxPerAreaUnit(val numeratorUnit: FluxUnit, val denominatorUnit: AreaUnit)
      extends FluxDensityUnit with QuotientUnit[FluxDensityUnit, FluxUnit, AreaUnit]{

    override lazy val unitInTesla: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: FluxUnit, dUnit: AreaUnit): FluxDensityUnit =
    new QuotientFluxPerAreaUnit(nUnit, dUnit)
}

trait MultiplicativeByFluxDensityUnit[R]{
  def *(unit: FluxDensityUnit): R
}

trait DivisibleByFluxDensityUnit[R]{
  def /(unit: FluxDensityUnit): R
}

trait FluxDensityPostfixOps[A]{
  import FluxDensityUnit._

  protected def fluxDensityPostfixOps(unit: FluxDensityUnit): A


  def yT : A = fluxDensityPostfixOps(YoctoTesla)
  def zT : A = fluxDensityPostfixOps(ZeptoTesla)
  def aT : A = fluxDensityPostfixOps(AttoTesla)
  def fT : A = fluxDensityPostfixOps(FemtoTesla)
  def pT : A = fluxDensityPostfixOps(PicoTesla)
  def nT : A = fluxDensityPostfixOps(NanoTesla)
  def microTesla : A = fluxDensityPostfixOps(MicroTesla)
  def microT : A = fluxDensityPostfixOps(MicroTesla)
  def μT : A = fluxDensityPostfixOps(MicroTesla)
  def mT : A = fluxDensityPostfixOps(MilliTesla)
  def cT : A = fluxDensityPostfixOps(CentiTesla)
  def dT : A = fluxDensityPostfixOps(DeciTesla)
  def T : A = fluxDensityPostfixOps(Tesla)
  def daT : A = fluxDensityPostfixOps(DecaTesla)
  def hT : A = fluxDensityPostfixOps(HectoTesla)
  def kT : A = fluxDensityPostfixOps(KiloTesla)
  def MT : A = fluxDensityPostfixOps(MegaTesla)
  def GT : A = fluxDensityPostfixOps(GigaTesla)
  def TT : A = fluxDensityPostfixOps(TeraTesla)
  def PT : A = fluxDensityPostfixOps(PetaTesla)
  def ET : A = fluxDensityPostfixOps(ExaTesla)
  def ZT : A = fluxDensityPostfixOps(ZettaTesla)
  def YT : A = fluxDensityPostfixOps(YottaTesla)
  def yG : A = fluxDensityPostfixOps(YoctoGauss)
  def zG : A = fluxDensityPostfixOps(ZeptoGauss)
  def aG : A = fluxDensityPostfixOps(AttoGauss)
  def fG : A = fluxDensityPostfixOps(FemtoGauss)
  def pG : A = fluxDensityPostfixOps(PicoGauss)
  def nG : A = fluxDensityPostfixOps(NanoGauss)
  def microGauss : A = fluxDensityPostfixOps(MicroGauss)
  def microG : A = fluxDensityPostfixOps(MicroGauss)
  def μG : A = fluxDensityPostfixOps(MicroGauss)
  def mG : A = fluxDensityPostfixOps(MilliGauss)
  def cG : A = fluxDensityPostfixOps(CentiGauss)
  def dG : A = fluxDensityPostfixOps(DeciGauss)
  def G : A = fluxDensityPostfixOps(Gauss)
  def daG : A = fluxDensityPostfixOps(DecaGauss)
  def hG : A = fluxDensityPostfixOps(HectoGauss)
  def kG : A = fluxDensityPostfixOps(KiloGauss)
  def MG : A = fluxDensityPostfixOps(MegaGauss)
  def GG : A = fluxDensityPostfixOps(GigaGauss)
  def TG : A = fluxDensityPostfixOps(TeraGauss)
  def PG : A = fluxDensityPostfixOps(PetaGauss)
  def EG : A = fluxDensityPostfixOps(ExaGauss)
  def ZG : A = fluxDensityPostfixOps(ZettaGauss)
  def YG : A = fluxDensityPostfixOps(YottaGauss)
}

trait FluxDensityDot[A]{
  import FluxDensityUnit._

  protected def fluxDensityDot(unit: FluxDensityUnit): A

  def yT(dot: Dot): A = fluxDensityDot(YoctoTesla)
  def zT(dot: Dot): A = fluxDensityDot(ZeptoTesla)
  def aT(dot: Dot): A = fluxDensityDot(AttoTesla)
  def fT(dot: Dot): A = fluxDensityDot(FemtoTesla)
  def pT(dot: Dot): A = fluxDensityDot(PicoTesla)
  def nT(dot: Dot): A = fluxDensityDot(NanoTesla)
  def microTesla(dot: Dot): A = fluxDensityDot(MicroTesla)
  def microT(dot: Dot): A = fluxDensityDot(MicroTesla)
  def μT(dot: Dot): A = fluxDensityDot(MicroTesla)
  def mT(dot: Dot): A = fluxDensityDot(MilliTesla)
  def cT(dot: Dot): A = fluxDensityDot(CentiTesla)
  def dT(dot: Dot): A = fluxDensityDot(DeciTesla)
  def T(dot: Dot): A = fluxDensityDot(Tesla)
  def daT(dot: Dot): A = fluxDensityDot(DecaTesla)
  def hT(dot: Dot): A = fluxDensityDot(HectoTesla)
  def kT(dot: Dot): A = fluxDensityDot(KiloTesla)
  def MT(dot: Dot): A = fluxDensityDot(MegaTesla)
  def GT(dot: Dot): A = fluxDensityDot(GigaTesla)
  def TT(dot: Dot): A = fluxDensityDot(TeraTesla)
  def PT(dot: Dot): A = fluxDensityDot(PetaTesla)
  def ET(dot: Dot): A = fluxDensityDot(ExaTesla)
  def ZT(dot: Dot): A = fluxDensityDot(ZettaTesla)
  def YT(dot: Dot): A = fluxDensityDot(YottaTesla)
  def yG(dot: Dot): A = fluxDensityDot(YoctoGauss)
  def zG(dot: Dot): A = fluxDensityDot(ZeptoGauss)
  def aG(dot: Dot): A = fluxDensityDot(AttoGauss)
  def fG(dot: Dot): A = fluxDensityDot(FemtoGauss)
  def pG(dot: Dot): A = fluxDensityDot(PicoGauss)
  def nG(dot: Dot): A = fluxDensityDot(NanoGauss)
  def microGauss(dot: Dot): A = fluxDensityDot(MicroGauss)
  def microG(dot: Dot): A = fluxDensityDot(MicroGauss)
  def μG(dot: Dot): A = fluxDensityDot(MicroGauss)
  def mG(dot: Dot): A = fluxDensityDot(MilliGauss)
  def cG(dot: Dot): A = fluxDensityDot(CentiGauss)
  def dG(dot: Dot): A = fluxDensityDot(DeciGauss)
  def G(dot: Dot): A = fluxDensityDot(Gauss)
  def daG(dot: Dot): A = fluxDensityDot(DecaGauss)
  def hG(dot: Dot): A = fluxDensityDot(HectoGauss)
  def kG(dot: Dot): A = fluxDensityDot(KiloGauss)
  def MG(dot: Dot): A = fluxDensityDot(MegaGauss)
  def GG(dot: Dot): A = fluxDensityDot(GigaGauss)
  def TG(dot: Dot): A = fluxDensityDot(TeraGauss)
  def PG(dot: Dot): A = fluxDensityDot(PetaGauss)
  def EG(dot: Dot): A = fluxDensityDot(ExaGauss)
  def ZG(dot: Dot): A = fluxDensityDot(ZettaGauss)
  def YG(dot: Dot): A = fluxDensityDot(YottaGauss)
}

trait FluxDensityPer[A]{
  import FluxDensityUnit._

  protected def fluxDensityPer(unit: FluxDensityUnit): A

  def yT(per: Per): A = fluxDensityPer(YoctoTesla)
  def zT(per: Per): A = fluxDensityPer(ZeptoTesla)
  def aT(per: Per): A = fluxDensityPer(AttoTesla)
  def fT(per: Per): A = fluxDensityPer(FemtoTesla)
  def pT(per: Per): A = fluxDensityPer(PicoTesla)
  def nT(per: Per): A = fluxDensityPer(NanoTesla)
  def microTesla(per: Per): A = fluxDensityPer(MicroTesla)
  def microT(per: Per): A = fluxDensityPer(MicroTesla)
  def μT(per: Per): A = fluxDensityPer(MicroTesla)
  def mT(per: Per): A = fluxDensityPer(MilliTesla)
  def cT(per: Per): A = fluxDensityPer(CentiTesla)
  def dT(per: Per): A = fluxDensityPer(DeciTesla)
  def T(per: Per): A = fluxDensityPer(Tesla)
  def daT(per: Per): A = fluxDensityPer(DecaTesla)
  def hT(per: Per): A = fluxDensityPer(HectoTesla)
  def kT(per: Per): A = fluxDensityPer(KiloTesla)
  def MT(per: Per): A = fluxDensityPer(MegaTesla)
  def GT(per: Per): A = fluxDensityPer(GigaTesla)
  def TT(per: Per): A = fluxDensityPer(TeraTesla)
  def PT(per: Per): A = fluxDensityPer(PetaTesla)
  def ET(per: Per): A = fluxDensityPer(ExaTesla)
  def ZT(per: Per): A = fluxDensityPer(ZettaTesla)
  def YT(per: Per): A = fluxDensityPer(YottaTesla)
  def yG(per: Per): A = fluxDensityPer(YoctoGauss)
  def zG(per: Per): A = fluxDensityPer(ZeptoGauss)
  def aG(per: Per): A = fluxDensityPer(AttoGauss)
  def fG(per: Per): A = fluxDensityPer(FemtoGauss)
  def pG(per: Per): A = fluxDensityPer(PicoGauss)
  def nG(per: Per): A = fluxDensityPer(NanoGauss)
  def microGauss(per: Per): A = fluxDensityPer(MicroGauss)
  def microG(per: Per): A = fluxDensityPer(MicroGauss)
  def μG(per: Per): A = fluxDensityPer(MicroGauss)
  def mG(per: Per): A = fluxDensityPer(MilliGauss)
  def cG(per: Per): A = fluxDensityPer(CentiGauss)
  def dG(per: Per): A = fluxDensityPer(DeciGauss)
  def G(per: Per): A = fluxDensityPer(Gauss)
  def daG(per: Per): A = fluxDensityPer(DecaGauss)
  def hG(per: Per): A = fluxDensityPer(HectoGauss)
  def kG(per: Per): A = fluxDensityPer(KiloGauss)
  def MG(per: Per): A = fluxDensityPer(MegaGauss)
  def GG(per: Per): A = fluxDensityPer(GigaGauss)
  def TG(per: Per): A = fluxDensityPer(TeraGauss)
  def PG(per: Per): A = fluxDensityPer(PetaGauss)
  def EG(per: Per): A = fluxDensityPer(ExaGauss)
  def ZG(per: Per): A = fluxDensityPer(ZettaGauss)
  def YG(per: Per): A = fluxDensityPer(YottaGauss)
}

trait PredefinedFluxDensityUnit extends FluxDensityPostfixOps[FluxDensityUnit]{
  override protected def fluxDensityPostfixOps(unit: FluxDensityUnit) = unit
  
}

object PredefinedFluxDensityUnit extends PredefinedFluxDensityUnit
