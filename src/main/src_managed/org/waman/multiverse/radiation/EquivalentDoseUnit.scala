package org.waman.multiverse.radiation

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.time._

sealed trait EquivalentDoseUnit extends PhysicalUnit[EquivalentDoseUnit]
  with DivisibleByTimeUnit[EquivalentDoseRateUnit]{

  override def getSIUnit = org.waman.multiverse.radiation.EquivalentDoseUnit.Sievert

  override def /(unit: TimeUnit) = EquivalentDoseRateUnit(this, unit)
}

object EquivalentDoseUnit extends ConstantsDefined[EquivalentDoseUnit]{

  // intrinsic
  private[EquivalentDoseUnit]
  class IntrinsicEquivalentDoseUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends EquivalentDoseUnit{

    def this(name: String, symbols: Seq[String], unit: EquivalentDoseUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: EquivalentDoseUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoSievert extends IntrinsicEquivalentDoseUnit("YoctoSievert", Seq("ySv"), r"1e-24")
  case object ZeptoSievert extends IntrinsicEquivalentDoseUnit("ZeptoSievert", Seq("zSv"), r"1e-21")
  case object AttoSievert extends IntrinsicEquivalentDoseUnit("AttoSievert", Seq("aSv"), r"1e-18")
  case object FemtoSievert extends IntrinsicEquivalentDoseUnit("FemtoSievert", Seq("fSv"), r"1e-15")
  case object PicoSievert extends IntrinsicEquivalentDoseUnit("PicoSievert", Seq("pSv"), r"1e-12")
  case object NanoSievert extends IntrinsicEquivalentDoseUnit("NanoSievert", Seq("nSv"), r"1e-9")
  case object MicroSievert extends IntrinsicEquivalentDoseUnit("MicroSievert", Seq("μSv", "mcSv"), r"1e-6")
  case object MilliSievert extends IntrinsicEquivalentDoseUnit("MilliSievert", Seq("mSv"), r"1e-3")
  case object CentiSievert extends IntrinsicEquivalentDoseUnit("CentiSievert", Seq("cSv"), r"1e-2")
  case object DeciSievert extends IntrinsicEquivalentDoseUnit("DeciSievert", Seq("dSv"), r"1e-1")
  case object Sievert extends IntrinsicEquivalentDoseUnit("Sievert", Seq("Sv"), r"1")
  case object DecaSievert extends IntrinsicEquivalentDoseUnit("DecaSievert", Seq("daSv"), r"1e1")
  case object HectoSievert extends IntrinsicEquivalentDoseUnit("HectoSievert", Seq("hSv"), r"1e2")
  case object KiloSievert extends IntrinsicEquivalentDoseUnit("KiloSievert", Seq("kSv"), r"1e3")
  case object MegaSievert extends IntrinsicEquivalentDoseUnit("MegaSievert", Seq("MSv"), r"1e6")
  case object GigaSievert extends IntrinsicEquivalentDoseUnit("GigaSievert", Seq("GSv"), r"1e9")
  case object TeraSievert extends IntrinsicEquivalentDoseUnit("TeraSievert", Seq("TSv"), r"1e12")
  case object PetaSievert extends IntrinsicEquivalentDoseUnit("PetaSievert", Seq("PSv"), r"1e15")
  case object ExaSievert extends IntrinsicEquivalentDoseUnit("ExaSievert", Seq("ESv"), r"1e18")
  case object ZettaSievert extends IntrinsicEquivalentDoseUnit("ZettaSievert", Seq("ZSv"), r"1e21")
  case object YottaSievert extends IntrinsicEquivalentDoseUnit("YottaSievert", Seq("YSv"), r"1e24")
  case object YoctoREM extends IntrinsicEquivalentDoseUnit("YoctoREM", Seq("yrem"), r"1e-24" * r"1e-2")
  case object ZeptoREM extends IntrinsicEquivalentDoseUnit("ZeptoREM", Seq("zrem"), r"1e-21" * r"1e-2")
  case object AttoREM extends IntrinsicEquivalentDoseUnit("AttoREM", Seq("arem"), r"1e-18" * r"1e-2")
  case object FemtoREM extends IntrinsicEquivalentDoseUnit("FemtoREM", Seq("frem"), r"1e-15" * r"1e-2")
  case object PicoREM extends IntrinsicEquivalentDoseUnit("PicoREM", Seq("prem"), r"1e-12" * r"1e-2")
  case object NanoREM extends IntrinsicEquivalentDoseUnit("NanoREM", Seq("nrem"), r"1e-9" * r"1e-2")
  case object MicroREM extends IntrinsicEquivalentDoseUnit("MicroREM", Seq("μrem", "mcrem"), r"1e-6" * r"1e-2")
  case object MilliREM extends IntrinsicEquivalentDoseUnit("MilliREM", Seq("mrem"), r"1e-3" * r"1e-2")
  case object CentiREM extends IntrinsicEquivalentDoseUnit("CentiREM", Seq("crem"), r"1e-2" * r"1e-2")
  case object DeciREM extends IntrinsicEquivalentDoseUnit("DeciREM", Seq("drem"), r"1e-1" * r"1e-2")
  case object REM extends IntrinsicEquivalentDoseUnit("REM", Seq("rem"), r"1" * r"1e-2")
  case object DecaREM extends IntrinsicEquivalentDoseUnit("DecaREM", Seq("darem"), r"1e1" * r"1e-2")
  case object HectoREM extends IntrinsicEquivalentDoseUnit("HectoREM", Seq("hrem"), r"1e2" * r"1e-2")
  case object KiloREM extends IntrinsicEquivalentDoseUnit("KiloREM", Seq("krem"), r"1e3" * r"1e-2")
  case object MegaREM extends IntrinsicEquivalentDoseUnit("MegaREM", Seq("Mrem"), r"1e6" * r"1e-2")
  case object GigaREM extends IntrinsicEquivalentDoseUnit("GigaREM", Seq("Grem"), r"1e9" * r"1e-2")
  case object TeraREM extends IntrinsicEquivalentDoseUnit("TeraREM", Seq("Trem"), r"1e12" * r"1e-2")
  case object PetaREM extends IntrinsicEquivalentDoseUnit("PetaREM", Seq("Prem"), r"1e15" * r"1e-2")
  case object ExaREM extends IntrinsicEquivalentDoseUnit("ExaREM", Seq("Erem"), r"1e18" * r"1e-2")
  case object ZettaREM extends IntrinsicEquivalentDoseUnit("ZettaREM", Seq("Zrem"), r"1e21" * r"1e-2")
  case object YottaREM extends IntrinsicEquivalentDoseUnit("YottaREM", Seq("Yrem"), r"1e24" * r"1e-2")

  override lazy val values = Seq(YoctoSievert, ZeptoSievert, AttoSievert, FemtoSievert, PicoSievert, NanoSievert, MicroSievert, MilliSievert, CentiSievert, DeciSievert, Sievert, DecaSievert, HectoSievert, KiloSievert, MegaSievert, GigaSievert, TeraSievert, PetaSievert, ExaSievert, ZettaSievert, YottaSievert, YoctoREM, ZeptoREM, AttoREM, FemtoREM, PicoREM, NanoREM, MicroREM, MilliREM, CentiREM, DeciREM, REM, DecaREM, HectoREM, KiloREM, MegaREM, GigaREM, TeraREM, PetaREM, ExaREM, ZettaREM, YottaREM)
}

trait MultiplicativeByEquivalentDoseUnit[R]{
  def *(unit: EquivalentDoseUnit): R
}

trait DivisibleByEquivalentDoseUnit[R]{
  def /(unit: EquivalentDoseUnit): R
}

trait EquivalentDosePostfixOps[A]{
  import EquivalentDoseUnit._

  protected def equivalentDosePostfixOps(unit: EquivalentDoseUnit): A


  def ySv : A = equivalentDosePostfixOps(YoctoSievert)
  def zSv : A = equivalentDosePostfixOps(ZeptoSievert)
  def aSv : A = equivalentDosePostfixOps(AttoSievert)
  def fSv : A = equivalentDosePostfixOps(FemtoSievert)
  def pSv : A = equivalentDosePostfixOps(PicoSievert)
  def nSv : A = equivalentDosePostfixOps(NanoSievert)
  def μSv : A = equivalentDosePostfixOps(MicroSievert)
  def mcSv : A = equivalentDosePostfixOps(MicroSievert)
  def mSv : A = equivalentDosePostfixOps(MilliSievert)
  def cSv : A = equivalentDosePostfixOps(CentiSievert)
  def dSv : A = equivalentDosePostfixOps(DeciSievert)
  def Sv : A = equivalentDosePostfixOps(Sievert)
  def daSv : A = equivalentDosePostfixOps(DecaSievert)
  def hSv : A = equivalentDosePostfixOps(HectoSievert)
  def kSv : A = equivalentDosePostfixOps(KiloSievert)
  def MSv : A = equivalentDosePostfixOps(MegaSievert)
  def GSv : A = equivalentDosePostfixOps(GigaSievert)
  def TSv : A = equivalentDosePostfixOps(TeraSievert)
  def PSv : A = equivalentDosePostfixOps(PetaSievert)
  def ESv : A = equivalentDosePostfixOps(ExaSievert)
  def ZSv : A = equivalentDosePostfixOps(ZettaSievert)
  def YSv : A = equivalentDosePostfixOps(YottaSievert)
  def yrem : A = equivalentDosePostfixOps(YoctoREM)
  def zrem : A = equivalentDosePostfixOps(ZeptoREM)
  def arem : A = equivalentDosePostfixOps(AttoREM)
  def frem : A = equivalentDosePostfixOps(FemtoREM)
  def prem : A = equivalentDosePostfixOps(PicoREM)
  def nrem : A = equivalentDosePostfixOps(NanoREM)
  def μrem : A = equivalentDosePostfixOps(MicroREM)
  def mcrem : A = equivalentDosePostfixOps(MicroREM)
  def mrem : A = equivalentDosePostfixOps(MilliREM)
  def crem : A = equivalentDosePostfixOps(CentiREM)
  def drem : A = equivalentDosePostfixOps(DeciREM)
  def rem : A = equivalentDosePostfixOps(REM)
  def darem : A = equivalentDosePostfixOps(DecaREM)
  def hrem : A = equivalentDosePostfixOps(HectoREM)
  def krem : A = equivalentDosePostfixOps(KiloREM)
  def Mrem : A = equivalentDosePostfixOps(MegaREM)
  def Grem : A = equivalentDosePostfixOps(GigaREM)
  def Trem : A = equivalentDosePostfixOps(TeraREM)
  def Prem : A = equivalentDosePostfixOps(PetaREM)
  def Erem : A = equivalentDosePostfixOps(ExaREM)
  def Zrem : A = equivalentDosePostfixOps(ZettaREM)
  def Yrem : A = equivalentDosePostfixOps(YottaREM)
}

trait EquivalentDoseDot[A]{
  import EquivalentDoseUnit._

  protected def equivalentDoseDot(unit: EquivalentDoseUnit): A

  def ySv(dot: Dot): A = equivalentDoseDot(YoctoSievert)
  def zSv(dot: Dot): A = equivalentDoseDot(ZeptoSievert)
  def aSv(dot: Dot): A = equivalentDoseDot(AttoSievert)
  def fSv(dot: Dot): A = equivalentDoseDot(FemtoSievert)
  def pSv(dot: Dot): A = equivalentDoseDot(PicoSievert)
  def nSv(dot: Dot): A = equivalentDoseDot(NanoSievert)
  def μSv(dot: Dot): A = equivalentDoseDot(MicroSievert)
  def mcSv(dot: Dot): A = equivalentDoseDot(MicroSievert)
  def mSv(dot: Dot): A = equivalentDoseDot(MilliSievert)
  def cSv(dot: Dot): A = equivalentDoseDot(CentiSievert)
  def dSv(dot: Dot): A = equivalentDoseDot(DeciSievert)
  def Sv(dot: Dot): A = equivalentDoseDot(Sievert)
  def daSv(dot: Dot): A = equivalentDoseDot(DecaSievert)
  def hSv(dot: Dot): A = equivalentDoseDot(HectoSievert)
  def kSv(dot: Dot): A = equivalentDoseDot(KiloSievert)
  def MSv(dot: Dot): A = equivalentDoseDot(MegaSievert)
  def GSv(dot: Dot): A = equivalentDoseDot(GigaSievert)
  def TSv(dot: Dot): A = equivalentDoseDot(TeraSievert)
  def PSv(dot: Dot): A = equivalentDoseDot(PetaSievert)
  def ESv(dot: Dot): A = equivalentDoseDot(ExaSievert)
  def ZSv(dot: Dot): A = equivalentDoseDot(ZettaSievert)
  def YSv(dot: Dot): A = equivalentDoseDot(YottaSievert)
  def yrem(dot: Dot): A = equivalentDoseDot(YoctoREM)
  def zrem(dot: Dot): A = equivalentDoseDot(ZeptoREM)
  def arem(dot: Dot): A = equivalentDoseDot(AttoREM)
  def frem(dot: Dot): A = equivalentDoseDot(FemtoREM)
  def prem(dot: Dot): A = equivalentDoseDot(PicoREM)
  def nrem(dot: Dot): A = equivalentDoseDot(NanoREM)
  def μrem(dot: Dot): A = equivalentDoseDot(MicroREM)
  def mcrem(dot: Dot): A = equivalentDoseDot(MicroREM)
  def mrem(dot: Dot): A = equivalentDoseDot(MilliREM)
  def crem(dot: Dot): A = equivalentDoseDot(CentiREM)
  def drem(dot: Dot): A = equivalentDoseDot(DeciREM)
  def rem(dot: Dot): A = equivalentDoseDot(REM)
  def darem(dot: Dot): A = equivalentDoseDot(DecaREM)
  def hrem(dot: Dot): A = equivalentDoseDot(HectoREM)
  def krem(dot: Dot): A = equivalentDoseDot(KiloREM)
  def Mrem(dot: Dot): A = equivalentDoseDot(MegaREM)
  def Grem(dot: Dot): A = equivalentDoseDot(GigaREM)
  def Trem(dot: Dot): A = equivalentDoseDot(TeraREM)
  def Prem(dot: Dot): A = equivalentDoseDot(PetaREM)
  def Erem(dot: Dot): A = equivalentDoseDot(ExaREM)
  def Zrem(dot: Dot): A = equivalentDoseDot(ZettaREM)
  def Yrem(dot: Dot): A = equivalentDoseDot(YottaREM)
}

trait EquivalentDosePer[A]{
  import EquivalentDoseUnit._

  protected def equivalentDosePer(unit: EquivalentDoseUnit): A

  def ySv(per: Per): A = equivalentDosePer(YoctoSievert)
  def zSv(per: Per): A = equivalentDosePer(ZeptoSievert)
  def aSv(per: Per): A = equivalentDosePer(AttoSievert)
  def fSv(per: Per): A = equivalentDosePer(FemtoSievert)
  def pSv(per: Per): A = equivalentDosePer(PicoSievert)
  def nSv(per: Per): A = equivalentDosePer(NanoSievert)
  def μSv(per: Per): A = equivalentDosePer(MicroSievert)
  def mcSv(per: Per): A = equivalentDosePer(MicroSievert)
  def mSv(per: Per): A = equivalentDosePer(MilliSievert)
  def cSv(per: Per): A = equivalentDosePer(CentiSievert)
  def dSv(per: Per): A = equivalentDosePer(DeciSievert)
  def Sv(per: Per): A = equivalentDosePer(Sievert)
  def daSv(per: Per): A = equivalentDosePer(DecaSievert)
  def hSv(per: Per): A = equivalentDosePer(HectoSievert)
  def kSv(per: Per): A = equivalentDosePer(KiloSievert)
  def MSv(per: Per): A = equivalentDosePer(MegaSievert)
  def GSv(per: Per): A = equivalentDosePer(GigaSievert)
  def TSv(per: Per): A = equivalentDosePer(TeraSievert)
  def PSv(per: Per): A = equivalentDosePer(PetaSievert)
  def ESv(per: Per): A = equivalentDosePer(ExaSievert)
  def ZSv(per: Per): A = equivalentDosePer(ZettaSievert)
  def YSv(per: Per): A = equivalentDosePer(YottaSievert)
  def yrem(per: Per): A = equivalentDosePer(YoctoREM)
  def zrem(per: Per): A = equivalentDosePer(ZeptoREM)
  def arem(per: Per): A = equivalentDosePer(AttoREM)
  def frem(per: Per): A = equivalentDosePer(FemtoREM)
  def prem(per: Per): A = equivalentDosePer(PicoREM)
  def nrem(per: Per): A = equivalentDosePer(NanoREM)
  def μrem(per: Per): A = equivalentDosePer(MicroREM)
  def mcrem(per: Per): A = equivalentDosePer(MicroREM)
  def mrem(per: Per): A = equivalentDosePer(MilliREM)
  def crem(per: Per): A = equivalentDosePer(CentiREM)
  def drem(per: Per): A = equivalentDosePer(DeciREM)
  def rem(per: Per): A = equivalentDosePer(REM)
  def darem(per: Per): A = equivalentDosePer(DecaREM)
  def hrem(per: Per): A = equivalentDosePer(HectoREM)
  def krem(per: Per): A = equivalentDosePer(KiloREM)
  def Mrem(per: Per): A = equivalentDosePer(MegaREM)
  def Grem(per: Per): A = equivalentDosePer(GigaREM)
  def Trem(per: Per): A = equivalentDosePer(TeraREM)
  def Prem(per: Per): A = equivalentDosePer(PetaREM)
  def Erem(per: Per): A = equivalentDosePer(ExaREM)
  def Zrem(per: Per): A = equivalentDosePer(ZettaREM)
  def Yrem(per: Per): A = equivalentDosePer(YottaREM)
}

trait PredefinedEquivalentDoseUnit extends EquivalentDosePostfixOps[EquivalentDoseUnit]{
  override protected def equivalentDosePostfixOps(unit: EquivalentDoseUnit) = unit
  
}

object PredefinedEquivalentDoseUnit extends PredefinedEquivalentDoseUnit
