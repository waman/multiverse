package org.waman.multiverse.radiation

import org.waman.multiverse._
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait EquivalentDosePostfixOps[A]{

  import EquivalentDoseUnit._

  protected def equivalentDosePostfixOps(equivalentDoseUnit: EquivalentDoseUnit): A

  def ySv: A = equivalentDosePostfixOps(YoctoSievert)
  def zSv: A = equivalentDosePostfixOps(ZeptoSievert)
  def aSv: A = equivalentDosePostfixOps(AttoSievert)
  def fSv: A = equivalentDosePostfixOps(FemtoSievert)
  def pSv: A = equivalentDosePostfixOps(PicoSievert)
  def nSv: A = equivalentDosePostfixOps(NanoSievert)
  def μSv: A = equivalentDosePostfixOps(MicroSievert)
  def mSv: A = equivalentDosePostfixOps(MilliSievert)
  def cSv: A = equivalentDosePostfixOps(CentiSievert)
  def dSv: A = equivalentDosePostfixOps(DeciSievert)
  def Sv : A = equivalentDosePostfixOps(Sievert)
  def daSv: A = equivalentDosePostfixOps(DecaSievert)
  def hSv: A = equivalentDosePostfixOps(HectoSievert)
  def kSv: A = equivalentDosePostfixOps(KiloSievert)
  def MSv: A = equivalentDosePostfixOps(MegaSievert)
  def GSv: A = equivalentDosePostfixOps(GigaSievert)
  def TSv: A = equivalentDosePostfixOps(TeraSievert)
  def PSv: A = equivalentDosePostfixOps(PetaSievert)
  def ESv: A = equivalentDosePostfixOps(ExaSievert)
  def ZSv: A = equivalentDosePostfixOps(ZettaSievert)
  def YSv: A = equivalentDosePostfixOps(YottaSievert)

  def yrem: A = equivalentDosePostfixOps(YoctoREM)
  def zrem: A = equivalentDosePostfixOps(ZeptoREM)
  def arem: A = equivalentDosePostfixOps(AttoREM)
  def frem: A = equivalentDosePostfixOps(FemtoREM)
  def prem: A = equivalentDosePostfixOps(PicoREM)
  def nrem: A = equivalentDosePostfixOps(NanoREM)
  def μrem: A = equivalentDosePostfixOps(MicroREM)
  def mrem: A = equivalentDosePostfixOps(MilliREM)
  def crem: A = equivalentDosePostfixOps(CentiREM)
  def drem: A = equivalentDosePostfixOps(DeciREM)
  def rem : A = equivalentDosePostfixOps(REM)
  def darem: A = equivalentDosePostfixOps(DecaREM)
  def hrem: A = equivalentDosePostfixOps(HectoREM)
  def krem: A = equivalentDosePostfixOps(KiloREM)
  def Mrem: A = equivalentDosePostfixOps(MegaREM)
  def Grem: A = equivalentDosePostfixOps(GigaREM)
  def Trem: A = equivalentDosePostfixOps(TeraREM)
  def Prem: A = equivalentDosePostfixOps(PetaREM)
  def Erem: A = equivalentDosePostfixOps(ExaREM)
  def Zrem: A = equivalentDosePostfixOps(ZettaREM)
  def Yrem: A = equivalentDosePostfixOps(YottaREM)
}

trait EquivalentDosePer[A]{

  import EquivalentDoseUnit._

  protected def equivalentDosePer(equivalentDoseUnit: EquivalentDoseUnit): A

  def ySv(per: Per): A = equivalentDosePer(YoctoSievert)
  def zSv(per: Per): A = equivalentDosePer(ZeptoSievert)
  def aSv(per: Per): A = equivalentDosePer(AttoSievert)
  def fSv(per: Per): A = equivalentDosePer(FemtoSievert)
  def pSv(per: Per): A = equivalentDosePer(PicoSievert)
  def nSv(per: Per): A = equivalentDosePer(NanoSievert)
  def μSv(per: Per): A = equivalentDosePer(MicroSievert)
  def mSv(per: Per): A = equivalentDosePer(MilliSievert)
  def cSv(per: Per): A = equivalentDosePer(CentiSievert)
  def dSv(per: Per): A = equivalentDosePer(DeciSievert)
  def Sv (per: Per): A = equivalentDosePer(Sievert)
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
  def mrem(per: Per): A = equivalentDosePer(MilliREM)
  def crem(per: Per): A = equivalentDosePer(CentiREM)
  def drem(per: Per): A = equivalentDosePer(DeciREM)
  def rem (per: Per): A = equivalentDosePer(REM)
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

class EquivalentDose[A: Fractional](val value: A, val unit: EquivalentDoseUnit)
  extends Quantity[A, EquivalentDoseUnit]
    with EquivalentDosePostfixOps[A]
    with DivisibleByTimeUnit[EquivalentDoseRate[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: EquivalentDoseUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSievert) / real(evalUnit.unitInSievert)

  override protected def equivalentDosePostfixOps(equivalentDoseUnit: EquivalentDoseUnit) =
    apply(equivalentDoseUnit)

  override def /(timeUnit: TimeUnit) = new EquivalentDoseRate(value, unit / timeUnit)
}

sealed abstract class EquivalentDoseUnit(val symbol: String, val unitInSievert: Real)
    extends PhysicalUnit[EquivalentDoseUnit]
    with DivisibleByTimeUnit[EquivalentDoseRateUnit]{

  def this(symbol: String, factor: Real, unit: EquivalentDoseUnit) =
    this(symbol, factor * unit.unitInSievert)

  override def baseUnit = EquivalentDoseUnit.Sievert
  override def valueInBaseUnit = unitInSievert

  override def /(timeUnit: TimeUnit) = EquivalentDoseRateUnit(this, timeUnit)
}

object EquivalentDoseUnit extends ConstantsDefined[EquivalentDoseUnit]{

  // intrinsic
  case object YoctoSievert extends EquivalentDoseUnit("ySv", r"1e-24")
  case object ZeptoSievert extends EquivalentDoseUnit("zSv", r"1e-21")
  case object AttoSievert  extends EquivalentDoseUnit("aSv", r"1e-18")
  case object FemtoSievert extends EquivalentDoseUnit("fSv", r"1e-15")
  case object PicoSievert  extends EquivalentDoseUnit("pSv", r"1e-12")
  case object NanoSievert  extends EquivalentDoseUnit("nSv", r"1e-9")
  case object MicroSievert extends EquivalentDoseUnit("μSv", r"1e-6")
  case object MilliSievert extends EquivalentDoseUnit("mSv", r"1e-3")
  case object CentiSievert extends EquivalentDoseUnit("cSv", r"1e-2")
  case object DeciSievert  extends EquivalentDoseUnit("dSv", r"1e-1")
  case object Sievert      extends EquivalentDoseUnit("Sv" , 1)
  case object DecaSievert  extends EquivalentDoseUnit("daSv", r"1e1")
  case object HectoSievert extends EquivalentDoseUnit("hSv", r"1e2")
  case object KiloSievert  extends EquivalentDoseUnit("kSv", r"1e3")
  case object MegaSievert  extends EquivalentDoseUnit("MSv", r"1e6")
  case object GigaSievert  extends EquivalentDoseUnit("GSv", r"1e9")
  case object TeraSievert  extends EquivalentDoseUnit("TSv", r"1e12")
  case object PetaSievert  extends EquivalentDoseUnit("PSv", r"1e15")
  case object ExaSievert   extends EquivalentDoseUnit("ESv", r"1e18")
  case object ZettaSievert extends EquivalentDoseUnit("ZSv", r"1e21")
  case object YottaSievert extends EquivalentDoseUnit("YSv", r"1e24")

  case object YoctoREM extends EquivalentDoseUnit("yrem", r"1e-24", REM)
  case object ZeptoREM extends EquivalentDoseUnit("zrem", r"1e-21", REM)
  case object AttoREM  extends EquivalentDoseUnit("arem", r"1e-18", REM)
  case object FemtoREM extends EquivalentDoseUnit("frem", r"1e-15", REM)
  case object PicoREM  extends EquivalentDoseUnit("prem", r"1e-12", REM)
  case object NanoREM  extends EquivalentDoseUnit("nrem", r"1e-9", REM)
  case object MicroREM extends EquivalentDoseUnit("μrem", r"1e-6", REM)
  case object MilliREM extends EquivalentDoseUnit("mrem", r"1e-3", REM)
  case object CentiREM extends EquivalentDoseUnit("crem", r"1e-2", REM)
  case object DeciREM  extends EquivalentDoseUnit("drem", r"1e-1", REM)
  case object REM extends EquivalentDoseUnit("rem", r"0.01")
  case object DecaREM  extends EquivalentDoseUnit("darem", r"1e1", REM)
  case object HectoREM extends EquivalentDoseUnit("hrem", r"1e2", REM)
  case object KiloREM  extends EquivalentDoseUnit("krem", r"1e3", REM)
  case object MegaREM  extends EquivalentDoseUnit("Mrem", r"1e6", REM)
  case object GigaREM  extends EquivalentDoseUnit("Grem", r"1e9", REM)
  case object TeraREM  extends EquivalentDoseUnit("Trem", r"1e12", REM)
  case object PetaREM  extends EquivalentDoseUnit("Prem", r"1e15", REM)
  case object ExaREM   extends EquivalentDoseUnit("Erem", r"1e18", REM)
  case object ZettaREM extends EquivalentDoseUnit("Zrem", r"1e21", REM)
  case object YottaREM extends EquivalentDoseUnit("Yrem", r"1e24", REM)

  override lazy val values = Seq(
    YoctoSievert,
    ZeptoSievert,
    AttoSievert,
    FemtoSievert,
    PicoSievert,
    NanoSievert,
    MicroSievert,
    MilliSievert,
    CentiSievert,
    DeciSievert,
    Sievert,
    DecaSievert,
    HectoSievert,
    KiloSievert,
    MegaSievert,
    GigaSievert,
    TeraSievert,
    PetaSievert,
    ExaSievert,
    ZettaSievert,
    YottaSievert,

    YoctoREM,
    ZeptoREM,
    AttoREM,
    FemtoREM,
    PicoREM,
    NanoREM,
    MicroREM,
    MilliREM,
    CentiREM,
    DeciREM,
    REM,
    DecaREM,
    HectoREM,
    KiloREM,
    MegaREM,
    GigaREM,
    TeraREM,
    PetaREM,
    ExaREM,
    ZettaREM,
    YottaREM
  )
}

trait PredefinedEquivalentDoseUnit extends EquivalentDosePostfixOps[EquivalentDoseUnit]{

  override protected def equivalentDosePostfixOps(equivalentDoseUnit: EquivalentDoseUnit) = equivalentDoseUnit
}

object PredefinedEquivalentDoseUnit extends PredefinedEquivalentDoseUnit

trait EquivalentDoseFactory[A]
    extends EquivalentDosePostfixOps[EquivalentDose[A]]
    with EquivalentDosePer[TimePostfixOps[EquivalentDoseRate[A]]]{

  def apply(unit: EquivalentDoseUnit): EquivalentDose[A]

  override protected def equivalentDosePostfixOps(equivalentDoseUnit: EquivalentDoseUnit) =
    apply(equivalentDoseUnit)

  // EquivalentDose / Time -> EquivalentDoseRate
  def apply(unit: EquivalentDoseRateUnit) : EquivalentDoseRate[A]

  override protected def equivalentDosePer(equivalentDoseUnit: EquivalentDoseUnit) =
    new TimePostfixOps[EquivalentDoseRate[A]]{
      override protected def timePostfixOps(timeUnit: TimeUnit) = apply(equivalentDoseUnit / timeUnit)
    }
}