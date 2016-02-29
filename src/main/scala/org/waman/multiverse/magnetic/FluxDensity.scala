package org.waman.multiverse.magnetic

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPostfixOps, AreaUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait FluxDensityPostfixOps[A]{

  import FluxDensityUnit._

  protected def fluxDensityPostfixOps(fluxDensityUnit: FluxDensityUnit): A


  def yT: A = fluxDensityPostfixOps(YoctoTesla)
  def zT: A = fluxDensityPostfixOps(ZeptoTesla)
  def aT: A = fluxDensityPostfixOps(AttoTesla)
  def fT: A = fluxDensityPostfixOps(FemtoTesla)
  def pT: A = fluxDensityPostfixOps(PicoTesla)
  def nT: A = fluxDensityPostfixOps(NanoTesla)
  def μT: A = fluxDensityPostfixOps(MicroTesla)
  def mT: A = fluxDensityPostfixOps(MilliTesla)
  def cT: A = fluxDensityPostfixOps(CentiTesla)
  def dT: A = fluxDensityPostfixOps(DeciTesla)
  def T : A = fluxDensityPostfixOps(Tesla)
  def daT: A = fluxDensityPostfixOps(DecaTesla)
  def hT: A = fluxDensityPostfixOps(HectoTesla)
  def kT: A = fluxDensityPostfixOps(KiloTesla)
  def MT: A = fluxDensityPostfixOps(MegaTesla)
  def GT: A = fluxDensityPostfixOps(GigaTesla)
  def TT: A = fluxDensityPostfixOps(TeraTesla)
  def PT: A = fluxDensityPostfixOps(PetaTesla)
  def ET: A = fluxDensityPostfixOps(ExaTesla)
  def ZT: A = fluxDensityPostfixOps(ZettaTesla)
  def YT: A = fluxDensityPostfixOps(YottaTesla)

  def yG: A = fluxDensityPostfixOps(YoctoGauss)
  def zG: A = fluxDensityPostfixOps(ZeptoGauss)
  def aG: A = fluxDensityPostfixOps(AttoGauss)
  def fG: A = fluxDensityPostfixOps(FemtoGauss)
  def pG: A = fluxDensityPostfixOps(PicoGauss)
  def nG: A = fluxDensityPostfixOps(NanoGauss)
  def μG: A = fluxDensityPostfixOps(MicroGauss)
  def mG: A = fluxDensityPostfixOps(MilliGauss)
  def cG: A = fluxDensityPostfixOps(CentiGauss)
  def dG: A = fluxDensityPostfixOps(DeciGauss)
  def G : A = fluxDensityPostfixOps(Gauss)
  def daG: A = fluxDensityPostfixOps(DecaGauss)
  def hG: A = fluxDensityPostfixOps(HectoGauss)
  def kG: A = fluxDensityPostfixOps(KiloGauss)
  def MG: A = fluxDensityPostfixOps(MegaGauss)
  def GG: A = fluxDensityPostfixOps(GigaGauss)
  def TG: A = fluxDensityPostfixOps(TeraGauss)
  def PG: A = fluxDensityPostfixOps(PetaGauss)
  def EG: A = fluxDensityPostfixOps(ExaGauss)
  def ZG: A = fluxDensityPostfixOps(ZettaGauss)
  def YG: A = fluxDensityPostfixOps(YottaGauss)
}

class FluxDensity[A: Fractional](val value: A, val unit: FluxDensityUnit)
  extends Quantity[A, FluxDensityUnit]
    with FluxDensityPostfixOps[A]
    with FluxPostfixOps[DivisibleByAreaUnit[A]]
    with FluxPer[AreaPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: FluxDensityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInTesla) / real(evalUnit.unitInTesla)

  override protected def fluxDensityPostfixOps(fluxDensityUnit: FluxDensityUnit) = apply(fluxDensityUnit)

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = new DivisibleByAreaUnit[A]{
    override def /(areaUnit: AreaUnit) = apply(fluxUnit / areaUnit)
  }

  override protected def fluxPer(fluxUnit: FluxUnit) = new AreaPostfixOps[A]{
    override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(fluxUnit / areaUnit)
  }
}

sealed trait FluxDensityUnit
    extends PhysicalUnit[FluxDensityUnit]{

  def unitInTesla: Real

  override def baseUnit = FluxDensityUnit.Tesla
  override def valueInBaseUnit = unitInTesla
}

object FluxDensityUnit extends ConstantsDefined[FluxDensityUnit]{
  
  // intrinsic
  private[FluxDensityUnit]
  class IntrinsicFluxDensityUnit(val symbol: String, val unitInTesla: Real)
      extends FluxDensityUnit{

    def this(symbol: String, factor: Real, unit: FluxDensityUnit) =
      this(symbol, factor * unit.unitInTesla)
  }

  case object YoctoTesla extends IntrinsicFluxDensityUnit("yT", r"1e-24")
  case object ZeptoTesla extends IntrinsicFluxDensityUnit("zT", r"1e-21")
  case object AttoTesla  extends IntrinsicFluxDensityUnit("aT", r"1e-18")
  case object FemtoTesla extends IntrinsicFluxDensityUnit("fT", r"1e-15")
  case object PicoTesla  extends IntrinsicFluxDensityUnit("pT", r"1e-12")
  case object NanoTesla  extends IntrinsicFluxDensityUnit("nT", r"1e-9")
  case object MicroTesla extends IntrinsicFluxDensityUnit("μT", r"1e-6")
  case object MilliTesla extends IntrinsicFluxDensityUnit("mT", r"1e-3")
  case object CentiTesla extends IntrinsicFluxDensityUnit("cT", r"1e-2")
  case object DeciTesla  extends IntrinsicFluxDensityUnit("dT", r"1e-1")
  case object Tesla      extends IntrinsicFluxDensityUnit("T" , 1)
  case object DecaTesla  extends IntrinsicFluxDensityUnit("daT", r"1e1")
  case object HectoTesla extends IntrinsicFluxDensityUnit("hT", r"1e2")
  case object KiloTesla  extends IntrinsicFluxDensityUnit("kT", r"1e3")
  case object MegaTesla  extends IntrinsicFluxDensityUnit("MT", r"1e6")
  case object GigaTesla  extends IntrinsicFluxDensityUnit("GT", r"1e9")
  case object TeraTesla  extends IntrinsicFluxDensityUnit("TT", r"1e12")
  case object PetaTesla  extends IntrinsicFluxDensityUnit("PT", r"1e15")
  case object ExaTesla   extends IntrinsicFluxDensityUnit("ET", r"1e18")
  case object ZettaTesla extends IntrinsicFluxDensityUnit("ZT", r"1e21")
  case object YottaTesla extends IntrinsicFluxDensityUnit("YT", r"1e24")

  case object YoctoGauss extends IntrinsicFluxDensityUnit("yG", r"1e-24", Gauss)
  case object ZeptoGauss extends IntrinsicFluxDensityUnit("zG", r"1e-21", Gauss)
  case object AttoGauss  extends IntrinsicFluxDensityUnit("aG", r"1e-18", Gauss)
  case object FemtoGauss extends IntrinsicFluxDensityUnit("fG", r"1e-15", Gauss)
  case object PicoGauss  extends IntrinsicFluxDensityUnit("pG", r"1e-12", Gauss)
  case object NanoGauss  extends IntrinsicFluxDensityUnit("nG", r"1e-9", Gauss)
  case object MicroGauss extends IntrinsicFluxDensityUnit("μG", r"1e-6", Gauss)
  case object MilliGauss extends IntrinsicFluxDensityUnit("mG", r"1e-3", Gauss)
  case object CentiGauss extends IntrinsicFluxDensityUnit("cG", r"1e-2", Gauss)
  case object DeciGauss  extends IntrinsicFluxDensityUnit("dG", r"1e-1", Gauss)
  case object Gauss      extends IntrinsicFluxDensityUnit("G", r"1e-4")
  case object DecaGauss  extends IntrinsicFluxDensityUnit("daG", r"1e1", Gauss)
  case object HectoGauss extends IntrinsicFluxDensityUnit("hG", r"1e2", Gauss)
  case object KiloGauss  extends IntrinsicFluxDensityUnit("kG", r"1e3", Gauss)
  case object MegaGauss  extends IntrinsicFluxDensityUnit("MG", r"1e6", Gauss)
  case object GigaGauss  extends IntrinsicFluxDensityUnit("GG", r"1e9", Gauss)
  case object TeraGauss  extends IntrinsicFluxDensityUnit("TG", r"1e12", Gauss)
  case object PetaGauss  extends IntrinsicFluxDensityUnit("PG", r"1e15", Gauss)
  case object ExaGauss   extends IntrinsicFluxDensityUnit("EG", r"1e18", Gauss)
  case object ZettaGauss extends IntrinsicFluxDensityUnit("ZG", r"1e21", Gauss)
  case object YottaGauss extends IntrinsicFluxDensityUnit("YG", r"1e24", Gauss)

  override lazy val values = Seq(
    YoctoTesla,
    ZeptoTesla,
    AttoTesla,
    FemtoTesla,
    PicoTesla,
    NanoTesla,
    MicroTesla,
    MilliTesla,
    CentiTesla,
    DeciTesla,
    Tesla,
    DecaTesla,
    HectoTesla,
    KiloTesla,
    MegaTesla,
    GigaTesla,
    TeraTesla,
    PetaTesla,
    ExaTesla,
    ZettaTesla,
    YottaTesla,

    YoctoGauss,
    ZeptoGauss,
    AttoGauss,
    FemtoGauss,
    PicoGauss,
    NanoGauss,
    MicroGauss,
    MilliGauss,
    CentiGauss,
    DeciGauss,
    Gauss,
    DecaGauss,
    HectoGauss,
    KiloGauss,
    MegaGauss,
    GigaGauss,
    TeraGauss,
    PetaGauss,
    ExaGauss,
    ZettaGauss,
    YottaGauss
  )

  // Flux / Area -> FluxDensity
  private[FluxDensityUnit]
  class QuotientFluxDensityUnit(val numeratorUnit: FluxUnit, val denominatorUnit: AreaUnit)
      extends FluxDensityUnit with QuotientUnit[FluxDensityUnit, FluxUnit, AreaUnit]{

    override val unitInTesla: Real = numeratorUnit.unitInWeber / denominatorUnit.unitInSquareMetre
  }

  def apply(fUnit: FluxUnit, aUnit: AreaUnit) = new QuotientFluxDensityUnit(fUnit, aUnit)
}

trait PredefinedFluxDensityUnit extends FluxDensityPostfixOps[FluxDensityUnit]{

  override protected def fluxDensityPostfixOps(fluxDensityUnit: FluxDensityUnit) = fluxDensityUnit
}

object PredefinedFluxDensityUnit extends PredefinedFluxDensityUnit

trait FluxDensityFactory[A]
    extends FluxDensityPostfixOps[FluxDensity[A]]{

  def apply(unit: FluxDensityUnit): FluxDensity[A]

  override protected def fluxDensityPostfixOps(fluxDensityUnit: FluxDensityUnit) =
    apply(fluxDensityUnit)
}