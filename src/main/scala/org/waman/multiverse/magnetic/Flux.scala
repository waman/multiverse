package org.waman.multiverse.magnetic

import org.waman.multiverse._
import org.waman.multiverse.electric._
import org.waman.multiverse.metric.{AreaPostfixOps, AreaUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait FluxPostfixOps[A]{

  import FluxUnit._

  protected def fluxPostfixOps(fluxUnit: FluxUnit): A
  
  def yWb: A = fluxPostfixOps(YoctoWeber)
  def zWb: A = fluxPostfixOps(ZeptoWeber)
  def aWb: A = fluxPostfixOps(AttoWeber)
  def fWb: A = fluxPostfixOps(FemtoWeber)
  def pWb: A = fluxPostfixOps(PicoWeber)
  def nWb: A = fluxPostfixOps(NanoWeber)
  def μWb: A = fluxPostfixOps(MicroWeber)
  def mWb: A = fluxPostfixOps(MilliWeber)
  def cWb: A = fluxPostfixOps(CentiWeber)
  def dWb: A = fluxPostfixOps(DeciWeber)
  def Wb : A = fluxPostfixOps(Weber)
  def daWb: A = fluxPostfixOps(DecaWeber)
  def hWb: A = fluxPostfixOps(HectoWeber)
  def kWb: A = fluxPostfixOps(KiloWeber)
  def MWb: A = fluxPostfixOps(MegaWeber)
  def GWb: A = fluxPostfixOps(GigaWeber)
  def TWb: A = fluxPostfixOps(TeraWeber)
  def PWb: A = fluxPostfixOps(PetaWeber)
  def EWb: A = fluxPostfixOps(ExaWeber)
  def ZWb: A = fluxPostfixOps(ZettaWeber)
  def YWb: A = fluxPostfixOps(YottaWeber)

  def yMx: A = fluxPostfixOps(YoctoMaxwell)
  def zMx: A = fluxPostfixOps(ZeptoMaxwell)
  def aMx: A = fluxPostfixOps(AttoMaxwell)
  def fMx: A = fluxPostfixOps(FemtoMaxwell)
  def pMx: A = fluxPostfixOps(PicoMaxwell)
  def nMx: A = fluxPostfixOps(NanoMaxwell)
  def μMx: A = fluxPostfixOps(MicroMaxwell)
  def mMx: A = fluxPostfixOps(MilliMaxwell)
  def cMx: A = fluxPostfixOps(CentiMaxwell)
  def dMx: A = fluxPostfixOps(DeciMaxwell)
  def Mx : A = fluxPostfixOps(Maxwell)
  def daMx: A = fluxPostfixOps(DecaMaxwell)
  def hMx: A = fluxPostfixOps(HectoMaxwell)
  def kMx: A = fluxPostfixOps(KiloMaxwell)
  def MMx: A = fluxPostfixOps(MegaMaxwell)
  def GMx: A = fluxPostfixOps(GigaMaxwell)
  def TMx: A = fluxPostfixOps(TeraMaxwell)
  def PMx: A = fluxPostfixOps(PetaMaxwell)
  def EMx: A = fluxPostfixOps(ExaMaxwell)
  def ZMx: A = fluxPostfixOps(ZettaMaxwell)
  def YMx: A = fluxPostfixOps(YottaMaxwell)
}

trait FluxPer[A]{

  import FluxUnit._

  protected def fluxPer(fluxUnit: FluxUnit): A

  def yWb(per: Per): A = fluxPer(YoctoWeber)
  def zWb(per: Per): A = fluxPer(ZeptoWeber)
  def aWb(per: Per): A = fluxPer(AttoWeber)
  def fWb(per: Per): A = fluxPer(FemtoWeber)
  def pWb(per: Per): A = fluxPer(PicoWeber)
  def nWb(per: Per): A = fluxPer(NanoWeber)
  def μWb(per: Per): A = fluxPer(MicroWeber)
  def mWb(per: Per): A = fluxPer(MilliWeber)
  def cWb(per: Per): A = fluxPer(CentiWeber)
  def dWb(per: Per): A = fluxPer(DeciWeber)
  def Wb (per: Per): A = fluxPer(Weber)
  def daWb(per: Per): A = fluxPer(DecaWeber)
  def hWb(per: Per): A = fluxPer(HectoWeber)
  def kWb(per: Per): A = fluxPer(KiloWeber)
  def MWb(per: Per): A = fluxPer(MegaWeber)
  def GWb(per: Per): A = fluxPer(GigaWeber)
  def TWb(per: Per): A = fluxPer(TeraWeber)
  def PWb(per: Per): A = fluxPer(PetaWeber)
  def EWb(per: Per): A = fluxPer(ExaWeber)
  def ZWb(per: Per): A = fluxPer(ZettaWeber)
  def YWb(per: Per): A = fluxPer(YottaWeber)

  def yMx(per: Per): A = fluxPer(YoctoMaxwell)
  def zMx(per: Per): A = fluxPer(ZeptoMaxwell)
  def aMx(per: Per): A = fluxPer(AttoMaxwell)
  def fMx(per: Per): A = fluxPer(FemtoMaxwell)
  def pMx(per: Per): A = fluxPer(PicoMaxwell)
  def nMx(per: Per): A = fluxPer(NanoMaxwell)
  def μMx(per: Per): A = fluxPer(MicroMaxwell)
  def mMx(per: Per): A = fluxPer(MilliMaxwell)
  def cMx(per: Per): A = fluxPer(CentiMaxwell)
  def dMx(per: Per): A = fluxPer(DeciMaxwell)
  def Mx (per: Per): A = fluxPer(Maxwell)
  def daMx(per: Per): A = fluxPer(DecaMaxwell)
  def hMx(per: Per): A = fluxPer(HectoMaxwell)
  def kMx(per: Per): A = fluxPer(KiloMaxwell)
  def MMx(per: Per): A = fluxPer(MegaMaxwell)
  def GMx(per: Per): A = fluxPer(GigaMaxwell)
  def TMx(per: Per): A = fluxPer(TeraMaxwell)
  def PMx(per: Per): A = fluxPer(PetaMaxwell)
  def EMx(per: Per): A = fluxPer(ExaMaxwell)
  def ZMx(per: Per): A = fluxPer(ZettaMaxwell)
  def YMx(per: Per): A = fluxPer(YottaMaxwell)
}

class Flux[A: Fractional](val value: A, val unit: FluxUnit)
  extends Quantity[A, FluxUnit]
    with FluxPostfixOps[A]
    with DivisibleByAreaUnit[FluxDensity[A]]
    with DivisibleByCurrentUnit[Inductance[A]]
    with VoltagePostfixOps[MultiplicativeByTimeUnit[A]]
    with VoltageDot[TimePostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: FluxUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInWeber) / real(evalUnit.unitInWeber)

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = apply(fluxUnit)

  override def /(areaUnit: AreaUnit) = new FluxDensity(value, unit / areaUnit)

  override def /(currentUnit: CurrentUnit) = new Inductance(value, unit / currentUnit)

  override protected def voltagePostfixOps(voltageUnit: VoltageUnit) = new MultiplicativeByTimeUnit[A]{
    override def *(timeUnit: TimeUnit) = apply(voltageUnit * timeUnit)
  }

  override protected def voltageDot(voltageUnit: VoltageUnit) = new TimePostfixOps[A]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(voltageUnit * timeUnit)
  }
}

sealed trait FluxUnit
    extends PhysicalUnit[FluxUnit]
    with DivisibleByAreaUnit[FluxDensityUnit]
    with DivisibleByCurrentUnit[InductanceUnit]{
  
  def unitInWeber: Real

  override def baseUnit = FluxUnit.Weber
  override def valueInBaseUnit = unitInWeber

  override def /(areaUnit: AreaUnit) = FluxDensityUnit(this, areaUnit)

  override def /(currentUnit: CurrentUnit) = InductanceUnit(this, currentUnit)
}

object FluxUnit extends ConstantsDefined[FluxUnit]{
  
  // intrinsic
  private[FluxUnit]
  class IntrinsicFluxUnit(val symbol: String, val unitInWeber: Real)
    extends FluxUnit{

    def this(symbol: String, factor: Real, unit: FluxUnit) =
      this(symbol, factor * unit.unitInWeber)
  }
  
  case object YoctoWeber extends IntrinsicFluxUnit("yWb", r"1e-24")
  case object ZeptoWeber extends IntrinsicFluxUnit("zWb", r"1e-21")
  case object AttoWeber  extends IntrinsicFluxUnit("aWb", r"1e-18")
  case object FemtoWeber extends IntrinsicFluxUnit("fWb", r"1e-15")
  case object PicoWeber  extends IntrinsicFluxUnit("pWb", r"1e-12")
  case object NanoWeber  extends IntrinsicFluxUnit("nWb", r"1e-9")
  case object MicroWeber extends IntrinsicFluxUnit("μWb", r"1e-6")
  case object MilliWeber extends IntrinsicFluxUnit("mWb", r"1e-3")
  case object CentiWeber extends IntrinsicFluxUnit("cWb", r"1e-2")
  case object DeciWeber  extends IntrinsicFluxUnit("dWb", r"1e-1")
  case object Weber      extends IntrinsicFluxUnit("Wb" , 1)
  case object DecaWeber  extends IntrinsicFluxUnit("daWb", r"1e1")
  case object HectoWeber extends IntrinsicFluxUnit("hWb", r"1e2")
  case object KiloWeber  extends IntrinsicFluxUnit("kWb", r"1e3")
  case object MegaWeber  extends IntrinsicFluxUnit("MWb", r"1e6")
  case object GigaWeber  extends IntrinsicFluxUnit("GWb", r"1e9")
  case object TeraWeber  extends IntrinsicFluxUnit("TWb", r"1e12")
  case object PetaWeber  extends IntrinsicFluxUnit("PWb", r"1e15")
  case object ExaWeber   extends IntrinsicFluxUnit("EWb", r"1e18")
  case object ZettaWeber extends IntrinsicFluxUnit("ZWb", r"1e21")
  case object YottaWeber extends IntrinsicFluxUnit("YWb", r"1e24")

  case object YoctoMaxwell extends IntrinsicFluxUnit("yMx", r"1e-24", Maxwell)
  case object ZeptoMaxwell extends IntrinsicFluxUnit("zMx", r"1e-21", Maxwell)
  case object AttoMaxwell  extends IntrinsicFluxUnit("aMx", r"1e-18", Maxwell)
  case object FemtoMaxwell extends IntrinsicFluxUnit("fMx", r"1e-15", Maxwell)
  case object PicoMaxwell  extends IntrinsicFluxUnit("pMx", r"1e-12", Maxwell)
  case object NanoMaxwell  extends IntrinsicFluxUnit("nMx", r"1e-9", Maxwell)
  case object MicroMaxwell extends IntrinsicFluxUnit("μMx", r"1e-6", Maxwell)
  case object MilliMaxwell extends IntrinsicFluxUnit("mMx", r"1e-3", Maxwell)
  case object CentiMaxwell extends IntrinsicFluxUnit("cMx", r"1e-2", Maxwell)
  case object DeciMaxwell  extends IntrinsicFluxUnit("dMx", r"1e-1", Maxwell)
  case object Maxwell      extends IntrinsicFluxUnit("Mx", r"1e-8")
  case object DecaMaxwell  extends IntrinsicFluxUnit("daMx", r"1e1", Maxwell)
  case object HectoMaxwell extends IntrinsicFluxUnit("hMx", r"1e2", Maxwell)
  case object KiloMaxwell  extends IntrinsicFluxUnit("kMx", r"1e3", Maxwell)
  case object MegaMaxwell  extends IntrinsicFluxUnit("MMx", r"1e6", Maxwell)
  case object GigaMaxwell  extends IntrinsicFluxUnit("GMx", r"1e9", Maxwell)
  case object TeraMaxwell  extends IntrinsicFluxUnit("TMx", r"1e12", Maxwell)
  case object PetaMaxwell  extends IntrinsicFluxUnit("PMx", r"1e15", Maxwell)
  case object ExaMaxwell   extends IntrinsicFluxUnit("EMx", r"1e18", Maxwell)
  case object ZettaMaxwell extends IntrinsicFluxUnit("ZMx", r"1e21", Maxwell)
  case object YottaMaxwell extends IntrinsicFluxUnit("YMx", r"1e24", Maxwell)

  override lazy val values = Seq(
    YoctoWeber,
    ZeptoWeber,
    AttoWeber,
    FemtoWeber,
    PicoWeber,
    NanoWeber,
    MicroWeber,
    MilliWeber,
    CentiWeber,
    DeciWeber,
    Weber,
    DecaWeber,
    HectoWeber,
    KiloWeber,
    MegaWeber,
    GigaWeber,
    TeraWeber,
    PetaWeber,
    ExaWeber,
    ZettaWeber,
    YottaWeber,

    YoctoMaxwell,
    ZeptoMaxwell,
    AttoMaxwell,
    FemtoMaxwell,
    PicoMaxwell,
    NanoMaxwell,
    MicroMaxwell,
    MilliMaxwell,
    CentiMaxwell,
    DeciMaxwell,
    Maxwell,
    DecaMaxwell,
    HectoMaxwell,
    KiloMaxwell,
    MegaMaxwell,
    GigaMaxwell,
    TeraMaxwell,
    PetaMaxwell,
    ExaMaxwell,
    ZettaMaxwell,
    YottaMaxwell
  )

  // Voltage * Time -> Flux
  private[FluxUnit]
  class ProductFluxUnit(val firstUnit: VoltageUnit, val secondUnit: TimeUnit)
    extends FluxUnit with ProductUnit[FluxUnit, VoltageUnit, TimeUnit]{

    override val unitInWeber: Real = firstUnit.unitInVolt * secondUnit.unitInSecond
  }

  def apply(vUnit: VoltageUnit, tUnit: TimeUnit) = new ProductFluxUnit(vUnit, tUnit)
}

trait PredefinedFluxUnit extends FluxPostfixOps[FluxUnit]{

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = fluxUnit
}

object PredefinedFluxUnit extends PredefinedFluxUnit

trait FluxFactory[A]
    extends FluxPostfixOps[Flux[A]]
    with FluxPer[AreaPostfixOps[FluxDensity[A]] with CurrentPostfixOps[Inductance[A]]]{

  def apply(unit: FluxUnit): Flux[A]

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = apply(fluxUnit)

  // Flux / Area -> FluxDensity
  // Flux / Current -> Inductance
  def apply(fdUnit: FluxDensityUnit): FluxDensity[A]
  def apply(iUnit: InductanceUnit): Inductance[A]

  override protected def fluxPer(fluxUnit: FluxUnit) =
    new AreaPostfixOps[FluxDensity[A]] with CurrentPostfixOps[Inductance[A]]{

      override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(fluxUnit / areaUnit)
      override protected def currentPostfixOps(currentUnit: CurrentUnit) = apply(fluxUnit / currentUnit)
    }
}