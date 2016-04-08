package org.waman.multiverse.magnetic

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._
import org.waman.multiverse.time._
import org.waman.multiverse.electric._

sealed trait FluxUnit extends PhysicalUnit[FluxUnit]
  with DivisibleByAreaUnit[FluxDensityUnit]
  with DivisibleByCurrentUnit[InductanceUnit]{

  override def getSIUnit = org.waman.multiverse.magnetic.FluxUnit.Weber

  override def /(unit: AreaUnit) = FluxDensityUnit(this, unit)

  override def /(unit: CurrentUnit) = InductanceUnit(this, unit)
}

object FluxUnit extends ConstantsDefined[FluxUnit]{

  // intrinsic
  private[FluxUnit]
  class IntrinsicFluxUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends FluxUnit{

    def this(name: String, symbols: Seq[String], unit: FluxUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: FluxUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoWeber extends IntrinsicFluxUnit("YoctoWeber", Seq("yWb"), r"1e-24")
  case object ZeptoWeber extends IntrinsicFluxUnit("ZeptoWeber", Seq("zWb"), r"1e-21")
  case object AttoWeber extends IntrinsicFluxUnit("AttoWeber", Seq("aWb"), r"1e-18")
  case object FemtoWeber extends IntrinsicFluxUnit("FemtoWeber", Seq("fWb"), r"1e-15")
  case object PicoWeber extends IntrinsicFluxUnit("PicoWeber", Seq("pWb"), r"1e-12")
  case object NanoWeber extends IntrinsicFluxUnit("NanoWeber", Seq("nWb"), r"1e-9")
  case object MicroWeber extends IntrinsicFluxUnit("MicroWeber", Seq("μWb", "mcWb"), r"1e-6")
  case object MilliWeber extends IntrinsicFluxUnit("MilliWeber", Seq("mWb"), r"1e-3")
  case object CentiWeber extends IntrinsicFluxUnit("CentiWeber", Seq("cWb"), r"1e-2")
  case object DeciWeber extends IntrinsicFluxUnit("DeciWeber", Seq("dWb"), r"1e-1")
  case object Weber extends IntrinsicFluxUnit("Weber", Seq("Wb"), r"1")
  case object DecaWeber extends IntrinsicFluxUnit("DecaWeber", Seq("daWb"), r"1e1")
  case object HectoWeber extends IntrinsicFluxUnit("HectoWeber", Seq("hWb"), r"1e2")
  case object KiloWeber extends IntrinsicFluxUnit("KiloWeber", Seq("kWb"), r"1e3")
  case object MegaWeber extends IntrinsicFluxUnit("MegaWeber", Seq("MWb"), r"1e6")
  case object GigaWeber extends IntrinsicFluxUnit("GigaWeber", Seq("GWb"), r"1e9")
  case object TeraWeber extends IntrinsicFluxUnit("TeraWeber", Seq("TWb"), r"1e12")
  case object PetaWeber extends IntrinsicFluxUnit("PetaWeber", Seq("PWb"), r"1e15")
  case object ExaWeber extends IntrinsicFluxUnit("ExaWeber", Seq("EWb"), r"1e18")
  case object ZettaWeber extends IntrinsicFluxUnit("ZettaWeber", Seq("ZWb"), r"1e21")
  case object YottaWeber extends IntrinsicFluxUnit("YottaWeber", Seq("YWb"), r"1e24")
  case object YoctoMaxwell extends IntrinsicFluxUnit("YoctoMaxwell", Seq("yMx"), r"1e-24" * r"1e-8")
  case object ZeptoMaxwell extends IntrinsicFluxUnit("ZeptoMaxwell", Seq("zMx"), r"1e-21" * r"1e-8")
  case object AttoMaxwell extends IntrinsicFluxUnit("AttoMaxwell", Seq("aMx"), r"1e-18" * r"1e-8")
  case object FemtoMaxwell extends IntrinsicFluxUnit("FemtoMaxwell", Seq("fMx"), r"1e-15" * r"1e-8")
  case object PicoMaxwell extends IntrinsicFluxUnit("PicoMaxwell", Seq("pMx"), r"1e-12" * r"1e-8")
  case object NanoMaxwell extends IntrinsicFluxUnit("NanoMaxwell", Seq("nMx"), r"1e-9" * r"1e-8")
  case object MicroMaxwell extends IntrinsicFluxUnit("MicroMaxwell", Seq("μMx", "mcMx"), r"1e-6" * r"1e-8")
  case object MilliMaxwell extends IntrinsicFluxUnit("MilliMaxwell", Seq("mMx"), r"1e-3" * r"1e-8")
  case object CentiMaxwell extends IntrinsicFluxUnit("CentiMaxwell", Seq("cMx"), r"1e-2" * r"1e-8")
  case object DeciMaxwell extends IntrinsicFluxUnit("DeciMaxwell", Seq("dMx"), r"1e-1" * r"1e-8")
  case object Maxwell extends IntrinsicFluxUnit("Maxwell", Seq("Mx"), r"1" * r"1e-8")
  case object DecaMaxwell extends IntrinsicFluxUnit("DecaMaxwell", Seq("daMx"), r"1e1" * r"1e-8")
  case object HectoMaxwell extends IntrinsicFluxUnit("HectoMaxwell", Seq("hMx"), r"1e2" * r"1e-8")
  case object KiloMaxwell extends IntrinsicFluxUnit("KiloMaxwell", Seq("kMx"), r"1e3" * r"1e-8")
  case object MegaMaxwell extends IntrinsicFluxUnit("MegaMaxwell", Seq("MMx"), r"1e6" * r"1e-8")
  case object GigaMaxwell extends IntrinsicFluxUnit("GigaMaxwell", Seq("GMx"), r"1e9" * r"1e-8")
  case object TeraMaxwell extends IntrinsicFluxUnit("TeraMaxwell", Seq("TMx"), r"1e12" * r"1e-8")
  case object PetaMaxwell extends IntrinsicFluxUnit("PetaMaxwell", Seq("PMx"), r"1e15" * r"1e-8")
  case object ExaMaxwell extends IntrinsicFluxUnit("ExaMaxwell", Seq("EMx"), r"1e18" * r"1e-8")
  case object ZettaMaxwell extends IntrinsicFluxUnit("ZettaMaxwell", Seq("ZMx"), r"1e21" * r"1e-8")
  case object YottaMaxwell extends IntrinsicFluxUnit("YottaMaxwell", Seq("YMx"), r"1e24" * r"1e-8")

  override lazy val values = Seq(YoctoWeber, ZeptoWeber, AttoWeber, FemtoWeber, PicoWeber, NanoWeber, MicroWeber, MilliWeber, CentiWeber, DeciWeber, Weber, DecaWeber, HectoWeber, KiloWeber, MegaWeber, GigaWeber, TeraWeber, PetaWeber, ExaWeber, ZettaWeber, YottaWeber, YoctoMaxwell, ZeptoMaxwell, AttoMaxwell, FemtoMaxwell, PicoMaxwell, NanoMaxwell, MicroMaxwell, MilliMaxwell, CentiMaxwell, DeciMaxwell, Maxwell, DecaMaxwell, HectoMaxwell, KiloMaxwell, MegaMaxwell, GigaMaxwell, TeraMaxwell, PetaMaxwell, ExaMaxwell, ZettaMaxwell, YottaMaxwell)

  // VoltageUnit * TimeUnit -> Flux
  private[FluxUnit]
  class ProductVoltageDotTimeUnit(val firstUnit: VoltageUnit, val secondUnit: TimeUnit)
      extends FluxUnit with ProductUnit[FluxUnit, VoltageUnit, TimeUnit]{

    override lazy val unitValueInSIUnit: Real =
      firstUnit.unitValueInSIUnit * secondUnit.unitValueInSIUnit
  }

  def apply(unit1: VoltageUnit, unit2: TimeUnit): FluxUnit =
    new ProductVoltageDotTimeUnit(unit1, unit2)
}

trait MultiplicativeByFluxUnit[R]{
  def *(unit: FluxUnit): R
}

trait DivisibleByFluxUnit[R]{
  def /(unit: FluxUnit): R
}

trait FluxPostfixOps[A]{
  import FluxUnit._

  protected def fluxPostfixOps(unit: FluxUnit): A


  def yWb : A = fluxPostfixOps(YoctoWeber)
  def zWb : A = fluxPostfixOps(ZeptoWeber)
  def aWb : A = fluxPostfixOps(AttoWeber)
  def fWb : A = fluxPostfixOps(FemtoWeber)
  def pWb : A = fluxPostfixOps(PicoWeber)
  def nWb : A = fluxPostfixOps(NanoWeber)
  def μWb : A = fluxPostfixOps(MicroWeber)
  def mcWb : A = fluxPostfixOps(MicroWeber)
  def mWb : A = fluxPostfixOps(MilliWeber)
  def cWb : A = fluxPostfixOps(CentiWeber)
  def dWb : A = fluxPostfixOps(DeciWeber)
  def Wb : A = fluxPostfixOps(Weber)
  def daWb : A = fluxPostfixOps(DecaWeber)
  def hWb : A = fluxPostfixOps(HectoWeber)
  def kWb : A = fluxPostfixOps(KiloWeber)
  def MWb : A = fluxPostfixOps(MegaWeber)
  def GWb : A = fluxPostfixOps(GigaWeber)
  def TWb : A = fluxPostfixOps(TeraWeber)
  def PWb : A = fluxPostfixOps(PetaWeber)
  def EWb : A = fluxPostfixOps(ExaWeber)
  def ZWb : A = fluxPostfixOps(ZettaWeber)
  def YWb : A = fluxPostfixOps(YottaWeber)
  def yMx : A = fluxPostfixOps(YoctoMaxwell)
  def zMx : A = fluxPostfixOps(ZeptoMaxwell)
  def aMx : A = fluxPostfixOps(AttoMaxwell)
  def fMx : A = fluxPostfixOps(FemtoMaxwell)
  def pMx : A = fluxPostfixOps(PicoMaxwell)
  def nMx : A = fluxPostfixOps(NanoMaxwell)
  def μMx : A = fluxPostfixOps(MicroMaxwell)
  def mcMx : A = fluxPostfixOps(MicroMaxwell)
  def mMx : A = fluxPostfixOps(MilliMaxwell)
  def cMx : A = fluxPostfixOps(CentiMaxwell)
  def dMx : A = fluxPostfixOps(DeciMaxwell)
  def Mx : A = fluxPostfixOps(Maxwell)
  def daMx : A = fluxPostfixOps(DecaMaxwell)
  def hMx : A = fluxPostfixOps(HectoMaxwell)
  def kMx : A = fluxPostfixOps(KiloMaxwell)
  def MMx : A = fluxPostfixOps(MegaMaxwell)
  def GMx : A = fluxPostfixOps(GigaMaxwell)
  def TMx : A = fluxPostfixOps(TeraMaxwell)
  def PMx : A = fluxPostfixOps(PetaMaxwell)
  def EMx : A = fluxPostfixOps(ExaMaxwell)
  def ZMx : A = fluxPostfixOps(ZettaMaxwell)
  def YMx : A = fluxPostfixOps(YottaMaxwell)
}

trait FluxDot[A]{
  import FluxUnit._

  protected def fluxDot(unit: FluxUnit): A

  def yWb(dot: Dot): A = fluxDot(YoctoWeber)
  def zWb(dot: Dot): A = fluxDot(ZeptoWeber)
  def aWb(dot: Dot): A = fluxDot(AttoWeber)
  def fWb(dot: Dot): A = fluxDot(FemtoWeber)
  def pWb(dot: Dot): A = fluxDot(PicoWeber)
  def nWb(dot: Dot): A = fluxDot(NanoWeber)
  def μWb(dot: Dot): A = fluxDot(MicroWeber)
  def mcWb(dot: Dot): A = fluxDot(MicroWeber)
  def mWb(dot: Dot): A = fluxDot(MilliWeber)
  def cWb(dot: Dot): A = fluxDot(CentiWeber)
  def dWb(dot: Dot): A = fluxDot(DeciWeber)
  def Wb(dot: Dot): A = fluxDot(Weber)
  def daWb(dot: Dot): A = fluxDot(DecaWeber)
  def hWb(dot: Dot): A = fluxDot(HectoWeber)
  def kWb(dot: Dot): A = fluxDot(KiloWeber)
  def MWb(dot: Dot): A = fluxDot(MegaWeber)
  def GWb(dot: Dot): A = fluxDot(GigaWeber)
  def TWb(dot: Dot): A = fluxDot(TeraWeber)
  def PWb(dot: Dot): A = fluxDot(PetaWeber)
  def EWb(dot: Dot): A = fluxDot(ExaWeber)
  def ZWb(dot: Dot): A = fluxDot(ZettaWeber)
  def YWb(dot: Dot): A = fluxDot(YottaWeber)
  def yMx(dot: Dot): A = fluxDot(YoctoMaxwell)
  def zMx(dot: Dot): A = fluxDot(ZeptoMaxwell)
  def aMx(dot: Dot): A = fluxDot(AttoMaxwell)
  def fMx(dot: Dot): A = fluxDot(FemtoMaxwell)
  def pMx(dot: Dot): A = fluxDot(PicoMaxwell)
  def nMx(dot: Dot): A = fluxDot(NanoMaxwell)
  def μMx(dot: Dot): A = fluxDot(MicroMaxwell)
  def mcMx(dot: Dot): A = fluxDot(MicroMaxwell)
  def mMx(dot: Dot): A = fluxDot(MilliMaxwell)
  def cMx(dot: Dot): A = fluxDot(CentiMaxwell)
  def dMx(dot: Dot): A = fluxDot(DeciMaxwell)
  def Mx(dot: Dot): A = fluxDot(Maxwell)
  def daMx(dot: Dot): A = fluxDot(DecaMaxwell)
  def hMx(dot: Dot): A = fluxDot(HectoMaxwell)
  def kMx(dot: Dot): A = fluxDot(KiloMaxwell)
  def MMx(dot: Dot): A = fluxDot(MegaMaxwell)
  def GMx(dot: Dot): A = fluxDot(GigaMaxwell)
  def TMx(dot: Dot): A = fluxDot(TeraMaxwell)
  def PMx(dot: Dot): A = fluxDot(PetaMaxwell)
  def EMx(dot: Dot): A = fluxDot(ExaMaxwell)
  def ZMx(dot: Dot): A = fluxDot(ZettaMaxwell)
  def YMx(dot: Dot): A = fluxDot(YottaMaxwell)
}

trait FluxPer[A]{
  import FluxUnit._

  protected def fluxPer(unit: FluxUnit): A

  def yWb(per: Per): A = fluxPer(YoctoWeber)
  def zWb(per: Per): A = fluxPer(ZeptoWeber)
  def aWb(per: Per): A = fluxPer(AttoWeber)
  def fWb(per: Per): A = fluxPer(FemtoWeber)
  def pWb(per: Per): A = fluxPer(PicoWeber)
  def nWb(per: Per): A = fluxPer(NanoWeber)
  def μWb(per: Per): A = fluxPer(MicroWeber)
  def mcWb(per: Per): A = fluxPer(MicroWeber)
  def mWb(per: Per): A = fluxPer(MilliWeber)
  def cWb(per: Per): A = fluxPer(CentiWeber)
  def dWb(per: Per): A = fluxPer(DeciWeber)
  def Wb(per: Per): A = fluxPer(Weber)
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
  def mcMx(per: Per): A = fluxPer(MicroMaxwell)
  def mMx(per: Per): A = fluxPer(MilliMaxwell)
  def cMx(per: Per): A = fluxPer(CentiMaxwell)
  def dMx(per: Per): A = fluxPer(DeciMaxwell)
  def Mx(per: Per): A = fluxPer(Maxwell)
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

trait PredefinedFluxUnit extends FluxPostfixOps[FluxUnit]{
  override protected def fluxPostfixOps(unit: FluxUnit) = unit
  
}

object PredefinedFluxUnit extends PredefinedFluxUnit
