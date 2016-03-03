package org.waman.multiverse.magnetic

import org.waman.multiverse._
import org.waman.multiverse.electric.{CurrentPostfixOps, CurrentUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait InductancePostfixOps[A]{

  import InductanceUnit._

  protected def inductancePostfixOps(inductanceUnit: InductanceUnit): A

  def yH: A = inductancePostfixOps(YoctoHenry)
  def zH: A = inductancePostfixOps(ZeptoHenry)
  def aH: A = inductancePostfixOps(AttoHenry)
  def fH: A = inductancePostfixOps(FemtoHenry)
  def pH: A = inductancePostfixOps(PicoHenry)
  def nH: A = inductancePostfixOps(NanoHenry)
  def microHenry: A = inductancePostfixOps(MicroHenry)
  def microH: A = microHenry
  def μH: A = microHenry
  def mH: A = inductancePostfixOps(MilliHenry)
  def cH: A = inductancePostfixOps(CentiHenry)
  def dH: A = inductancePostfixOps(DeciHenry)
  def H : A = inductancePostfixOps(Henry)
  def daH: A = inductancePostfixOps(DecaHenry)
  def hH: A = inductancePostfixOps(HectoHenry)
  def kH: A = inductancePostfixOps(KiloHenry)
  def MH: A = inductancePostfixOps(MegaHenry)
  def GH: A = inductancePostfixOps(GigaHenry)
  def TH: A = inductancePostfixOps(TeraHenry)
  def PH: A = inductancePostfixOps(PetaHenry)
  def EH: A = inductancePostfixOps(ExaHenry)
  def ZH: A = inductancePostfixOps(ZettaHenry)
  def YH: A = inductancePostfixOps(YottaHenry)
}

class Inductance[A: Fractional](val value: A, val unit: InductanceUnit)
  extends Quantity[A, InductanceUnit]
    with InductancePostfixOps[A]
    with FluxPostfixOps[DivisibleByCurrentUnit[A]]
    with FluxPer[CurrentPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: InductanceUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInHenry) / real(evalUnit.unitInHenry)

  override protected def inductancePostfixOps(inductanceUnit: InductanceUnit) = apply(inductanceUnit)

  override protected def fluxPostfixOps(fluxUnit: FluxUnit) = new DivisibleByCurrentUnit[A]{
    override def /(currentUnit: CurrentUnit) = apply(fluxUnit / currentUnit)
  }

  override protected def fluxPer(fluxUnit: FluxUnit) = new CurrentPostfixOps[A]{
    override protected def currentPostfixOps(currentUnit: CurrentUnit) = apply(fluxUnit / currentUnit)
  }
}

sealed trait InductanceUnit extends PhysicalUnit[InductanceUnit]{
  
  def unitInHenry: Real

  override def baseUnit = InductanceUnit.Henry
  override def valueInBaseUnit = unitInHenry
}

object InductanceUnit extends ConstantsDefined[InductanceUnit]{

  import scala.language.implicitConversions
  implicit def convertToSeq(s: String): Seq[String] = Seq(s)
  
  // intrinsic
  private[InductanceUnit]
  class IntrinsicInductanceUnit(val symbols: Seq[String], val unitInHenry: Real)
      extends InductanceUnit{
  }

  case object YoctoHenry extends IntrinsicInductanceUnit("yH", r"1e-24")
  case object ZeptoHenry extends IntrinsicInductanceUnit("zH", r"1e-21")
  case object AttoHenry  extends IntrinsicInductanceUnit("aH", r"1e-18")
  case object FemtoHenry extends IntrinsicInductanceUnit("fH", r"1e-15")
  case object PicoHenry  extends IntrinsicInductanceUnit("pH", r"1e-12")
  case object NanoHenry  extends IntrinsicInductanceUnit("nH", r"1e-9")
  case object MicroHenry extends IntrinsicInductanceUnit(Seq("μH", "microHenry", "microH"), r"1e-6")
  case object MilliHenry extends IntrinsicInductanceUnit("mH", r"1e-3")
  case object CentiHenry extends IntrinsicInductanceUnit("cH", r"1e-2")
  case object DeciHenry  extends IntrinsicInductanceUnit("dH", r"1e-1")
  case object Henry      extends IntrinsicInductanceUnit("H" , 1)
  case object DecaHenry  extends IntrinsicInductanceUnit("daH", r"1e1")
  case object HectoHenry extends IntrinsicInductanceUnit("hH", r"1e2")
  case object KiloHenry  extends IntrinsicInductanceUnit("kH", r"1e3")
  case object MegaHenry  extends IntrinsicInductanceUnit("MH", r"1e6")
  case object GigaHenry  extends IntrinsicInductanceUnit("GH", r"1e9")
  case object TeraHenry  extends IntrinsicInductanceUnit("TH", r"1e12")
  case object PetaHenry  extends IntrinsicInductanceUnit("PH", r"1e15")
  case object ExaHenry   extends IntrinsicInductanceUnit("EH", r"1e18")
  case object ZettaHenry extends IntrinsicInductanceUnit("ZH", r"1e21")
  case object YottaHenry extends IntrinsicInductanceUnit("YH", r"1e24")

  override lazy val values = Seq(
    YoctoHenry,
    ZeptoHenry,
    AttoHenry,
    FemtoHenry,
    PicoHenry,
    NanoHenry,
    MicroHenry,
    MilliHenry,
    CentiHenry,
    DeciHenry,
    Henry,
    DecaHenry,
    HectoHenry,
    KiloHenry,
    MegaHenry,
    GigaHenry,
    TeraHenry,
    PetaHenry,
    ExaHenry,
    ZettaHenry,
    YottaHenry
  )
  
  // MagneticFlux / Area -> Inductance
  private[InductanceUnit]
  class QuotientInductanceUnit(val numeratorUnit: FluxUnit, val denominatorUnit: CurrentUnit)
    extends InductanceUnit with QuotientUnit[InductanceUnit, FluxUnit, CurrentUnit]{

    override lazy val unitInHenry: Real =
      numeratorUnit.unitInWeber / denominatorUnit.unitInAmpere
  }

  def apply(mUnit: FluxUnit, cUnit: CurrentUnit): InductanceUnit =
    new QuotientInductanceUnit(mUnit, cUnit)
}

trait PredefinedInductanceUnit extends InductancePostfixOps[InductanceUnit]{

  override protected def inductancePostfixOps(inductanceUnit: InductanceUnit) = inductanceUnit
}

object PredefinedInductanceUnit extends PredefinedInductanceUnit

trait InductanceFactory[A]
    extends InductancePostfixOps[Inductance[A]]{

  def apply(unit: InductanceUnit): Inductance[A]

  override protected def inductancePostfixOps(inductanceUnit: InductanceUnit) =
    apply(inductanceUnit)
}