package org.waman.multiverse.electric

import org.waman.multiverse._
import org.waman.multiverse.time._
import spire.implicits._
import spire.math.Real

sealed trait CurrentUnit extends PhysicalUnit[CurrentUnit]
  with MultiplicativeByTimeUnit[ChargeUnit]{

  def unitInAmpere: Real

  override def baseUnit = org.waman.multiverse.electric.CurrentUnit.Ampere
  override def valueInBaseUnit = unitInAmpere

  override def *(unit: TimeUnit) = ChargeUnit(this, unit)
}

object CurrentUnit extends ConstantsDefined[CurrentUnit]{

  // intrinsic
  private[CurrentUnit]
  class IntrinsicCurrentUnit(name: String, val symbols: Seq[String], val unitInAmpere: Real)
      extends CurrentUnit{

    def this(name: String, symbols: Seq[String], unit: CurrentUnit) =
      this(name, symbols, unit.unitInAmpere)

    def this(name: String, symbols: Seq[String], factor: Real, unit: CurrentUnit) =
      this(name, symbols, factor * unit.unitInAmpere)
  }


  case object YoctoAmpere extends IntrinsicCurrentUnit("YoctoAmpere", Seq("yA"), r"1e-24")
  case object ZeptoAmpere extends IntrinsicCurrentUnit("ZeptoAmpere", Seq("zA"), r"1e-21")
  case object AttoAmpere extends IntrinsicCurrentUnit("AttoAmpere", Seq("aA"), r"1e-18")
  case object FemtoAmpere extends IntrinsicCurrentUnit("FemtoAmpere", Seq("fA"), r"1e-15")
  case object PicoAmpere extends IntrinsicCurrentUnit("PicoAmpere", Seq("pA"), r"1e-12")
  case object NanoAmpere extends IntrinsicCurrentUnit("NanoAmpere", Seq("nA"), r"1e-9")
  case object MicroAmpere extends IntrinsicCurrentUnit("MicroAmpere", Seq("microAmpere", "microA", "μA"), r"1e-6")
  case object MilliAmpere extends IntrinsicCurrentUnit("MilliAmpere", Seq("mA"), r"1e-3")
  case object CentiAmpere extends IntrinsicCurrentUnit("CentiAmpere", Seq("cA"), r"1e-2")
  case object DeciAmpere extends IntrinsicCurrentUnit("DeciAmpere", Seq("dA"), r"1e-1")
  case object Ampere extends IntrinsicCurrentUnit("Ampere", Seq("A"), r"1")
  case object DecaAmpere extends IntrinsicCurrentUnit("DecaAmpere", Seq("daA"), r"1e1")
  case object HectoAmpere extends IntrinsicCurrentUnit("HectoAmpere", Seq("hA"), r"1e2")
  case object KiloAmpere extends IntrinsicCurrentUnit("KiloAmpere", Seq("kA"), r"1e3")
  case object MegaAmpere extends IntrinsicCurrentUnit("MegaAmpere", Seq("MA"), r"1e6")
  case object GigaAmpere extends IntrinsicCurrentUnit("GigaAmpere", Seq("GA"), r"1e9")
  case object TeraAmpere extends IntrinsicCurrentUnit("TeraAmpere", Seq("TA"), r"1e12")
  case object PetaAmpere extends IntrinsicCurrentUnit("PetaAmpere", Seq("PA"), r"1e15")
  case object ExaAmpere extends IntrinsicCurrentUnit("ExaAmpere", Seq("EA"), r"1e18")
  case object ZettaAmpere extends IntrinsicCurrentUnit("ZettaAmpere", Seq("ZA"), r"1e21")
  case object YottaAmpere extends IntrinsicCurrentUnit("YottaAmpere", Seq("YA"), r"1e24")

  override lazy val values = Seq(YoctoAmpere, ZeptoAmpere, AttoAmpere, FemtoAmpere, PicoAmpere, NanoAmpere, MicroAmpere, MilliAmpere, CentiAmpere, DeciAmpere, Ampere, DecaAmpere, HectoAmpere, KiloAmpere, MegaAmpere, GigaAmpere, TeraAmpere, PetaAmpere, ExaAmpere, ZettaAmpere, YottaAmpere)

  // ChargeUnit / TimeUnit -> Current
  private[CurrentUnit]
  class QuotientChargePerTimeUnit(val numeratorUnit: ChargeUnit, val denominatorUnit: TimeUnit)
      extends CurrentUnit with QuotientUnit[CurrentUnit, ChargeUnit, TimeUnit]{

    override lazy val unitInAmpere: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: ChargeUnit, dUnit: TimeUnit): CurrentUnit =
    new QuotientChargePerTimeUnit(nUnit, dUnit)
}

trait MultiplicativeByCurrentUnit[R]{
  def *(unit: CurrentUnit): R
}

trait DivisibleByCurrentUnit[R]{
  def /(unit: CurrentUnit): R
}

trait CurrentPostfixOps[A]{
  import CurrentUnit._

  protected def currentPostfixOps(unit: CurrentUnit): A

  def yA : A = currentPostfixOps(YoctoAmpere)
  def zA : A = currentPostfixOps(ZeptoAmpere)
  def aA : A = currentPostfixOps(AttoAmpere)
  def fA : A = currentPostfixOps(FemtoAmpere)
  def pA : A = currentPostfixOps(PicoAmpere)
  def nA : A = currentPostfixOps(NanoAmpere)
  def microAmpere : A = currentPostfixOps(MicroAmpere)
  def microA : A = currentPostfixOps(MicroAmpere)
  def μA : A = currentPostfixOps(MicroAmpere)
  def mA : A = currentPostfixOps(MilliAmpere)
  def cA : A = currentPostfixOps(CentiAmpere)
  def dA : A = currentPostfixOps(DeciAmpere)
  def A : A = currentPostfixOps(Ampere)
  def daA : A = currentPostfixOps(DecaAmpere)
  def hA : A = currentPostfixOps(HectoAmpere)
  def kA : A = currentPostfixOps(KiloAmpere)
  def MA : A = currentPostfixOps(MegaAmpere)
  def GA : A = currentPostfixOps(GigaAmpere)
  def TA : A = currentPostfixOps(TeraAmpere)
  def PA : A = currentPostfixOps(PetaAmpere)
  def EA : A = currentPostfixOps(ExaAmpere)
  def ZA : A = currentPostfixOps(ZettaAmpere)
  def YA : A = currentPostfixOps(YottaAmpere)
}

trait CurrentDot[A]{
  import CurrentUnit._

  protected def currentDot(unit: CurrentUnit): A

  def yA(dot: Dot): A = currentDot(YoctoAmpere)
  def zA(dot: Dot): A = currentDot(ZeptoAmpere)
  def aA(dot: Dot): A = currentDot(AttoAmpere)
  def fA(dot: Dot): A = currentDot(FemtoAmpere)
  def pA(dot: Dot): A = currentDot(PicoAmpere)
  def nA(dot: Dot): A = currentDot(NanoAmpere)
  def microAmpere(dot: Dot): A = currentDot(MicroAmpere)
  def microA(dot: Dot): A = currentDot(MicroAmpere)
  def μA(dot: Dot): A = currentDot(MicroAmpere)
  def mA(dot: Dot): A = currentDot(MilliAmpere)
  def cA(dot: Dot): A = currentDot(CentiAmpere)
  def dA(dot: Dot): A = currentDot(DeciAmpere)
  def A(dot: Dot): A = currentDot(Ampere)
  def daA(dot: Dot): A = currentDot(DecaAmpere)
  def hA(dot: Dot): A = currentDot(HectoAmpere)
  def kA(dot: Dot): A = currentDot(KiloAmpere)
  def MA(dot: Dot): A = currentDot(MegaAmpere)
  def GA(dot: Dot): A = currentDot(GigaAmpere)
  def TA(dot: Dot): A = currentDot(TeraAmpere)
  def PA(dot: Dot): A = currentDot(PetaAmpere)
  def EA(dot: Dot): A = currentDot(ExaAmpere)
  def ZA(dot: Dot): A = currentDot(ZettaAmpere)
  def YA(dot: Dot): A = currentDot(YottaAmpere)
}

trait CurrentPer[A]{
  import CurrentUnit._

  protected def currentPer(unit: CurrentUnit): A

  def yA(per: Per): A = currentPer(YoctoAmpere)
  def zA(per: Per): A = currentPer(ZeptoAmpere)
  def aA(per: Per): A = currentPer(AttoAmpere)
  def fA(per: Per): A = currentPer(FemtoAmpere)
  def pA(per: Per): A = currentPer(PicoAmpere)
  def nA(per: Per): A = currentPer(NanoAmpere)
  def microAmpere(per: Per): A = currentPer(MicroAmpere)
  def microA(per: Per): A = currentPer(MicroAmpere)
  def μA(per: Per): A = currentPer(MicroAmpere)
  def mA(per: Per): A = currentPer(MilliAmpere)
  def cA(per: Per): A = currentPer(CentiAmpere)
  def dA(per: Per): A = currentPer(DeciAmpere)
  def A(per: Per): A = currentPer(Ampere)
  def daA(per: Per): A = currentPer(DecaAmpere)
  def hA(per: Per): A = currentPer(HectoAmpere)
  def kA(per: Per): A = currentPer(KiloAmpere)
  def MA(per: Per): A = currentPer(MegaAmpere)
  def GA(per: Per): A = currentPer(GigaAmpere)
  def TA(per: Per): A = currentPer(TeraAmpere)
  def PA(per: Per): A = currentPer(PetaAmpere)
  def EA(per: Per): A = currentPer(ExaAmpere)
  def ZA(per: Per): A = currentPer(ZettaAmpere)
  def YA(per: Per): A = currentPer(YottaAmpere)
}

trait PredefinedCurrentUnit extends CurrentPostfixOps[CurrentUnit]{
  override protected def currentPostfixOps(unit: CurrentUnit) = unit
  
}

object PredefinedCurrentUnit extends PredefinedCurrentUnit
