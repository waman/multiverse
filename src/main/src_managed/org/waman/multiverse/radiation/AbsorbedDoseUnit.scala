package org.waman.multiverse.radiation

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.mass._
import org.waman.multiverse.energy._

sealed trait AbsorbedDoseUnit extends PhysicalUnit[AbsorbedDoseUnit]{

  def unitInGray: Real

  override def baseUnit = org.waman.multiverse.radiation.AbsorbedDoseUnit.Gray
  override def valueInBaseUnit = unitInGray
}

object AbsorbedDoseUnit extends ConstantsDefined[AbsorbedDoseUnit]{

  // intrinsic
  private[AbsorbedDoseUnit]
  class IntrinsicAbsorbedDoseUnit(name: String, val symbols: Seq[String], val unitInGray: Real)
      extends AbsorbedDoseUnit{

    def this(name: String, symbols: Seq[String], unit: AbsorbedDoseUnit) =
      this(name, symbols, unit.unitInGray)

    def this(name: String, symbols: Seq[String], factor: Real, unit: AbsorbedDoseUnit) =
      this(name, symbols, factor * unit.unitInGray)
  }


  case object YoctoGray extends IntrinsicAbsorbedDoseUnit("YoctoGray", Seq("yGy"), r"1e-24")
  case object ZeptoGray extends IntrinsicAbsorbedDoseUnit("ZeptoGray", Seq("zGy"), r"1e-21")
  case object AttoGray extends IntrinsicAbsorbedDoseUnit("AttoGray", Seq("aGy"), r"1e-18")
  case object FemtoGray extends IntrinsicAbsorbedDoseUnit("FemtoGray", Seq("fGy"), r"1e-15")
  case object PicoGray extends IntrinsicAbsorbedDoseUnit("PicoGray", Seq("pGy"), r"1e-12")
  case object NanoGray extends IntrinsicAbsorbedDoseUnit("NanoGray", Seq("nGy"), r"1e-9")
  case object MicroGray extends IntrinsicAbsorbedDoseUnit("MicroGray", Seq("microGray", "microGy", "μGy"), r"1e-6")
  case object MilliGray extends IntrinsicAbsorbedDoseUnit("MilliGray", Seq("mGy"), r"1e-3")
  case object CentiGray extends IntrinsicAbsorbedDoseUnit("CentiGray", Seq("cGy"), r"1e-2")
  case object DeciGray extends IntrinsicAbsorbedDoseUnit("DeciGray", Seq("dGy"), r"1e-1")
  case object Gray extends IntrinsicAbsorbedDoseUnit("Gray", Seq("Gy"), r"1")
  case object DecaGray extends IntrinsicAbsorbedDoseUnit("DecaGray", Seq("daGy"), r"1e1")
  case object HectoGray extends IntrinsicAbsorbedDoseUnit("HectoGray", Seq("hGy"), r"1e2")
  case object KiloGray extends IntrinsicAbsorbedDoseUnit("KiloGray", Seq("kGy"), r"1e3")
  case object MegaGray extends IntrinsicAbsorbedDoseUnit("MegaGray", Seq("MGy"), r"1e6")
  case object GigaGray extends IntrinsicAbsorbedDoseUnit("GigaGray", Seq("GGy"), r"1e9")
  case object TeraGray extends IntrinsicAbsorbedDoseUnit("TeraGray", Seq("TGy"), r"1e12")
  case object PetaGray extends IntrinsicAbsorbedDoseUnit("PetaGray", Seq("PGy"), r"1e15")
  case object ExaGray extends IntrinsicAbsorbedDoseUnit("ExaGray", Seq("EGy"), r"1e18")
  case object ZettaGray extends IntrinsicAbsorbedDoseUnit("ZettaGray", Seq("ZGy"), r"1e21")
  case object YottaGray extends IntrinsicAbsorbedDoseUnit("YottaGray", Seq("YGy"), r"1e24")

  override lazy val values = Seq(YoctoGray, ZeptoGray, AttoGray, FemtoGray, PicoGray, NanoGray, MicroGray, MilliGray, CentiGray, DeciGray, Gray, DecaGray, HectoGray, KiloGray, MegaGray, GigaGray, TeraGray, PetaGray, ExaGray, ZettaGray, YottaGray)

  // EnergyUnit / MassUnit -> AbsorbedDose
  private[AbsorbedDoseUnit]
  class QuotientEnergyPerMassUnit(val numeratorUnit: EnergyUnit, val denominatorUnit: MassUnit)
      extends AbsorbedDoseUnit with QuotientUnit[AbsorbedDoseUnit, EnergyUnit, MassUnit]{

    override lazy val unitInGray: Real =
      numeratorUnit.valueInBaseUnit / denominatorUnit.valueInBaseUnit
  }

  def apply(nUnit: EnergyUnit, dUnit: MassUnit): AbsorbedDoseUnit =
    new QuotientEnergyPerMassUnit(nUnit, dUnit)
}

trait MultiplicativeByAbsorbedDoseUnit[R]{
  def *(unit: AbsorbedDoseUnit): R
}

trait DivisibleByAbsorbedDoseUnit[R]{
  def /(unit: AbsorbedDoseUnit): R
}

trait AbsorbedDosePostfixOps[A]{
  import AbsorbedDoseUnit._

  protected def absorbedDosePostfixOps(unit: AbsorbedDoseUnit): A


  def yGy : A = absorbedDosePostfixOps(YoctoGray)
  def zGy : A = absorbedDosePostfixOps(ZeptoGray)
  def aGy : A = absorbedDosePostfixOps(AttoGray)
  def fGy : A = absorbedDosePostfixOps(FemtoGray)
  def pGy : A = absorbedDosePostfixOps(PicoGray)
  def nGy : A = absorbedDosePostfixOps(NanoGray)
  def microGray : A = absorbedDosePostfixOps(MicroGray)
  def microGy : A = absorbedDosePostfixOps(MicroGray)
  def μGy : A = absorbedDosePostfixOps(MicroGray)
  def mGy : A = absorbedDosePostfixOps(MilliGray)
  def cGy : A = absorbedDosePostfixOps(CentiGray)
  def dGy : A = absorbedDosePostfixOps(DeciGray)
  def Gy : A = absorbedDosePostfixOps(Gray)
  def daGy : A = absorbedDosePostfixOps(DecaGray)
  def hGy : A = absorbedDosePostfixOps(HectoGray)
  def kGy : A = absorbedDosePostfixOps(KiloGray)
  def MGy : A = absorbedDosePostfixOps(MegaGray)
  def GGy : A = absorbedDosePostfixOps(GigaGray)
  def TGy : A = absorbedDosePostfixOps(TeraGray)
  def PGy : A = absorbedDosePostfixOps(PetaGray)
  def EGy : A = absorbedDosePostfixOps(ExaGray)
  def ZGy : A = absorbedDosePostfixOps(ZettaGray)
  def YGy : A = absorbedDosePostfixOps(YottaGray)
}

trait AbsorbedDoseDot[A]{
  import AbsorbedDoseUnit._

  protected def absorbedDoseDot(unit: AbsorbedDoseUnit): A

  def yGy(dot: Dot): A = absorbedDoseDot(YoctoGray)
  def zGy(dot: Dot): A = absorbedDoseDot(ZeptoGray)
  def aGy(dot: Dot): A = absorbedDoseDot(AttoGray)
  def fGy(dot: Dot): A = absorbedDoseDot(FemtoGray)
  def pGy(dot: Dot): A = absorbedDoseDot(PicoGray)
  def nGy(dot: Dot): A = absorbedDoseDot(NanoGray)
  def microGray(dot: Dot): A = absorbedDoseDot(MicroGray)
  def microGy(dot: Dot): A = absorbedDoseDot(MicroGray)
  def μGy(dot: Dot): A = absorbedDoseDot(MicroGray)
  def mGy(dot: Dot): A = absorbedDoseDot(MilliGray)
  def cGy(dot: Dot): A = absorbedDoseDot(CentiGray)
  def dGy(dot: Dot): A = absorbedDoseDot(DeciGray)
  def Gy(dot: Dot): A = absorbedDoseDot(Gray)
  def daGy(dot: Dot): A = absorbedDoseDot(DecaGray)
  def hGy(dot: Dot): A = absorbedDoseDot(HectoGray)
  def kGy(dot: Dot): A = absorbedDoseDot(KiloGray)
  def MGy(dot: Dot): A = absorbedDoseDot(MegaGray)
  def GGy(dot: Dot): A = absorbedDoseDot(GigaGray)
  def TGy(dot: Dot): A = absorbedDoseDot(TeraGray)
  def PGy(dot: Dot): A = absorbedDoseDot(PetaGray)
  def EGy(dot: Dot): A = absorbedDoseDot(ExaGray)
  def ZGy(dot: Dot): A = absorbedDoseDot(ZettaGray)
  def YGy(dot: Dot): A = absorbedDoseDot(YottaGray)
}

trait AbsorbedDosePer[A]{
  import AbsorbedDoseUnit._

  protected def absorbedDosePer(unit: AbsorbedDoseUnit): A

  def yGy(per: Per): A = absorbedDosePer(YoctoGray)
  def zGy(per: Per): A = absorbedDosePer(ZeptoGray)
  def aGy(per: Per): A = absorbedDosePer(AttoGray)
  def fGy(per: Per): A = absorbedDosePer(FemtoGray)
  def pGy(per: Per): A = absorbedDosePer(PicoGray)
  def nGy(per: Per): A = absorbedDosePer(NanoGray)
  def microGray(per: Per): A = absorbedDosePer(MicroGray)
  def microGy(per: Per): A = absorbedDosePer(MicroGray)
  def μGy(per: Per): A = absorbedDosePer(MicroGray)
  def mGy(per: Per): A = absorbedDosePer(MilliGray)
  def cGy(per: Per): A = absorbedDosePer(CentiGray)
  def dGy(per: Per): A = absorbedDosePer(DeciGray)
  def Gy(per: Per): A = absorbedDosePer(Gray)
  def daGy(per: Per): A = absorbedDosePer(DecaGray)
  def hGy(per: Per): A = absorbedDosePer(HectoGray)
  def kGy(per: Per): A = absorbedDosePer(KiloGray)
  def MGy(per: Per): A = absorbedDosePer(MegaGray)
  def GGy(per: Per): A = absorbedDosePer(GigaGray)
  def TGy(per: Per): A = absorbedDosePer(TeraGray)
  def PGy(per: Per): A = absorbedDosePer(PetaGray)
  def EGy(per: Per): A = absorbedDosePer(ExaGray)
  def ZGy(per: Per): A = absorbedDosePer(ZettaGray)
  def YGy(per: Per): A = absorbedDosePer(YottaGray)
}

trait PredefinedAbsorbedDoseUnit extends AbsorbedDosePostfixOps[AbsorbedDoseUnit]{
  override protected def absorbedDosePostfixOps(unit: AbsorbedDoseUnit) = unit
  
}

object PredefinedAbsorbedDoseUnit extends PredefinedAbsorbedDoseUnit
