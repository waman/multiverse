package org.waman.multiverse.radiation

import org.waman.multiverse._
import org.waman.multiverse.energy.{EnergyPer, EnergyPostfixOps, EnergyUnit}
import org.waman.multiverse.mass.{MassPostfixOps, MassUnit}
import org.waman.multiverse.radiation.AbsorbedDoseUnit.Gray
import spire.implicits._
import spire.math.{Fractional, Real}

trait AbsorbedDosePostfixOps[A]{

  import AbsorbedDoseUnit._

  protected def absorbedDosePostfixOps(absorbedDoseUnit: AbsorbedDoseUnit): A
  
  def yGy: A = absorbedDosePostfixOps(YoctoGray)
  def zGy: A = absorbedDosePostfixOps(ZeptoGray)
  def aGy: A = absorbedDosePostfixOps(AttoGray)
  def fGy: A = absorbedDosePostfixOps(FemtoGray)
  def pGy: A = absorbedDosePostfixOps(PicoGray)
  def nGy: A = absorbedDosePostfixOps(NanoGray)
  def microGray: A = absorbedDosePostfixOps(MicroGray)
  def microGy  : A = microGray
  def μGy: A = microGray
  def mGy: A = absorbedDosePostfixOps(MilliGray)
  def cGy: A = absorbedDosePostfixOps(CentiGray)
  def dGy: A = absorbedDosePostfixOps(DeciGray)
  def Gy: A = absorbedDosePostfixOps(Gray)
  def daGy: A = absorbedDosePostfixOps(DecaGray)
  def hGy: A = absorbedDosePostfixOps(HectoGray)
  def kGy: A = absorbedDosePostfixOps(KiloGray)
  def MGy: A = absorbedDosePostfixOps(MegaGray)
  def GGy: A = absorbedDosePostfixOps(GigaGray)
  def TGy: A = absorbedDosePostfixOps(TeraGray)
  def PGy: A = absorbedDosePostfixOps(PetaGray)
  def EGy: A = absorbedDosePostfixOps(ExaGray)
  def ZGy: A = absorbedDosePostfixOps(ZettaGray)
  def YGy: A = absorbedDosePostfixOps(YottaGray)
}

class AbsorbedDose[A: Fractional](val value: A, val unit: AbsorbedDoseUnit)
  extends Quantity[A, AbsorbedDoseUnit]
    with AbsorbedDosePostfixOps[A]
    with EnergyPostfixOps[DivisibleByMassUnit[A]]
    with EnergyPer[MassPostfixOps[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: AbsorbedDoseUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInGray) / real(evalUnit.unitInGray)

  override protected def absorbedDosePostfixOps(absorbedDoseUnit: AbsorbedDoseUnit) = apply(absorbedDoseUnit)

  override protected def energyPostfixOps(energyUnit: EnergyUnit) = new DivisibleByMassUnit[A]{
    override def /(massUnit: MassUnit) = apply(energyUnit/ massUnit)
  }

  override protected def energyPer(energyUnit: EnergyUnit) = new MassPostfixOps[A]{
    override protected def massPostfixOps(massUnit: MassUnit) = apply(energyUnit / massUnit)
  }
}

sealed trait AbsorbedDoseUnit extends PhysicalUnit[AbsorbedDoseUnit]{

  def unitInGray: Real

  override def baseUnit = Gray
  override def valueInBaseUnit = unitInGray
}

object AbsorbedDoseUnit extends ConstantsDefined[AbsorbedDoseUnit]{

  import scala.language.implicitConversions
  implicit def convertToSeq(s: String): Seq[String] = Seq(s)

  // intrinsic
  private[AbsorbedDoseUnit]
  class IntrinsicAbsorbedDoseUnit(val symbols: Seq[String], val unitInGray: Real)
    extends AbsorbedDoseUnit

  case object YoctoGray extends IntrinsicAbsorbedDoseUnit("yGy", r"1e-24")
  case object ZeptoGray extends IntrinsicAbsorbedDoseUnit("zGy", r"1e-21")
  case object AttoGray  extends IntrinsicAbsorbedDoseUnit("aGy", r"1e-18")
  case object FemtoGray extends IntrinsicAbsorbedDoseUnit("fGy", r"1e-15")
  case object PicoGray  extends IntrinsicAbsorbedDoseUnit("pGy", r"1e-12")
  case object NanoGray  extends IntrinsicAbsorbedDoseUnit("nGy", r"1e-9")
  case object MicroGray extends IntrinsicAbsorbedDoseUnit(Seq("μGy", "microGray", "microGy"), r"1e-6")
  case object MilliGray extends IntrinsicAbsorbedDoseUnit("mGy", r"1e-3")
  case object CentiGray extends IntrinsicAbsorbedDoseUnit("cGy", r"1e-2")
  case object DeciGray  extends IntrinsicAbsorbedDoseUnit("dGy", r"1e-1")
  case object Gray      extends IntrinsicAbsorbedDoseUnit("Gy" , 1)
  case object DecaGray  extends IntrinsicAbsorbedDoseUnit("daGy", r"1e1")
  case object HectoGray extends IntrinsicAbsorbedDoseUnit("hGy", r"1e2")
  case object KiloGray  extends IntrinsicAbsorbedDoseUnit("kGy", r"1e3")
  case object MegaGray  extends IntrinsicAbsorbedDoseUnit("MGy", r"1e6")
  case object GigaGray  extends IntrinsicAbsorbedDoseUnit("GGy", r"1e9")
  case object TeraGray  extends IntrinsicAbsorbedDoseUnit("TGy", r"1e12")
  case object PetaGray  extends IntrinsicAbsorbedDoseUnit("PGy", r"1e15")
  case object ExaGray   extends IntrinsicAbsorbedDoseUnit("EGy", r"1e18")
  case object ZettaGray extends IntrinsicAbsorbedDoseUnit("ZGy", r"1e21")
  case object YottaGray extends IntrinsicAbsorbedDoseUnit("YGy", r"1e24")

  override lazy val values = Seq(
    YoctoGray,
    ZeptoGray,
    AttoGray,
    FemtoGray,
    PicoGray,
    NanoGray,
    MicroGray,
    MilliGray,
    CentiGray,
    DeciGray,
    Gray,
    DecaGray,
    HectoGray,
    KiloGray,
    MegaGray,
    GigaGray,
    TeraGray,
    PetaGray,
    ExaGray,
    ZettaGray,
    YottaGray
  )

  // Quotient (Energy / Mass)
  private[AbsorbedDoseUnit]
  class QuotientAbsorbedDoseUnit(val numeratorUnit: EnergyUnit, val denominatorUnit: MassUnit)
    extends AbsorbedDoseUnit with QuotientUnit[AbsorbedDoseUnit, EnergyUnit, MassUnit]{

    override lazy val unitInGray: Real =
      numeratorUnit.unitInJoule / denominatorUnit.unitInKiloGram
  }

  def apply(eUnit: EnergyUnit, mUnit: MassUnit): AbsorbedDoseUnit =
    new QuotientAbsorbedDoseUnit(eUnit, mUnit)
}

trait PredefinedAbsorbedDoseUnit extends AbsorbedDosePostfixOps[AbsorbedDoseUnit]{

  override protected def absorbedDosePostfixOps(absorbedDoseUnit: AbsorbedDoseUnit) = absorbedDoseUnit
}

object PredefinedAbsorbedDoseUnit extends PredefinedAbsorbedDoseUnit

trait AbsorbedDoseFactory[A]
    extends AbsorbedDosePostfixOps[AbsorbedDose[A]]{

  def apply(unit: AbsorbedDoseUnit): AbsorbedDose[A]

  override protected def absorbedDosePostfixOps(absorbedDoseUnit: AbsorbedDoseUnit) =
    apply(absorbedDoseUnit)
}