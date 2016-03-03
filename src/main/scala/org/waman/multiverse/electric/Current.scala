package org.waman.multiverse.electric

import org.waman.multiverse._
import spire.implicits._
import spire.math.{Fractional, Real}

trait CurrentPostfixOps[A]{

  import CurrentUnit._

  protected def currentPostfixOps(currentUnit: CurrentUnit): A
  
  def yA: A = currentPostfixOps(YoctoAmpere)
  def zA: A = currentPostfixOps(ZeptoAmpere)
  def aA: A = currentPostfixOps(AttoAmpere)
  def fA: A = currentPostfixOps(FemtoAmpere)
  def pA: A = currentPostfixOps(PicoAmpere)
  def nA: A = currentPostfixOps(NanoAmpere)
  def microAmpere: A = currentPostfixOps(MicroAmpere)
  def microA     : A = microAmpere
  def μA: A = microAmpere
  def mA: A = currentPostfixOps(MilliAmpere)
  def cA: A = currentPostfixOps(CentiAmpere)
  def dA: A = currentPostfixOps(DeciAmpere)
  def A : A = currentPostfixOps(Ampere)
  def daA: A = currentPostfixOps(DecaAmpere)
  def hA: A = currentPostfixOps(HectoAmpere)
  def kA: A = currentPostfixOps(KiloAmpere)
  def MA: A = currentPostfixOps(MegaAmpere)
  def GA: A = currentPostfixOps(GigaAmpere)
  def TA: A = currentPostfixOps(TeraAmpere)
  def PA: A = currentPostfixOps(PetaAmpere)
  def EA: A = currentPostfixOps(ExaAmpere)
  def ZA: A = currentPostfixOps(ZettaAmpere)
  def YA: A = currentPostfixOps(YottaAmpere)
}

class Current[A: Fractional](val value: A, val unit: CurrentUnit)
  extends Quantity[A, CurrentUnit]
    with CurrentPostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: CurrentUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInAmpere) / real(evalUnit.unitInAmpere)


  override protected def currentPostfixOps(currentUnit: CurrentUnit) = apply(currentUnit)
}

sealed abstract class CurrentUnit(val symbols: Seq[String], val unitInAmpere: Real)
  extends PhysicalUnit[CurrentUnit]{

  override def baseUnit = CurrentUnit.Ampere
  override def valueInBaseUnit = unitInAmpere
}

object CurrentUnit extends ConstantsDefined[CurrentUnit]{

  import scala.language.implicitConversions
  implicit def convertToSeq(s: String): Seq[String] = Seq(s)

  // intrinsic
  case object YoctoAmpere extends CurrentUnit("yA", r"1e-24")
  case object ZeptoAmpere extends CurrentUnit("zA", r"1e-21")
  case object AttoAmpere  extends CurrentUnit("aA", r"1e-18")
  case object FemtoAmpere extends CurrentUnit("fA", r"1e-15")
  case object PicoAmpere  extends CurrentUnit("pA", r"1e-12")
  case object NanoAmpere  extends CurrentUnit("nA", r"1e-9")
  case object MicroAmpere extends CurrentUnit(Seq("μA", "microAmpere", "microA"), r"1e-6")
  case object MilliAmpere extends CurrentUnit("mA", r"1e-3")
  case object CentiAmpere extends CurrentUnit("cA", r"1e-2")
  case object DeciAmpere  extends CurrentUnit("dA", r"1e-1")
  case object Ampere      extends CurrentUnit("A" , 1)
  case object DecaAmpere  extends CurrentUnit("daA", r"1e1")
  case object HectoAmpere extends CurrentUnit("hA", r"1e2")
  case object KiloAmpere  extends CurrentUnit("kA", r"1e3")
  case object MegaAmpere  extends CurrentUnit("MA", r"1e6")
  case object GigaAmpere  extends CurrentUnit("GA", r"1e9")
  case object TeraAmpere  extends CurrentUnit("TA", r"1e12")
  case object PetaAmpere  extends CurrentUnit("PA", r"1e15")
  case object ExaAmpere   extends CurrentUnit("EA", r"1e18")
  case object ZettaAmpere extends CurrentUnit("ZA", r"1e21")
  case object YottaAmpere extends CurrentUnit("YA", r"1e24")

  override lazy val values = Seq(
    YoctoAmpere,
    ZeptoAmpere,
    AttoAmpere,
    FemtoAmpere,
    PicoAmpere,
    NanoAmpere,
    MicroAmpere,
    MilliAmpere,
    CentiAmpere,
    DeciAmpere,
    Ampere,
    DecaAmpere,
    HectoAmpere,
    KiloAmpere,
    MegaAmpere,
    GigaAmpere,
    TeraAmpere,
    PetaAmpere,
    ExaAmpere,
    ZettaAmpere,
    YottaAmpere
  )
}

trait PredefinedCurrentUnit extends CurrentPostfixOps[CurrentUnit]{

  override protected def currentPostfixOps(currentUnit: CurrentUnit) = currentUnit
}

object PredefinedCurrentUnit extends PredefinedCurrentUnit

trait CurrentFactory[A]
    extends CurrentPostfixOps[Current[A]]{

  def apply(unit: CurrentUnit): Current[A]

  override protected def currentPostfixOps(currentUnit: CurrentUnit) =
    apply(currentUnit)
}