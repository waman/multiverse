package org.waman.multiverse.angle

import org.waman.multiverse.MultiverseUtil.twoPi
import org.waman.multiverse.{PhysicalUnit, Quantity, UnitConverter}
import spire.implicits._
import spire.math.{Fractional, Real}

trait SolidAnglePostfixOps[A]{
  import SolidAngleUnit._

  protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit): A

  def dasr: A = solidAnglePostfixOps(DecaSteradian)
  def sr  : A = solidAnglePostfixOps(Steradian)
  def dsr : A = solidAnglePostfixOps(DeciSteradian)
  def csr : A = solidAnglePostfixOps(CentiSteradian)
  def msr : A = solidAnglePostfixOps(MilliSteradian)
  def μsr : A = solidAnglePostfixOps(MicroSteradian)
  def nsr : A = solidAnglePostfixOps(NanoSteradian)
  def psr : A = solidAnglePostfixOps(PicoSteradian)
  def fsr : A = solidAnglePostfixOps(FemtoSteradian)
  def asr : A = solidAnglePostfixOps(AttoSteradian)
  def zsr : A = solidAnglePostfixOps(ZeptoSteradian)
  def ysr : A = solidAnglePostfixOps(YoctoSteradian)
  
  def deg2: A = solidAnglePostfixOps(SquareDegree)
}

class SolidAngle[A: Fractional](val value: A, val unit: SolidAngleUnit)
  extends Quantity[A, SolidAngleUnit]
    with SolidAnglePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: SolidAngleUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInSteradian) / real(evalUnit.unitInSteradian)

  override protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit) = apply(solidAngleUnit)
}

sealed abstract class SolidAngleUnit(val symbol: String, val unitInSteradian: Real)
  extends PhysicalUnit[SolidAngleUnit]{

  override val baseUnit = SolidAngleUnit.Steradian
  override val inBaseUnitAccessor = () => unitInSteradian
}

object SolidAngleUnit{

  case object DecaSteradian  extends SolidAngleUnit("dasr", r"1e1")
  case object Steradian      extends SolidAngleUnit("sr", 1)
  case object DeciSteradian  extends SolidAngleUnit("dsr", r"1e-1")
  case object CentiSteradian extends SolidAngleUnit("csr", r"1e-2")
  case object MilliSteradian extends SolidAngleUnit("msr", r"1e-3")
  case object MicroSteradian extends SolidAngleUnit("μsr", r"1e-6")
  case object NanoSteradian  extends SolidAngleUnit("nsr", r"1e-9")
  case object PicoSteradian  extends SolidAngleUnit("psr", r"1e-12")
  case object FemtoSteradian extends SolidAngleUnit("fsr", r"1e-15")
  case object AttoSteradian  extends SolidAngleUnit("asr", r"1e-18")
  case object ZeptoSteradian extends SolidAngleUnit("zsr", r"1e-21")
  case object YoctoSteradian extends SolidAngleUnit("ysr", r"1e-24")
  
  case object SquareDegree extends SolidAngleUnit("deg2", (twoPi / r"360")**2)
}

trait PredefinedSolidAngleUnit extends SolidAnglePostfixOps[SolidAngleUnit]{
  override protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit) = solidAngleUnit
}

object PredefinedSolidAngleUnit extends PredefinedSolidAngleUnit

trait SolidAngleUnitInterpreter[A]
  extends SolidAnglePostfixOps[SolidAngle[A]]{

  def apply(unit: SolidAngleUnit): SolidAngle[A]

  override protected def solidAnglePostfixOps(solidAngleUnit: SolidAngleUnit) = apply(solidAngleUnit)
}