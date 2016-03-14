package org.waman.multiverse.angle

import org.waman.multiverse.MultiverseUtil.twoPi
import org.waman.multiverse._
import spire.implicits._
import spire.math.Real

sealed trait SolidAngleUnit extends PhysicalUnit[SolidAngleUnit]{

  def unitInSteradian: Real

  override def baseUnit = org.waman.multiverse.angle.SolidAngleUnit.Steradian
  override def valueInBaseUnit = unitInSteradian
}

object SolidAngleUnit extends ConstantsDefined[SolidAngleUnit]{

  // intrinsic
  private[SolidAngleUnit]
  class IntrinsicSolidAngleUnit(name: String, val symbols: Seq[String], val unitInSteradian: Real)
      extends SolidAngleUnit{

    def this(name: String, symbols: Seq[String], unit: SolidAngleUnit) =
      this(name, symbols, unit.unitInSteradian)

    def this(name: String, symbols: Seq[String], factor: Real, unit: SolidAngleUnit) =
      this(name, symbols, factor * unit.unitInSteradian)
  }

  case object YoctoSteradian extends IntrinsicSolidAngleUnit("YoctoSteradian", Seq("ysr"), r"1e-24")
    
  case object ZeptoSteradian extends IntrinsicSolidAngleUnit("ZeptoSteradian", Seq("zsr"), r"1e-21")
    
  case object AttoSteradian extends IntrinsicSolidAngleUnit("AttoSteradian", Seq("asr"), r"1e-18")
    
  case object FemtoSteradian extends IntrinsicSolidAngleUnit("FemtoSteradian", Seq("fsr"), r"1e-15")
    
  case object PicoSteradian extends IntrinsicSolidAngleUnit("PicoSteradian", Seq("psr"), r"1e-12")
    
  case object NanoSteradian extends IntrinsicSolidAngleUnit("NanoSteradian", Seq("nsr"), r"1e-9")
    
  case object MicroSteradian extends IntrinsicSolidAngleUnit("MicroSteradian", Seq("microSteradian", "microSr", "μsr"), r"1e-6")
    
  case object MilliSteradian extends IntrinsicSolidAngleUnit("MilliSteradian", Seq("msr"), r"1e-3")
    
  case object CentiSteradian extends IntrinsicSolidAngleUnit("CentiSteradian", Seq("csr"), r"1e-2")
    
  case object DeciSteradian extends IntrinsicSolidAngleUnit("DeciSteradian", Seq("dsr"), r"1e-1")
    
  case object Steradian extends IntrinsicSolidAngleUnit("Steradian", Seq("sr"), r"1")
    
  case object DecaSteradian extends IntrinsicSolidAngleUnit("DecaSteradian", Seq("dasr"), r"1e-1")
    
  case object SquareDegree extends IntrinsicSolidAngleUnit("SquareDegree", Seq("deg2"), (twoPi / r"360")**2)
    

  override lazy val values = Seq(YoctoSteradian, ZeptoSteradian, AttoSteradian, FemtoSteradian, PicoSteradian, NanoSteradian, MicroSteradian, MilliSteradian, CentiSteradian, DeciSteradian, Steradian, DecaSteradian, SquareDegree)
}

trait SolidAnglePostfixOps[A]{
  import SolidAngleUnit._

  protected def solidAnglePostfixOps(unit: SolidAngleUnit): A

  def ysr : A = solidAnglePostfixOps(YoctoSteradian)
  def zsr : A = solidAnglePostfixOps(ZeptoSteradian)
  def asr : A = solidAnglePostfixOps(AttoSteradian)
  def fsr : A = solidAnglePostfixOps(FemtoSteradian)
  def psr : A = solidAnglePostfixOps(PicoSteradian)
  def nsr : A = solidAnglePostfixOps(NanoSteradian)
  def microSteradian : A = solidAnglePostfixOps(MicroSteradian)
  def microSr : A = solidAnglePostfixOps(MicroSteradian)
  def μsr : A = solidAnglePostfixOps(MicroSteradian)
  def msr : A = solidAnglePostfixOps(MilliSteradian)
  def csr : A = solidAnglePostfixOps(CentiSteradian)
  def dsr : A = solidAnglePostfixOps(DeciSteradian)
  def sr : A = solidAnglePostfixOps(Steradian)
  def dasr : A = solidAnglePostfixOps(DecaSteradian)
  def deg2 : A = solidAnglePostfixOps(SquareDegree)
}

trait SolidAngleDot[A]{
  import SolidAngleUnit._

  protected def solidAngleDot(unit: SolidAngleUnit): A

  def ysr(dot: Dot): A = solidAngleDot(YoctoSteradian)
  def zsr(dot: Dot): A = solidAngleDot(ZeptoSteradian)
  def asr(dot: Dot): A = solidAngleDot(AttoSteradian)
  def fsr(dot: Dot): A = solidAngleDot(FemtoSteradian)
  def psr(dot: Dot): A = solidAngleDot(PicoSteradian)
  def nsr(dot: Dot): A = solidAngleDot(NanoSteradian)
  def microSteradian(dot: Dot): A = solidAngleDot(MicroSteradian)
  def microSr(dot: Dot): A = solidAngleDot(MicroSteradian)
  def μsr(dot: Dot): A = solidAngleDot(MicroSteradian)
  def msr(dot: Dot): A = solidAngleDot(MilliSteradian)
  def csr(dot: Dot): A = solidAngleDot(CentiSteradian)
  def dsr(dot: Dot): A = solidAngleDot(DeciSteradian)
  def sr(dot: Dot): A = solidAngleDot(Steradian)
  def dasr(dot: Dot): A = solidAngleDot(DecaSteradian)
  def deg2(dot: Dot): A = solidAngleDot(SquareDegree)
}

trait SolidAnglePer[A]{
  import SolidAngleUnit._

  protected def solidAnglePer(unit: SolidAngleUnit): A

  def ysr(per: Per): A = solidAnglePer(YoctoSteradian)
  def zsr(per: Per): A = solidAnglePer(ZeptoSteradian)
  def asr(per: Per): A = solidAnglePer(AttoSteradian)
  def fsr(per: Per): A = solidAnglePer(FemtoSteradian)
  def psr(per: Per): A = solidAnglePer(PicoSteradian)
  def nsr(per: Per): A = solidAnglePer(NanoSteradian)
  def microSteradian(per: Per): A = solidAnglePer(MicroSteradian)
  def microSr(per: Per): A = solidAnglePer(MicroSteradian)
  def μsr(per: Per): A = solidAnglePer(MicroSteradian)
  def msr(per: Per): A = solidAnglePer(MilliSteradian)
  def csr(per: Per): A = solidAnglePer(CentiSteradian)
  def dsr(per: Per): A = solidAnglePer(DeciSteradian)
  def sr(per: Per): A = solidAnglePer(Steradian)
  def dasr(per: Per): A = solidAnglePer(DecaSteradian)
  def deg2(per: Per): A = solidAnglePer(SquareDegree)
}

trait PredefinedSolidAngleUnit extends SolidAnglePostfixOps[SolidAngleUnit]{
  override protected def solidAnglePostfixOps(unit: SolidAngleUnit) = unit
  
}

object PredefinedSolidAngleUnit extends PredefinedSolidAngleUnit
