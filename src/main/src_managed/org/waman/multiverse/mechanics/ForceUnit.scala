package org.waman.multiverse.mechanics

import org.waman.multiverse._
import org.waman.multiverse.mechanics.AccelerationUnit.StandardGravity
import org.waman.multiverse.metric.LengthUnit
import spire.implicits._
import spire.math.Real

sealed trait ForceUnit extends PhysicalUnit[ForceUnit]
  with MultiplicativeByLengthUnit[TorqueUnit]{

  def unitInNewton: Real

  override def baseUnit = org.waman.multiverse.mechanics.ForceUnit.Newton
  override def valueInBaseUnit = unitInNewton

  override def *(unit: LengthUnit) = TorqueUnit(this, unit)
}

object ForceUnit extends ConstantsDefined[ForceUnit]{

  // intrinsic
  private[ForceUnit]
  class IntrinsicForceUnit(name: String, val symbols: Seq[String], val unitInNewton: Real)
      extends ForceUnit{

    def this(name: String, symbols: Seq[String], unit: ForceUnit) =
      this(name, symbols, unit.unitInNewton)

    def this(name: String, symbols: Seq[String], factor: Real, unit: ForceUnit) =
      this(name, symbols, factor * unit.unitInNewton)
  }

  case object YoctoNewton extends IntrinsicForceUnit("YoctoNewton", Seq("yN"), r"1e-24")
    
  case object ZeptoNewton extends IntrinsicForceUnit("ZeptoNewton", Seq("zN"), r"1e-21")
    
  case object AttoNewton extends IntrinsicForceUnit("AttoNewton", Seq("aN"), r"1e-18")
    
  case object FemtoNewton extends IntrinsicForceUnit("FemtoNewton", Seq("fN"), r"1e-15")
    
  case object PicoNewton extends IntrinsicForceUnit("PicoNewton", Seq("pN"), r"1e-12")
    
  case object NanoNewton extends IntrinsicForceUnit("NanoNewton", Seq("nN"), r"1e-9")
    
  case object MicroNewton extends IntrinsicForceUnit("MicroNewton", Seq("microNewton", "microN", "μN"), r"1e-6")
    
  case object MilliNewton extends IntrinsicForceUnit("MilliNewton", Seq("mN"), r"1e-3")
    
  case object CentiNewton extends IntrinsicForceUnit("CentiNewton", Seq("cN"), r"1e-2")
    
  case object DeciNewton extends IntrinsicForceUnit("DeciNewton", Seq("dN"), r"1e-1")
    
  case object Newton extends IntrinsicForceUnit("Newton", Seq("N"), r"1")
    
  case object DecaNewton extends IntrinsicForceUnit("DecaNewton", Seq("daN"), r"1e-1")
    
  case object HectoNewton extends IntrinsicForceUnit("HectoNewton", Seq("hN"), r"1e-2")
    
  case object KiloNewton extends IntrinsicForceUnit("KiloNewton", Seq("kN"), r"1e-3")
    
  case object MegaNewton extends IntrinsicForceUnit("MegaNewton", Seq("MN"), r"1e-6")
    
  case object GigaNewton extends IntrinsicForceUnit("GigaNewton", Seq("GN"), r"1e-9")
    
  case object TeraNewton extends IntrinsicForceUnit("TeraNewton", Seq("TN"), r"1e-12")
    
  case object PetaNewton extends IntrinsicForceUnit("PetaNewton", Seq("PN"), r"1e-15")
    
  case object ExaNewton extends IntrinsicForceUnit("ExaNewton", Seq("EN"), r"1e-18")
    
  case object ZettaNewton extends IntrinsicForceUnit("ZettaNewton", Seq("ZN"), r"1e-21")
    
  case object YottaNewton extends IntrinsicForceUnit("YottaNewton", Seq("YN"), r"1e-24")
    
  case object Dyne extends IntrinsicForceUnit("Dyne", Seq("dyn"), r"1e-5")
    
  case object KiloGramForce extends IntrinsicForceUnit("KiloGramForce", Seq("kgf", "kp"), StandardGravity.unitInMetrePerSecondSquared) with NotExact
    

  override lazy val values = Seq(YoctoNewton, ZeptoNewton, AttoNewton, FemtoNewton, PicoNewton, NanoNewton, MicroNewton, MilliNewton, CentiNewton, DeciNewton, Newton, DecaNewton, HectoNewton, KiloNewton, MegaNewton, GigaNewton, TeraNewton, PetaNewton, ExaNewton, ZettaNewton, YottaNewton, Dyne, KiloGramForce)
}

trait ForcePostfixOps[A]{
  import ForceUnit._

  protected def forcePostfixOps(unit: ForceUnit): A

  def yN : A = forcePostfixOps(YoctoNewton)
  def zN : A = forcePostfixOps(ZeptoNewton)
  def aN : A = forcePostfixOps(AttoNewton)
  def fN : A = forcePostfixOps(FemtoNewton)
  def pN : A = forcePostfixOps(PicoNewton)
  def nN : A = forcePostfixOps(NanoNewton)
  def microNewton : A = forcePostfixOps(MicroNewton)
  def microN : A = forcePostfixOps(MicroNewton)
  def μN : A = forcePostfixOps(MicroNewton)
  def mN : A = forcePostfixOps(MilliNewton)
  def cN : A = forcePostfixOps(CentiNewton)
  def dN : A = forcePostfixOps(DeciNewton)
  def N : A = forcePostfixOps(Newton)
  def daN : A = forcePostfixOps(DecaNewton)
  def hN : A = forcePostfixOps(HectoNewton)
  def kN : A = forcePostfixOps(KiloNewton)
  def MN : A = forcePostfixOps(MegaNewton)
  def GN : A = forcePostfixOps(GigaNewton)
  def TN : A = forcePostfixOps(TeraNewton)
  def PN : A = forcePostfixOps(PetaNewton)
  def EN : A = forcePostfixOps(ExaNewton)
  def ZN : A = forcePostfixOps(ZettaNewton)
  def YN : A = forcePostfixOps(YottaNewton)
  def dyn : A = forcePostfixOps(Dyne)
  def kgf : A = forcePostfixOps(KiloGramForce)
  def kp : A = forcePostfixOps(KiloGramForce)
}

trait ForceDot[A]{
  import ForceUnit._

  protected def forceDot(unit: ForceUnit): A

  def yN(dot: Dot): A = forceDot(YoctoNewton)
  def zN(dot: Dot): A = forceDot(ZeptoNewton)
  def aN(dot: Dot): A = forceDot(AttoNewton)
  def fN(dot: Dot): A = forceDot(FemtoNewton)
  def pN(dot: Dot): A = forceDot(PicoNewton)
  def nN(dot: Dot): A = forceDot(NanoNewton)
  def microNewton(dot: Dot): A = forceDot(MicroNewton)
  def microN(dot: Dot): A = forceDot(MicroNewton)
  def μN(dot: Dot): A = forceDot(MicroNewton)
  def mN(dot: Dot): A = forceDot(MilliNewton)
  def cN(dot: Dot): A = forceDot(CentiNewton)
  def dN(dot: Dot): A = forceDot(DeciNewton)
  def N(dot: Dot): A = forceDot(Newton)
  def daN(dot: Dot): A = forceDot(DecaNewton)
  def hN(dot: Dot): A = forceDot(HectoNewton)
  def kN(dot: Dot): A = forceDot(KiloNewton)
  def MN(dot: Dot): A = forceDot(MegaNewton)
  def GN(dot: Dot): A = forceDot(GigaNewton)
  def TN(dot: Dot): A = forceDot(TeraNewton)
  def PN(dot: Dot): A = forceDot(PetaNewton)
  def EN(dot: Dot): A = forceDot(ExaNewton)
  def ZN(dot: Dot): A = forceDot(ZettaNewton)
  def YN(dot: Dot): A = forceDot(YottaNewton)
  def dyn(dot: Dot): A = forceDot(Dyne)
  def kgf(dot: Dot): A = forceDot(KiloGramForce)
  def kp(dot: Dot): A = forceDot(KiloGramForce)
}

trait ForcePer[A]{
  import ForceUnit._

  protected def forcePer(unit: ForceUnit): A

  def yN(per: Per): A = forcePer(YoctoNewton)
  def zN(per: Per): A = forcePer(ZeptoNewton)
  def aN(per: Per): A = forcePer(AttoNewton)
  def fN(per: Per): A = forcePer(FemtoNewton)
  def pN(per: Per): A = forcePer(PicoNewton)
  def nN(per: Per): A = forcePer(NanoNewton)
  def microNewton(per: Per): A = forcePer(MicroNewton)
  def microN(per: Per): A = forcePer(MicroNewton)
  def μN(per: Per): A = forcePer(MicroNewton)
  def mN(per: Per): A = forcePer(MilliNewton)
  def cN(per: Per): A = forcePer(CentiNewton)
  def dN(per: Per): A = forcePer(DeciNewton)
  def N(per: Per): A = forcePer(Newton)
  def daN(per: Per): A = forcePer(DecaNewton)
  def hN(per: Per): A = forcePer(HectoNewton)
  def kN(per: Per): A = forcePer(KiloNewton)
  def MN(per: Per): A = forcePer(MegaNewton)
  def GN(per: Per): A = forcePer(GigaNewton)
  def TN(per: Per): A = forcePer(TeraNewton)
  def PN(per: Per): A = forcePer(PetaNewton)
  def EN(per: Per): A = forcePer(ExaNewton)
  def ZN(per: Per): A = forcePer(ZettaNewton)
  def YN(per: Per): A = forcePer(YottaNewton)
  def dyn(per: Per): A = forcePer(Dyne)
  def kgf(per: Per): A = forcePer(KiloGramForce)
  def kp(per: Per): A = forcePer(KiloGramForce)
}

trait PredefinedForceUnit extends ForcePostfixOps[ForceUnit]{
  override protected def forcePostfixOps(unit: ForceUnit) = unit
  
}

object PredefinedForceUnit extends PredefinedForceUnit
