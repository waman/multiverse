package org.waman.multiverse.mechanics

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._
import org.waman.multiverse.time._
import org.waman.multiverse.mass._
import org.waman.multiverse.fluid._
import org.waman.multiverse.mechanics.AccelerationUnit._
import org.waman.multiverse.mass.MassUnit._

sealed trait ForceUnit extends PhysicalUnit[ForceUnit]
  with MultiplicativeByLengthUnit[TorqueUnit]
  with MultiplicativeByTimeUnit[MomentumUnit]
  with DivisibleByAreaUnit[PressureUnit]{

  override def getSIUnit = org.waman.multiverse.mechanics.ForceUnit.Newton

  override def *(unit: LengthUnit) = TorqueUnit(this, unit)

  override def *(unit: TimeUnit) = MomentumUnit(this, unit)

  override def /(unit: AreaUnit) = PressureUnit(this, unit)
}

object ForceUnit extends ConstantsDefined[ForceUnit]{

  // intrinsic
  private[ForceUnit]
  class IntrinsicForceUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends ForceUnit{

    def this(name: String, symbols: Seq[String], unit: ForceUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: ForceUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoNewton extends IntrinsicForceUnit("YoctoNewton", Seq("yN"), r"1e-24")
  case object ZeptoNewton extends IntrinsicForceUnit("ZeptoNewton", Seq("zN"), r"1e-21")
  case object AttoNewton extends IntrinsicForceUnit("AttoNewton", Seq("aN"), r"1e-18")
  case object FemtoNewton extends IntrinsicForceUnit("FemtoNewton", Seq("fN"), r"1e-15")
  case object PicoNewton extends IntrinsicForceUnit("PicoNewton", Seq("pN"), r"1e-12")
  case object NanoNewton extends IntrinsicForceUnit("NanoNewton", Seq("nN"), r"1e-9")
  case object MicroNewton extends IntrinsicForceUnit("MicroNewton", Seq("μN", "mcN"), r"1e-6")
  case object MilliNewton extends IntrinsicForceUnit("MilliNewton", Seq("mN"), r"1e-3")
  case object CentiNewton extends IntrinsicForceUnit("CentiNewton", Seq("cN"), r"1e-2")
  case object DeciNewton extends IntrinsicForceUnit("DeciNewton", Seq("dN"), r"1e-1")
  case object Newton extends IntrinsicForceUnit("Newton", Seq("N"), r"1")
  case object DecaNewton extends IntrinsicForceUnit("DecaNewton", Seq("daN"), r"1e1")
  case object HectoNewton extends IntrinsicForceUnit("HectoNewton", Seq("hN"), r"1e2")
  case object KiloNewton extends IntrinsicForceUnit("KiloNewton", Seq("kN"), r"1e3")
  case object MegaNewton extends IntrinsicForceUnit("MegaNewton", Seq("MN"), r"1e6")
  case object GigaNewton extends IntrinsicForceUnit("GigaNewton", Seq("GN"), r"1e9")
  case object TeraNewton extends IntrinsicForceUnit("TeraNewton", Seq("TN"), r"1e12")
  case object PetaNewton extends IntrinsicForceUnit("PetaNewton", Seq("PN"), r"1e15")
  case object ExaNewton extends IntrinsicForceUnit("ExaNewton", Seq("EN"), r"1e18")
  case object ZettaNewton extends IntrinsicForceUnit("ZettaNewton", Seq("ZN"), r"1e21")
  case object YottaNewton extends IntrinsicForceUnit("YottaNewton", Seq("YN"), r"1e24")
  case object Dyne extends IntrinsicForceUnit("Dyne", Seq("dyn"), r"1e-5")
  case object KiloGramForce extends IntrinsicForceUnit("KiloGramForce", Seq("kgf", "kp", "Gf"), StandardGravity.unitValueInSIUnit) with NotExact
  case object MilliGraveForce extends IntrinsicForceUnit("MilliGraveForce", Seq("mGf", "gf"), StandardGravity.unitValueInSIUnit / 1000) with NotExact
  case object OunceForce extends IntrinsicForceUnit("OunceForce", Seq("ozf"), Ounce * StandardGravity)
  case object PoundForce extends IntrinsicForceUnit("PoundForce", Seq("lbf"), Pound * StandardGravity)
  case object Poundal extends IntrinsicForceUnit("Poundal", Seq("pdl"), Pound * FootPerSecondSquared)
  case object KipForce extends IntrinsicForceUnit("KipForce", Seq("kipf", "klbf"), 1000, Pound * StandardGravity)
  case object ShortTonForce extends IntrinsicForceUnit("ShortTonForce", Seq("sh_tnf"), ShortTon * StandardGravity)
  case object LongTonForce extends IntrinsicForceUnit("LongTonForce", Seq("tnf", "long_tnf"), LongTon * StandardGravity)
  case object Sthene extends IntrinsicForceUnit("Sthene", Seq("sn"), r"1e3")

  override lazy val values = Seq(YoctoNewton, ZeptoNewton, AttoNewton, FemtoNewton, PicoNewton, NanoNewton, MicroNewton, MilliNewton, CentiNewton, DeciNewton, Newton, DecaNewton, HectoNewton, KiloNewton, MegaNewton, GigaNewton, TeraNewton, PetaNewton, ExaNewton, ZettaNewton, YottaNewton, Dyne, KiloGramForce, MilliGraveForce, OunceForce, PoundForce, Poundal, KipForce, ShortTonForce, LongTonForce, Sthene)

  // MassUnit * AccelerationUnit -> Force
  private[ForceUnit]
  class ProductMassDotAccelerationUnit(val firstUnit: MassUnit, val secondUnit: AccelerationUnit)
      extends ForceUnit with ProductUnit[ForceUnit, MassUnit, AccelerationUnit]{

    override lazy val unitValueInSIUnit: Real =
      firstUnit.unitValueInSIUnit * secondUnit.unitValueInSIUnit
  }

  def apply(unit1: MassUnit, unit2: AccelerationUnit): ForceUnit =
    new ProductMassDotAccelerationUnit(unit1, unit2)
}

trait MultiplicativeByForceUnit[R]{
  def *(unit: ForceUnit): R
}

trait DivisibleByForceUnit[R]{
  def /(unit: ForceUnit): R
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
  def μN : A = forcePostfixOps(MicroNewton)
  def mcN : A = forcePostfixOps(MicroNewton)
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
  def Gf : A = forcePostfixOps(KiloGramForce)
  def mGf : A = forcePostfixOps(MilliGraveForce)
  def gf : A = forcePostfixOps(MilliGraveForce)
  def ozf : A = forcePostfixOps(OunceForce)
  def lbf : A = forcePostfixOps(PoundForce)
  def pdl : A = forcePostfixOps(Poundal)
  def kipf : A = forcePostfixOps(KipForce)
  def klbf : A = forcePostfixOps(KipForce)
  def sh_tnf : A = forcePostfixOps(ShortTonForce)
  def tnf : A = forcePostfixOps(LongTonForce)
  def long_tnf : A = forcePostfixOps(LongTonForce)
  def sn : A = forcePostfixOps(Sthene)
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
  def μN(dot: Dot): A = forceDot(MicroNewton)
  def mcN(dot: Dot): A = forceDot(MicroNewton)
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
  def Gf(dot: Dot): A = forceDot(KiloGramForce)
  def mGf(dot: Dot): A = forceDot(MilliGraveForce)
  def gf(dot: Dot): A = forceDot(MilliGraveForce)
  def ozf(dot: Dot): A = forceDot(OunceForce)
  def lbf(dot: Dot): A = forceDot(PoundForce)
  def pdl(dot: Dot): A = forceDot(Poundal)
  def kipf(dot: Dot): A = forceDot(KipForce)
  def klbf(dot: Dot): A = forceDot(KipForce)
  def sh_tnf(dot: Dot): A = forceDot(ShortTonForce)
  def tnf(dot: Dot): A = forceDot(LongTonForce)
  def long_tnf(dot: Dot): A = forceDot(LongTonForce)
  def sn(dot: Dot): A = forceDot(Sthene)
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
  def μN(per: Per): A = forcePer(MicroNewton)
  def mcN(per: Per): A = forcePer(MicroNewton)
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
  def Gf(per: Per): A = forcePer(KiloGramForce)
  def mGf(per: Per): A = forcePer(MilliGraveForce)
  def gf(per: Per): A = forcePer(MilliGraveForce)
  def ozf(per: Per): A = forcePer(OunceForce)
  def lbf(per: Per): A = forcePer(PoundForce)
  def pdl(per: Per): A = forcePer(Poundal)
  def kipf(per: Per): A = forcePer(KipForce)
  def klbf(per: Per): A = forcePer(KipForce)
  def sh_tnf(per: Per): A = forcePer(ShortTonForce)
  def tnf(per: Per): A = forcePer(LongTonForce)
  def long_tnf(per: Per): A = forcePer(LongTonForce)
  def sn(per: Per): A = forcePer(Sthene)
}

trait PredefinedForceUnit extends ForcePostfixOps[ForceUnit]{
  override protected def forcePostfixOps(unit: ForceUnit) = unit
  
}

object PredefinedForceUnit extends PredefinedForceUnit
