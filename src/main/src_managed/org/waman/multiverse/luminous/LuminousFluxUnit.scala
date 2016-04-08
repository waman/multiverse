package org.waman.multiverse.luminous

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._

sealed trait LuminousFluxUnit extends PhysicalUnit[LuminousFluxUnit]
  with DivisibleByAreaUnit[IlluminanceUnit]{

  override def getSIUnit = org.waman.multiverse.luminous.LuminousFluxUnit.Lumen

  override def /(unit: AreaUnit) = IlluminanceUnit(this, unit)
}

object LuminousFluxUnit extends ConstantsDefined[LuminousFluxUnit]{

  // intrinsic
  private[LuminousFluxUnit]
  class IntrinsicLuminousFluxUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends LuminousFluxUnit{

    def this(name: String, symbols: Seq[String], unit: LuminousFluxUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: LuminousFluxUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoLumen extends IntrinsicLuminousFluxUnit("YoctoLumen", Seq("ylm"), r"1e-24")
  case object ZeptoLumen extends IntrinsicLuminousFluxUnit("ZeptoLumen", Seq("zlm"), r"1e-21")
  case object AttoLumen extends IntrinsicLuminousFluxUnit("AttoLumen", Seq("alm"), r"1e-18")
  case object FemtoLumen extends IntrinsicLuminousFluxUnit("FemtoLumen", Seq("flm"), r"1e-15")
  case object PicoLumen extends IntrinsicLuminousFluxUnit("PicoLumen", Seq("plm"), r"1e-12")
  case object NanoLumen extends IntrinsicLuminousFluxUnit("NanoLumen", Seq("nlm"), r"1e-9")
  case object MicroLumen extends IntrinsicLuminousFluxUnit("MicroLumen", Seq("μlm", "mclm"), r"1e-6")
  case object MilliLumen extends IntrinsicLuminousFluxUnit("MilliLumen", Seq("mlm"), r"1e-3")
  case object CentiLumen extends IntrinsicLuminousFluxUnit("CentiLumen", Seq("clm"), r"1e-2")
  case object DeciLumen extends IntrinsicLuminousFluxUnit("DeciLumen", Seq("dlm"), r"1e-1")
  case object Lumen extends IntrinsicLuminousFluxUnit("Lumen", Seq("lm"), r"1")
  case object DecaLumen extends IntrinsicLuminousFluxUnit("DecaLumen", Seq("dalm"), r"1e1")
  case object HectoLumen extends IntrinsicLuminousFluxUnit("HectoLumen", Seq("hlm"), r"1e2")
  case object KiloLumen extends IntrinsicLuminousFluxUnit("KiloLumen", Seq("klm"), r"1e3")
  case object MegaLumen extends IntrinsicLuminousFluxUnit("MegaLumen", Seq("Mlm"), r"1e6")
  case object GigaLumen extends IntrinsicLuminousFluxUnit("GigaLumen", Seq("Glm"), r"1e9")
  case object TeraLumen extends IntrinsicLuminousFluxUnit("TeraLumen", Seq("Tlm"), r"1e12")
  case object PetaLumen extends IntrinsicLuminousFluxUnit("PetaLumen", Seq("Plm"), r"1e15")
  case object ExaLumen extends IntrinsicLuminousFluxUnit("ExaLumen", Seq("Elm"), r"1e18")
  case object ZettaLumen extends IntrinsicLuminousFluxUnit("ZettaLumen", Seq("Zlm"), r"1e21")
  case object YottaLumen extends IntrinsicLuminousFluxUnit("YottaLumen", Seq("Ylm"), r"1e24")

  override lazy val values = Seq(YoctoLumen, ZeptoLumen, AttoLumen, FemtoLumen, PicoLumen, NanoLumen, MicroLumen, MilliLumen, CentiLumen, DeciLumen, Lumen, DecaLumen, HectoLumen, KiloLumen, MegaLumen, GigaLumen, TeraLumen, PetaLumen, ExaLumen, ZettaLumen, YottaLumen)
}

trait MultiplicativeByLuminousFluxUnit[R]{
  def *(unit: LuminousFluxUnit): R
}

trait DivisibleByLuminousFluxUnit[R]{
  def /(unit: LuminousFluxUnit): R
}

trait LuminousFluxPostfixOps[A]{
  import LuminousFluxUnit._

  protected def luminousFluxPostfixOps(unit: LuminousFluxUnit): A


  def ylm : A = luminousFluxPostfixOps(YoctoLumen)
  def zlm : A = luminousFluxPostfixOps(ZeptoLumen)
  def alm : A = luminousFluxPostfixOps(AttoLumen)
  def flm : A = luminousFluxPostfixOps(FemtoLumen)
  def plm : A = luminousFluxPostfixOps(PicoLumen)
  def nlm : A = luminousFluxPostfixOps(NanoLumen)
  def μlm : A = luminousFluxPostfixOps(MicroLumen)
  def mclm : A = luminousFluxPostfixOps(MicroLumen)
  def mlm : A = luminousFluxPostfixOps(MilliLumen)
  def clm : A = luminousFluxPostfixOps(CentiLumen)
  def dlm : A = luminousFluxPostfixOps(DeciLumen)
  def lm : A = luminousFluxPostfixOps(Lumen)
  def dalm : A = luminousFluxPostfixOps(DecaLumen)
  def hlm : A = luminousFluxPostfixOps(HectoLumen)
  def klm : A = luminousFluxPostfixOps(KiloLumen)
  def Mlm : A = luminousFluxPostfixOps(MegaLumen)
  def Glm : A = luminousFluxPostfixOps(GigaLumen)
  def Tlm : A = luminousFluxPostfixOps(TeraLumen)
  def Plm : A = luminousFluxPostfixOps(PetaLumen)
  def Elm : A = luminousFluxPostfixOps(ExaLumen)
  def Zlm : A = luminousFluxPostfixOps(ZettaLumen)
  def Ylm : A = luminousFluxPostfixOps(YottaLumen)
}

trait LuminousFluxDot[A]{
  import LuminousFluxUnit._

  protected def luminousFluxDot(unit: LuminousFluxUnit): A

  def ylm(dot: Dot): A = luminousFluxDot(YoctoLumen)
  def zlm(dot: Dot): A = luminousFluxDot(ZeptoLumen)
  def alm(dot: Dot): A = luminousFluxDot(AttoLumen)
  def flm(dot: Dot): A = luminousFluxDot(FemtoLumen)
  def plm(dot: Dot): A = luminousFluxDot(PicoLumen)
  def nlm(dot: Dot): A = luminousFluxDot(NanoLumen)
  def μlm(dot: Dot): A = luminousFluxDot(MicroLumen)
  def mclm(dot: Dot): A = luminousFluxDot(MicroLumen)
  def mlm(dot: Dot): A = luminousFluxDot(MilliLumen)
  def clm(dot: Dot): A = luminousFluxDot(CentiLumen)
  def dlm(dot: Dot): A = luminousFluxDot(DeciLumen)
  def lm(dot: Dot): A = luminousFluxDot(Lumen)
  def dalm(dot: Dot): A = luminousFluxDot(DecaLumen)
  def hlm(dot: Dot): A = luminousFluxDot(HectoLumen)
  def klm(dot: Dot): A = luminousFluxDot(KiloLumen)
  def Mlm(dot: Dot): A = luminousFluxDot(MegaLumen)
  def Glm(dot: Dot): A = luminousFluxDot(GigaLumen)
  def Tlm(dot: Dot): A = luminousFluxDot(TeraLumen)
  def Plm(dot: Dot): A = luminousFluxDot(PetaLumen)
  def Elm(dot: Dot): A = luminousFluxDot(ExaLumen)
  def Zlm(dot: Dot): A = luminousFluxDot(ZettaLumen)
  def Ylm(dot: Dot): A = luminousFluxDot(YottaLumen)
}

trait LuminousFluxPer[A]{
  import LuminousFluxUnit._

  protected def luminousFluxPer(unit: LuminousFluxUnit): A

  def ylm(per: Per): A = luminousFluxPer(YoctoLumen)
  def zlm(per: Per): A = luminousFluxPer(ZeptoLumen)
  def alm(per: Per): A = luminousFluxPer(AttoLumen)
  def flm(per: Per): A = luminousFluxPer(FemtoLumen)
  def plm(per: Per): A = luminousFluxPer(PicoLumen)
  def nlm(per: Per): A = luminousFluxPer(NanoLumen)
  def μlm(per: Per): A = luminousFluxPer(MicroLumen)
  def mclm(per: Per): A = luminousFluxPer(MicroLumen)
  def mlm(per: Per): A = luminousFluxPer(MilliLumen)
  def clm(per: Per): A = luminousFluxPer(CentiLumen)
  def dlm(per: Per): A = luminousFluxPer(DeciLumen)
  def lm(per: Per): A = luminousFluxPer(Lumen)
  def dalm(per: Per): A = luminousFluxPer(DecaLumen)
  def hlm(per: Per): A = luminousFluxPer(HectoLumen)
  def klm(per: Per): A = luminousFluxPer(KiloLumen)
  def Mlm(per: Per): A = luminousFluxPer(MegaLumen)
  def Glm(per: Per): A = luminousFluxPer(GigaLumen)
  def Tlm(per: Per): A = luminousFluxPer(TeraLumen)
  def Plm(per: Per): A = luminousFluxPer(PetaLumen)
  def Elm(per: Per): A = luminousFluxPer(ExaLumen)
  def Zlm(per: Per): A = luminousFluxPer(ZettaLumen)
  def Ylm(per: Per): A = luminousFluxPer(YottaLumen)
}

trait PredefinedLuminousFluxUnit extends LuminousFluxPostfixOps[LuminousFluxUnit]{
  override protected def luminousFluxPostfixOps(unit: LuminousFluxUnit) = unit
  
}

object PredefinedLuminousFluxUnit extends PredefinedLuminousFluxUnit
