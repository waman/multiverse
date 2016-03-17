package org.waman.multiverse.luminous

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.metric._

sealed trait LuminousIntensityUnit extends PhysicalUnit[LuminousIntensityUnit]
  with DivisibleByAreaUnit[LuminanceUnit]{

  def unitInCandela: Real

  override def baseUnit = org.waman.multiverse.luminous.LuminousIntensityUnit.Candela
  override def valueInBaseUnit = unitInCandela

  override def /(unit: AreaUnit) = LuminanceUnit(this, unit)
}

object LuminousIntensityUnit extends ConstantsDefined[LuminousIntensityUnit]{

  // intrinsic
  private[LuminousIntensityUnit]
  class IntrinsicLuminousIntensityUnit(name: String, val symbols: Seq[String], val unitInCandela: Real)
      extends LuminousIntensityUnit{

    def this(name: String, symbols: Seq[String], unit: LuminousIntensityUnit) =
      this(name, symbols, unit.unitInCandela)

    def this(name: String, symbols: Seq[String], factor: Real, unit: LuminousIntensityUnit) =
      this(name, symbols, factor * unit.unitInCandela)
  }


  case object YoctoCandela extends IntrinsicLuminousIntensityUnit("YoctoCandela", Seq("ycd"), r"1e-24")
  case object ZeptoCandela extends IntrinsicLuminousIntensityUnit("ZeptoCandela", Seq("zcd"), r"1e-21")
  case object AttoCandela extends IntrinsicLuminousIntensityUnit("AttoCandela", Seq("acd"), r"1e-18")
  case object FemtoCandela extends IntrinsicLuminousIntensityUnit("FemtoCandela", Seq("fcd"), r"1e-15")
  case object PicoCandela extends IntrinsicLuminousIntensityUnit("PicoCandela", Seq("pcd"), r"1e-12")
  case object NanoCandela extends IntrinsicLuminousIntensityUnit("NanoCandela", Seq("ncd"), r"1e-9")
  case object MicroCandela extends IntrinsicLuminousIntensityUnit("MicroCandela", Seq("microCandela", "microCd", "μcd"), r"1e-6")
  case object MilliCandela extends IntrinsicLuminousIntensityUnit("MilliCandela", Seq("mcd"), r"1e-3")
  case object CentiCandela extends IntrinsicLuminousIntensityUnit("CentiCandela", Seq("ccd"), r"1e-2")
  case object DeciCandela extends IntrinsicLuminousIntensityUnit("DeciCandela", Seq("dcd"), r"1e-1")
  case object Candela extends IntrinsicLuminousIntensityUnit("Candela", Seq("cd"), r"1")
  case object DecaCandela extends IntrinsicLuminousIntensityUnit("DecaCandela", Seq("dacd"), r"1e1")
  case object HectoCandela extends IntrinsicLuminousIntensityUnit("HectoCandela", Seq("hcd"), r"1e2")
  case object KiloCandela extends IntrinsicLuminousIntensityUnit("KiloCandela", Seq("kcd"), r"1e3")
  case object MegaCandela extends IntrinsicLuminousIntensityUnit("MegaCandela", Seq("Mcd"), r"1e6")
  case object GigaCandela extends IntrinsicLuminousIntensityUnit("GigaCandela", Seq("Gcd"), r"1e9")
  case object TeraCandela extends IntrinsicLuminousIntensityUnit("TeraCandela", Seq("Tcd"), r"1e12")
  case object PetaCandela extends IntrinsicLuminousIntensityUnit("PetaCandela", Seq("Pcd"), r"1e15")
  case object ExaCandela extends IntrinsicLuminousIntensityUnit("ExaCandela", Seq("Ecd"), r"1e18")
  case object ZettaCandela extends IntrinsicLuminousIntensityUnit("ZettaCandela", Seq("Zcd"), r"1e21")
  case object YottaCandela extends IntrinsicLuminousIntensityUnit("YottaCandela", Seq("Ycd"), r"1e24")

  override lazy val values = Seq(YoctoCandela, ZeptoCandela, AttoCandela, FemtoCandela, PicoCandela, NanoCandela, MicroCandela, MilliCandela, CentiCandela, DeciCandela, Candela, DecaCandela, HectoCandela, KiloCandela, MegaCandela, GigaCandela, TeraCandela, PetaCandela, ExaCandela, ZettaCandela, YottaCandela)
}

trait MultiplicativeByLuminousIntensityUnit[R]{
  def *(unit: LuminousIntensityUnit): R
}

trait DivisibleByLuminousIntensityUnit[R]{
  def /(unit: LuminousIntensityUnit): R
}

trait LuminousIntensityPostfixOps[A]{
  import LuminousIntensityUnit._

  protected def luminousIntensityPostfixOps(unit: LuminousIntensityUnit): A

  def ycd : A = luminousIntensityPostfixOps(YoctoCandela)
  def zcd : A = luminousIntensityPostfixOps(ZeptoCandela)
  def acd : A = luminousIntensityPostfixOps(AttoCandela)
  def fcd : A = luminousIntensityPostfixOps(FemtoCandela)
  def pcd : A = luminousIntensityPostfixOps(PicoCandela)
  def ncd : A = luminousIntensityPostfixOps(NanoCandela)
  def microCandela : A = luminousIntensityPostfixOps(MicroCandela)
  def microCd : A = luminousIntensityPostfixOps(MicroCandela)
  def μcd : A = luminousIntensityPostfixOps(MicroCandela)
  def mcd : A = luminousIntensityPostfixOps(MilliCandela)
  def ccd : A = luminousIntensityPostfixOps(CentiCandela)
  def dcd : A = luminousIntensityPostfixOps(DeciCandela)
  def cd : A = luminousIntensityPostfixOps(Candela)
  def dacd : A = luminousIntensityPostfixOps(DecaCandela)
  def hcd : A = luminousIntensityPostfixOps(HectoCandela)
  def kcd : A = luminousIntensityPostfixOps(KiloCandela)
  def Mcd : A = luminousIntensityPostfixOps(MegaCandela)
  def Gcd : A = luminousIntensityPostfixOps(GigaCandela)
  def Tcd : A = luminousIntensityPostfixOps(TeraCandela)
  def Pcd : A = luminousIntensityPostfixOps(PetaCandela)
  def Ecd : A = luminousIntensityPostfixOps(ExaCandela)
  def Zcd : A = luminousIntensityPostfixOps(ZettaCandela)
  def Ycd : A = luminousIntensityPostfixOps(YottaCandela)
}

trait LuminousIntensityDot[A]{
  import LuminousIntensityUnit._

  protected def luminousIntensityDot(unit: LuminousIntensityUnit): A

  def ycd(dot: Dot): A = luminousIntensityDot(YoctoCandela)
  def zcd(dot: Dot): A = luminousIntensityDot(ZeptoCandela)
  def acd(dot: Dot): A = luminousIntensityDot(AttoCandela)
  def fcd(dot: Dot): A = luminousIntensityDot(FemtoCandela)
  def pcd(dot: Dot): A = luminousIntensityDot(PicoCandela)
  def ncd(dot: Dot): A = luminousIntensityDot(NanoCandela)
  def microCandela(dot: Dot): A = luminousIntensityDot(MicroCandela)
  def microCd(dot: Dot): A = luminousIntensityDot(MicroCandela)
  def μcd(dot: Dot): A = luminousIntensityDot(MicroCandela)
  def mcd(dot: Dot): A = luminousIntensityDot(MilliCandela)
  def ccd(dot: Dot): A = luminousIntensityDot(CentiCandela)
  def dcd(dot: Dot): A = luminousIntensityDot(DeciCandela)
  def cd(dot: Dot): A = luminousIntensityDot(Candela)
  def dacd(dot: Dot): A = luminousIntensityDot(DecaCandela)
  def hcd(dot: Dot): A = luminousIntensityDot(HectoCandela)
  def kcd(dot: Dot): A = luminousIntensityDot(KiloCandela)
  def Mcd(dot: Dot): A = luminousIntensityDot(MegaCandela)
  def Gcd(dot: Dot): A = luminousIntensityDot(GigaCandela)
  def Tcd(dot: Dot): A = luminousIntensityDot(TeraCandela)
  def Pcd(dot: Dot): A = luminousIntensityDot(PetaCandela)
  def Ecd(dot: Dot): A = luminousIntensityDot(ExaCandela)
  def Zcd(dot: Dot): A = luminousIntensityDot(ZettaCandela)
  def Ycd(dot: Dot): A = luminousIntensityDot(YottaCandela)
}

trait LuminousIntensityPer[A]{
  import LuminousIntensityUnit._

  protected def luminousIntensityPer(unit: LuminousIntensityUnit): A

  def ycd(per: Per): A = luminousIntensityPer(YoctoCandela)
  def zcd(per: Per): A = luminousIntensityPer(ZeptoCandela)
  def acd(per: Per): A = luminousIntensityPer(AttoCandela)
  def fcd(per: Per): A = luminousIntensityPer(FemtoCandela)
  def pcd(per: Per): A = luminousIntensityPer(PicoCandela)
  def ncd(per: Per): A = luminousIntensityPer(NanoCandela)
  def microCandela(per: Per): A = luminousIntensityPer(MicroCandela)
  def microCd(per: Per): A = luminousIntensityPer(MicroCandela)
  def μcd(per: Per): A = luminousIntensityPer(MicroCandela)
  def mcd(per: Per): A = luminousIntensityPer(MilliCandela)
  def ccd(per: Per): A = luminousIntensityPer(CentiCandela)
  def dcd(per: Per): A = luminousIntensityPer(DeciCandela)
  def cd(per: Per): A = luminousIntensityPer(Candela)
  def dacd(per: Per): A = luminousIntensityPer(DecaCandela)
  def hcd(per: Per): A = luminousIntensityPer(HectoCandela)
  def kcd(per: Per): A = luminousIntensityPer(KiloCandela)
  def Mcd(per: Per): A = luminousIntensityPer(MegaCandela)
  def Gcd(per: Per): A = luminousIntensityPer(GigaCandela)
  def Tcd(per: Per): A = luminousIntensityPer(TeraCandela)
  def Pcd(per: Per): A = luminousIntensityPer(PetaCandela)
  def Ecd(per: Per): A = luminousIntensityPer(ExaCandela)
  def Zcd(per: Per): A = luminousIntensityPer(ZettaCandela)
  def Ycd(per: Per): A = luminousIntensityPer(YottaCandela)
}

trait PredefinedLuminousIntensityUnit extends LuminousIntensityPostfixOps[LuminousIntensityUnit]{
  override protected def luminousIntensityPostfixOps(unit: LuminousIntensityUnit) = unit
  
}

object PredefinedLuminousIntensityUnit extends PredefinedLuminousIntensityUnit
