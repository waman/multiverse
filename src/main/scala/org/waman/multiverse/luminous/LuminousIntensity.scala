package org.waman.multiverse.luminous

import org.waman.multiverse._
import org.waman.multiverse.metric.{AreaPostfixOps, AreaUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

trait LuminousIntensityPostfixOps[A]{

  import LuminousIntensityUnit._

  protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit): A

  def ycd: A = luminousIntensityPostfixOps(YoctoCandela)
  def zcd: A = luminousIntensityPostfixOps(ZeptoCandela)
  def acd: A = luminousIntensityPostfixOps(AttoCandela)
  def fcd: A = luminousIntensityPostfixOps(FemtoCandela)
  def pcd: A = luminousIntensityPostfixOps(PicoCandela)
  def ncd: A = luminousIntensityPostfixOps(NanoCandela)
  def μcd: A = luminousIntensityPostfixOps(MicroCandela)
  def mcd: A = luminousIntensityPostfixOps(MilliCandela)
  def ccd: A = luminousIntensityPostfixOps(CentiCandela)
  def dcd: A = luminousIntensityPostfixOps(DeciCandela)
  def cd : A = luminousIntensityPostfixOps(Candela)
  def dacd: A = luminousIntensityPostfixOps(DecaCandela)
  def hcd: A = luminousIntensityPostfixOps(HectoCandela)
  def kcd: A = luminousIntensityPostfixOps(KiloCandela)
  def Mcd: A = luminousIntensityPostfixOps(MegaCandela)
  def Gcd: A = luminousIntensityPostfixOps(GigaCandela)
  def Tcd: A = luminousIntensityPostfixOps(TeraCandela)
  def Pcd: A = luminousIntensityPostfixOps(PetaCandela)
  def Ecd: A = luminousIntensityPostfixOps(ExaCandela)
  def Zcd: A = luminousIntensityPostfixOps(ZettaCandela)
  def Ycd: A = luminousIntensityPostfixOps(YottaCandela)
}

trait LuminousIntensityPer[A]{

  import LuminousIntensityUnit._

  protected def luminousIntensityPer(luminousIntensityUnit: LuminousIntensityUnit): A

  def ycd(per: Per): A = luminousIntensityPer(YoctoCandela)
  def zcd(per: Per): A = luminousIntensityPer(ZeptoCandela)
  def acd(per: Per): A = luminousIntensityPer(AttoCandela)
  def fcd(per: Per): A = luminousIntensityPer(FemtoCandela)
  def pcd(per: Per): A = luminousIntensityPer(PicoCandela)
  def ncd(per: Per): A = luminousIntensityPer(NanoCandela)
  def μcd(per: Per): A = luminousIntensityPer(MicroCandela)
  def mcd(per: Per): A = luminousIntensityPer(MilliCandela)
  def ccd(per: Per): A = luminousIntensityPer(CentiCandela)
  def dcd(per: Per): A = luminousIntensityPer(DeciCandela)
  def cd (per: Per): A = luminousIntensityPer(Candela)
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

class LuminousIntensity[A: Fractional](val value: A, val unit: LuminousIntensityUnit)
  extends Quantity[A, LuminousIntensityUnit]
    with LuminousIntensityPostfixOps[A]
    with DivisibleByAreaUnit[Luminance[A]]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: LuminousIntensityUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCandela) / real(evalUnit.unitInCandela)

  override protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit) =
    apply(luminousIntensityUnit)

  override def /(areaUnit: AreaUnit) = new Luminance[A](value, unit / areaUnit)
}

sealed abstract class LuminousIntensityUnit(val symbol: String, val unitInCandela: Real)
    extends PhysicalUnit[LuminousIntensityUnit]
    with DivisibleByAreaUnit[LuminanceUnit]{

  override def baseUnit = LuminousIntensityUnit.Candela
  override def valueInBaseUnit = unitInCandela

  override def /(areaUnit: AreaUnit) = LuminanceUnit(this, areaUnit)
}

object LuminousIntensityUnit extends ConstantsDefined[LuminousIntensityUnit]{

  case object YoctoCandela extends LuminousIntensityUnit("ycd", r"1e-24")
  case object ZeptoCandela extends LuminousIntensityUnit("zcd", r"1e-21")
  case object AttoCandela  extends LuminousIntensityUnit("acd", r"1e-18")
  case object FemtoCandela extends LuminousIntensityUnit("fcd", r"1e-15")
  case object PicoCandela  extends LuminousIntensityUnit("pcd", r"1e-12")
  case object NanoCandela  extends LuminousIntensityUnit("ncd", r"1e-9")
  case object MicroCandela extends LuminousIntensityUnit("μcd", r"1e-6")
  case object MilliCandela extends LuminousIntensityUnit("mcd", r"1e-3")
  case object CentiCandela extends LuminousIntensityUnit("ccd", r"1e-2")
  case object DeciCandela  extends LuminousIntensityUnit("dcd", r"1e-1")
  case object Candela      extends LuminousIntensityUnit("cd" , 1)
  case object DecaCandela  extends LuminousIntensityUnit("dacd", r"1e1")
  case object HectoCandela extends LuminousIntensityUnit("hcd", r"1e2")
  case object KiloCandela  extends LuminousIntensityUnit("kcd", r"1e3")
  case object MegaCandela  extends LuminousIntensityUnit("Mcd", r"1e6")
  case object GigaCandela  extends LuminousIntensityUnit("Gcd", r"1e9")
  case object TeraCandela  extends LuminousIntensityUnit("Tcd", r"1e12")
  case object PetaCandela  extends LuminousIntensityUnit("Pcd", r"1e15")
  case object ExaCandela   extends LuminousIntensityUnit("Ecd", r"1e18")
  case object ZettaCandela extends LuminousIntensityUnit("Zcd", r"1e21")
  case object YottaCandela extends LuminousIntensityUnit("Ycd", r"1e24")

  override lazy val values = Seq(
    YoctoCandela,
    ZeptoCandela,
    AttoCandela,
    FemtoCandela,
    PicoCandela,
    NanoCandela,
    MicroCandela,
    MilliCandela,
    CentiCandela,
    DeciCandela,
    Candela,
    DecaCandela,
    HectoCandela,
    KiloCandela,
    MegaCandela,
    GigaCandela,
    TeraCandela,
    PetaCandela,
    ExaCandela,
    ZettaCandela,
    YottaCandela
  )
}

trait PredefinedLuminousIntensityUnit extends LuminousIntensityPostfixOps[LuminousIntensityUnit]{

  override protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit) =
    luminousIntensityUnit
}

object PredefinedLuminousIntensityUnit extends PredefinedLuminousIntensityUnit

trait LuminousIntensityFactory[A]
    extends LuminousIntensityPostfixOps[LuminousIntensity[A]]
    with LuminousIntensityPer[AreaPostfixOps[Luminance[A]]]{

  def apply(unit: LuminousIntensityUnit): LuminousIntensity[A]

  override protected def luminousIntensityPostfixOps(luminousIntensityUnit: LuminousIntensityUnit) =
    apply(luminousIntensityUnit)

  // LuminousIntensity / Area -> Luminance
  def apply(unit: LuminanceUnit): Luminance[A]

  override protected def luminousIntensityPer(luminousIntensityUnit: LuminousIntensityUnit) =
    new AreaPostfixOps[Luminance[A]]{
      override protected def areaPostfixOps(areaUnit: AreaUnit) = apply(luminousIntensityUnit / areaUnit)
    }
}