package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait VolumePostfixOps[A]{

  // cubic
  def ym3: A
  def zm3: A
  def am3: A
  def fm3: A
  def pm3: A
  def nm3: A
  def μm3: A
  def mm3: A
  def cm3: A
  def dm3: A
  def m3 : A
  def dam3: A
  def hm3: A
  def km3: A
  def Mm3: A
  def Gm3: A
  def Tm3: A
  def Pm3: A
  def Em3: A
  def Zm3: A
  def Ym3: A

  // litre
  def yL: A
  def zL: A
  def aL: A
  def fL: A
  def pL: A
  def nL: A
  def μL: A
  def mL: A
  def cL: A
  def dL: A
  def L : A
  def daL: A
  def hL: A
  def kL: A
  def ML: A
  def GL: A
  def TL: A
  def PL: A
  def EL: A
  def ZL: A
  def YL: A

  def λ: A
}

class Volume[A: Fractional](val value: A, val unit: VolumeUnit)
  extends Quantity[A, VolumeUnit]
    with VolumePostfixOps[A]
    with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: VolumeUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.inCubicMetre) / real(evalUnit.inCubicMetre)

  // cubic
  def ym3 = apply(VolumeUnit.CubicYoctoMetre)
  def zm3 = apply(VolumeUnit.CubicZeptoMetre)
  def am3 = apply(VolumeUnit.CubicAttoMetre)
  def fm3 = apply(VolumeUnit.CubicFemtoMetre)
  def pm3 = apply(VolumeUnit.CubicPicoMetre)
  def nm3 = apply(VolumeUnit.CubicNanoMetre)
  def μm3 = apply(VolumeUnit.CubicMicroMetre)
  def mm3 = apply(VolumeUnit.CubicMilliMetre)
  def cm3 = apply(VolumeUnit.CubicCentiMetre)
  def dm3 = apply(VolumeUnit.CubicDeciMetre)
  def m3  = apply(VolumeUnit.CubicMetre)
  def dam3 = apply(VolumeUnit.CubicDecaMetre)
  def hm3 = apply(VolumeUnit.CubicHectoMetre)
  def km3 = apply(VolumeUnit.CubicKiloMetre)
  def Mm3 = apply(VolumeUnit.CubicMegaMetre)
  def Gm3 = apply(VolumeUnit.CubicGigaMetre)
  def Tm3 = apply(VolumeUnit.CubicTeraMetre)
  def Pm3 = apply(VolumeUnit.CubicPetaMetre)
  def Em3 = apply(VolumeUnit.CubicExaMetre)
  def Zm3 = apply(VolumeUnit.CubicZettaMetre)
  def Ym3 = apply(VolumeUnit.CubicYottaMetre)

  // litre
  def yL = apply(VolumeUnit.YoctoLitre)
  def zL = apply(VolumeUnit.ZeptoLitre)
  def aL = apply(VolumeUnit.AttoLitre)
  def fL = apply(VolumeUnit.FemtoLitre)
  def pL = apply(VolumeUnit.PicoLitre)
  def nL = apply(VolumeUnit.NanoLitre)
  def μL = apply(VolumeUnit.MicroLitre)
  def mL = apply(VolumeUnit.MilliLitre)
  def cL = apply(VolumeUnit.CentiLitre)
  def dL = apply(VolumeUnit.DeciLitre)
  def L  = apply(VolumeUnit.Litre)
  def daL = apply(VolumeUnit.DecaLitre)
  def hL = apply(VolumeUnit.HectoLitre)
  def kL = apply(VolumeUnit.KiloLitre)
  def ML = apply(VolumeUnit.MegaLitre)
  def GL = apply(VolumeUnit.GigaLitre)
  def TL = apply(VolumeUnit.TeraLitre)
  def PL = apply(VolumeUnit.PetaLitre)
  def EL = apply(VolumeUnit.ExaLitre)
  def ZL = apply(VolumeUnit.ZettaLitre)
  def YL = apply(VolumeUnit.YottaLitre)

  override def λ = apply(VolumeUnit.Lambda)
}

abstract class VolumeUnit(val symbol: String, val inCubicMetre: Real)
  extends PhysicalUnit {

  override protected def baseUnit = VolumeUnit.CubicMetre
  override protected def inBaseUnitAccessor = () => inCubicMetre
}

object VolumeUnit{
  case object CubicYoctoMetre extends VolumeUnit("ym3", r"1e-72")
  case object CubicZeptoMetre extends VolumeUnit("zm3", r"1e-63")
  case object CubicAttoMetre  extends VolumeUnit("am3", r"1e-54")
  case object CubicFemtoMetre extends VolumeUnit("fm3", r"1e-45")
  case object CubicPicoMetre  extends VolumeUnit("pm3", r"1e-36")
  case object CubicNanoMetre  extends VolumeUnit("nm3", r"1e-27")
  case object CubicMicroMetre extends VolumeUnit("μm3", r"1e-18")
  case object CubicMilliMetre extends VolumeUnit("mm3", r"1e-9")
  case object CubicCentiMetre extends VolumeUnit("cm3", r"1e-6")
  case object CubicDeciMetre  extends VolumeUnit("dm3", r"1e-3")
  case object CubicMetre      extends VolumeUnit("m3" , r"1")
  case object CubicDecaMetre  extends VolumeUnit("dam3", r"1e3")
  case object CubicHectoMetre extends VolumeUnit("hm3", r"1e6")
  case object CubicKiloMetre  extends VolumeUnit("km3", r"1e9")
  case object CubicMegaMetre  extends VolumeUnit("Mm3", r"1e18")
  case object CubicGigaMetre  extends VolumeUnit("Gm3", r"1e27")
  case object CubicTeraMetre  extends VolumeUnit("Tm3", r"1e36")
  case object CubicPetaMetre  extends VolumeUnit("Pm3", r"1e45")
  case object CubicExaMetre   extends VolumeUnit("Em3", r"1e54")
  case object CubicZettaMetre extends VolumeUnit("Zm3", r"1e63")
  case object CubicYottaMetre extends VolumeUnit("Ym3", r"1e72")

  case object YoctoLitre extends VolumeUnit("yL", r"1e-24")
  case object ZeptoLitre extends VolumeUnit("zL", r"1e-21")
  case object AttoLitre  extends VolumeUnit("aL", r"1e-18")
  case object FemtoLitre extends VolumeUnit("fL", r"1e-15")
  case object PicoLitre  extends VolumeUnit("pL", r"1e-12")
  case object NanoLitre  extends VolumeUnit("nL", r"1e-9")
  case object MicroLitre extends VolumeUnit("μL", r"1e-6")
  case object MilliLitre extends VolumeUnit("mL", r"1e-3")
  case object CentiLitre extends VolumeUnit("cL", r"1e-2")
  case object DeciLitre  extends VolumeUnit("dL", r"1e-1")
  case object Litre      extends VolumeUnit("L" , r"1")
  case object DecaLitre  extends VolumeUnit("daL", r"1e1")
  case object HectoLitre extends VolumeUnit("hL", r"1e2")
  case object KiloLitre  extends VolumeUnit("kL", r"1e3")
  case object MegaLitre  extends VolumeUnit("ML", r"1e6")
  case object GigaLitre  extends VolumeUnit("GL", r"1e9")
  case object TeraLitre  extends VolumeUnit("TL", r"1e12")
  case object PetaLitre  extends VolumeUnit("PL", r"1e15")
  case object ExaLitre   extends VolumeUnit("EL", r"1e18")
  case object ZettaLitre extends VolumeUnit("ZL", r"1e21")
  case object YottaLitre extends VolumeUnit("YL", r"1e24")

  case object Lambda extends VolumeUnit("λ", r"1e-9")
}

trait PredefinedVolumeUnit{

  val ym3 = VolumeUnit.CubicYoctoMetre
  val zm3 = VolumeUnit.CubicZeptoMetre
  val am3 = VolumeUnit.CubicAttoMetre
  val fm3 = VolumeUnit.CubicFemtoMetre
  val pm3 = VolumeUnit.CubicPicoMetre
  val nm3 = VolumeUnit.CubicNanoMetre
  val μm3 = VolumeUnit.CubicMicroMetre
  val mm3 = VolumeUnit.CubicMilliMetre
  val cm3 = VolumeUnit.CubicCentiMetre
  val dm3 = VolumeUnit.CubicDeciMetre
  val m3  = VolumeUnit.CubicMetre
  val dam3 = VolumeUnit.CubicDecaMetre
  val hm3 = VolumeUnit.CubicHectoMetre
  val km3 = VolumeUnit.CubicKiloMetre
  val Mm3 = VolumeUnit.CubicMegaMetre
  val Gm3 = VolumeUnit.CubicGigaMetre
  val Tm3 = VolumeUnit.CubicTeraMetre
  val Pm3 = VolumeUnit.CubicPetaMetre
  val Em3 = VolumeUnit.CubicExaMetre
  val Zm3 = VolumeUnit.CubicZettaMetre
  val Ym3 = VolumeUnit.CubicYottaMetre

  val yL = VolumeUnit.YoctoLitre
  val zL = VolumeUnit.ZeptoLitre
  val aL = VolumeUnit.AttoLitre
  val fL = VolumeUnit.FemtoLitre
  val pL = VolumeUnit.PicoLitre
  val nL = VolumeUnit.NanoLitre
  val μL = VolumeUnit.MicroLitre
  val mL = VolumeUnit.MilliLitre
  val cL = VolumeUnit.CentiLitre
  val dL = VolumeUnit.DeciLitre
  val L  = VolumeUnit.Litre
  val daL = VolumeUnit.DecaLitre
  val hL = VolumeUnit.HectoLitre
  val kL = VolumeUnit.KiloLitre
  val ML = VolumeUnit.MegaLitre
  val GL = VolumeUnit.GigaLitre
  val TL = VolumeUnit.TeraLitre
  val PL = VolumeUnit.PetaLitre
  val EL = VolumeUnit.ExaLitre
  val ZL = VolumeUnit.ZettaLitre
  val YL = VolumeUnit.YottaLitre

  val λ = VolumeUnit.Lambda
}

object PredefinedVolumeUnit extends PredefinedVolumeUnit

trait VolumeUnitInterpreter[A]
  extends VolumePostfixOps[Volume[A]]{

  def apply(unit: VolumeUnit): Volume[A]

  override def ym3 = apply(VolumeUnit.CubicYoctoMetre)
  override def zm3 = apply(VolumeUnit.CubicZeptoMetre)
  override def am3 = apply(VolumeUnit.CubicAttoMetre)
  override def fm3 = apply(VolumeUnit.CubicFemtoMetre)
  override def pm3 = apply(VolumeUnit.CubicPicoMetre)
  override def nm3 = apply(VolumeUnit.CubicNanoMetre)
  override def μm3 = apply(VolumeUnit.CubicMicroMetre)
  override def mm3 = apply(VolumeUnit.CubicMilliMetre)
  override def cm3 = apply(VolumeUnit.CubicCentiMetre)
  override def dm3 = apply(VolumeUnit.CubicDeciMetre)
  override def m3  = apply(VolumeUnit.CubicMetre)
  override def dam3 = apply(VolumeUnit.CubicDecaMetre)
  override def hm3 = apply(VolumeUnit.CubicHectoMetre)
  override def km3 = apply(VolumeUnit.CubicKiloMetre)
  override def Mm3 = apply(VolumeUnit.CubicMegaMetre)
  override def Gm3 = apply(VolumeUnit.CubicGigaMetre)
  override def Tm3 = apply(VolumeUnit.CubicTeraMetre)
  override def Pm3 = apply(VolumeUnit.CubicPetaMetre)
  override def Em3 = apply(VolumeUnit.CubicExaMetre)
  override def Zm3 = apply(VolumeUnit.CubicZettaMetre)
  override def Ym3 = apply(VolumeUnit.CubicYottaMetre)

  override def yL = apply(VolumeUnit.YoctoLitre)
  override def zL = apply(VolumeUnit.ZeptoLitre)
  override def aL = apply(VolumeUnit.AttoLitre)
  override def fL = apply(VolumeUnit.FemtoLitre)
  override def pL = apply(VolumeUnit.PicoLitre)
  override def nL = apply(VolumeUnit.NanoLitre)
  override def μL = apply(VolumeUnit.MicroLitre)
  override def mL = apply(VolumeUnit.MilliLitre)
  override def cL = apply(VolumeUnit.CentiLitre)
  override def dL = apply(VolumeUnit.DeciLitre)
  override def L  = apply(VolumeUnit.Litre)
  override def daL = apply(VolumeUnit.DecaLitre)
  override def hL = apply(VolumeUnit.HectoLitre)
  override def kL = apply(VolumeUnit.KiloLitre)
  override def ML = apply(VolumeUnit.MegaLitre)
  override def GL = apply(VolumeUnit.GigaLitre)
  override def TL = apply(VolumeUnit.TeraLitre)
  override def PL = apply(VolumeUnit.PetaLitre)
  override def EL = apply(VolumeUnit.ExaLitre)
  override def ZL = apply(VolumeUnit.ZettaLitre)
  override def YL = apply(VolumeUnit.YottaLitre)

  override def λ = apply(VolumeUnit.Lambda)
}