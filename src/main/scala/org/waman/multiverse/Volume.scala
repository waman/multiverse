package org.waman.multiverse

import spire.math.Real
import spire.math.Fractional
import spire.implicits._

trait VolumePostfixOps[A]{

  protected def volumePostfixOps(volumeUnit: VolumeUnit): A

  // cubic
  def ym3: A = volumePostfixOps(VolumeUnit.CubicYoctoMetre)
  def zm3: A = volumePostfixOps(VolumeUnit.CubicZeptoMetre)
  def am3: A = volumePostfixOps(VolumeUnit.CubicAttoMetre)
  def fm3: A = volumePostfixOps(VolumeUnit.CubicFemtoMetre)
  def pm3: A = volumePostfixOps(VolumeUnit.CubicPicoMetre)
  def nm3: A = volumePostfixOps(VolumeUnit.CubicNanoMetre)
  def μm3: A = volumePostfixOps(VolumeUnit.CubicMicroMetre)
  def mm3: A = volumePostfixOps(VolumeUnit.CubicMilliMetre)
  def cm3: A = volumePostfixOps(VolumeUnit.CubicCentiMetre)
  def dm3: A = volumePostfixOps(VolumeUnit.CubicDeciMetre)
  def m3 : A = volumePostfixOps(VolumeUnit.CubicMetre)
  def dam3: A = volumePostfixOps(VolumeUnit.CubicDecaMetre)
  def hm3: A = volumePostfixOps(VolumeUnit.CubicHectoMetre)
  def km3: A = volumePostfixOps(VolumeUnit.CubicKiloMetre)
  def Mm3: A = volumePostfixOps(VolumeUnit.CubicMegaMetre)
  def Gm3: A = volumePostfixOps(VolumeUnit.CubicGigaMetre)
  def Tm3: A = volumePostfixOps(VolumeUnit.CubicTeraMetre)
  def Pm3: A = volumePostfixOps(VolumeUnit.CubicPetaMetre)
  def Em3: A = volumePostfixOps(VolumeUnit.CubicExaMetre)
  def Zm3: A = volumePostfixOps(VolumeUnit.CubicZettaMetre)
  def Ym3: A = volumePostfixOps(VolumeUnit.CubicYottaMetre)

  // litre
  def yL: A = volumePostfixOps(VolumeUnit.YoctoLitre)
  def zL: A = volumePostfixOps(VolumeUnit.ZeptoLitre)
  def aL: A = volumePostfixOps(VolumeUnit.AttoLitre)
  def fL: A = volumePostfixOps(VolumeUnit.FemtoLitre)
  def pL: A = volumePostfixOps(VolumeUnit.PicoLitre)
  def nL: A = volumePostfixOps(VolumeUnit.NanoLitre)
  def μL: A = volumePostfixOps(VolumeUnit.MicroLitre)
  def mL: A = volumePostfixOps(VolumeUnit.MilliLitre)
  def cL: A = volumePostfixOps(VolumeUnit.CentiLitre)
  def dL: A = volumePostfixOps(VolumeUnit.DeciLitre)
  def L : A = volumePostfixOps(VolumeUnit.Litre)
  def daL: A = volumePostfixOps(VolumeUnit.DecaLitre)
  def hL: A = volumePostfixOps(VolumeUnit.HectoLitre)
  def kL: A = volumePostfixOps(VolumeUnit.KiloLitre)
  def ML: A = volumePostfixOps(VolumeUnit.MegaLitre)
  def GL: A = volumePostfixOps(VolumeUnit.GigaLitre)
  def TL: A = volumePostfixOps(VolumeUnit.TeraLitre)
  def PL: A = volumePostfixOps(VolumeUnit.PetaLitre)
  def EL: A = volumePostfixOps(VolumeUnit.ExaLitre)
  def ZL: A = volumePostfixOps(VolumeUnit.ZettaLitre)
  def YL: A = volumePostfixOps(VolumeUnit.YottaLitre)

  def λ: A = volumePostfixOps(VolumeUnit.Lambda)
}

trait VolumePer[A]{

  protected def volumePer(volumeUnit: VolumeUnit): A

  // cubic
  def ym3(per: Per): A = volumePer(VolumeUnit.CubicYoctoMetre)
  def zm3(per: Per): A = volumePer(VolumeUnit.CubicZeptoMetre)
  def am3(per: Per): A = volumePer(VolumeUnit.CubicAttoMetre)
  def fm3(per: Per): A = volumePer(VolumeUnit.CubicFemtoMetre)
  def pm3(per: Per): A = volumePer(VolumeUnit.CubicPicoMetre)
  def nm3(per: Per): A = volumePer(VolumeUnit.CubicNanoMetre)
  def μm3(per: Per): A = volumePer(VolumeUnit.CubicMicroMetre)
  def mm3(per: Per): A = volumePer(VolumeUnit.CubicMilliMetre)
  def cm3(per: Per): A = volumePer(VolumeUnit.CubicCentiMetre)
  def dm3(per: Per): A = volumePer(VolumeUnit.CubicDeciMetre)
  def m3 (per: Per): A = volumePer(VolumeUnit.CubicMetre)
  def dam3(per: Per): A = volumePer(VolumeUnit.CubicDecaMetre)
  def hm3(per: Per): A = volumePer(VolumeUnit.CubicHectoMetre)
  def km3(per: Per): A = volumePer(VolumeUnit.CubicKiloMetre)
  def Mm3(per: Per): A = volumePer(VolumeUnit.CubicMegaMetre)
  def Gm3(per: Per): A = volumePer(VolumeUnit.CubicGigaMetre)
  def Tm3(per: Per): A = volumePer(VolumeUnit.CubicTeraMetre)
  def Pm3(per: Per): A = volumePer(VolumeUnit.CubicPetaMetre)
  def Em3(per: Per): A = volumePer(VolumeUnit.CubicExaMetre)
  def Zm3(per: Per): A = volumePer(VolumeUnit.CubicZettaMetre)
  def Ym3(per: Per): A = volumePer(VolumeUnit.CubicYottaMetre)

  // litre
  def yL(per: Per): A = volumePer(VolumeUnit.YoctoLitre)
  def zL(per: Per): A = volumePer(VolumeUnit.ZeptoLitre)
  def aL(per: Per): A = volumePer(VolumeUnit.AttoLitre)
  def fL(per: Per): A = volumePer(VolumeUnit.FemtoLitre)
  def pL(per: Per): A = volumePer(VolumeUnit.PicoLitre)
  def nL(per: Per): A = volumePer(VolumeUnit.NanoLitre)
  def μL(per: Per): A = volumePer(VolumeUnit.MicroLitre)
  def mL(per: Per): A = volumePer(VolumeUnit.MilliLitre)
  def cL(per: Per): A = volumePer(VolumeUnit.CentiLitre)
  def dL(per: Per): A = volumePer(VolumeUnit.DeciLitre)
  def L (per: Per): A = volumePer(VolumeUnit.Litre)
  def daL(per: Per): A = volumePer(VolumeUnit.DecaLitre)
  def hL(per: Per): A = volumePer(VolumeUnit.HectoLitre)
  def kL(per: Per): A = volumePer(VolumeUnit.KiloLitre)
  def ML(per: Per): A = volumePer(VolumeUnit.MegaLitre)
  def GL(per: Per): A = volumePer(VolumeUnit.GigaLitre)
  def TL(per: Per): A = volumePer(VolumeUnit.TeraLitre)
  def PL(per: Per): A = volumePer(VolumeUnit.PetaLitre)
  def EL(per: Per): A = volumePer(VolumeUnit.ExaLitre)
  def ZL(per: Per): A = volumePer(VolumeUnit.ZettaLitre)
  def YL(per: Per): A = volumePer(VolumeUnit.YottaLitre)

  def λ(per: Per): A = volumePer(VolumeUnit.Lambda)
}

class Volume[A: Fractional](val value: A, val unit: VolumeUnit) extends Quantity[A, VolumeUnit]
  with VolumePostfixOps[A]
  with DivisibleBy[TimeUnit, VolumeFlow[A]]
  with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: VolumeUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCubicMetre) / real(evalUnit.unitInCubicMetre)

  override protected def volumePostfixOps(volumeUnit: VolumeUnit) = apply(volumeUnit)

  override def /(timeUnit: TimeUnit): VolumeFlow[A] = new VolumeFlow(value, unit / timeUnit)
}

abstract class VolumeUnit(val symbol: String, val unitInCubicMetre: Real)
  extends PhysicalUnit with DivisibleBy[TimeUnit, VolumeFlowUnit] {

  override protected val baseUnit = VolumeUnit.CubicMetre
  override protected val inBaseUnitAccessor = () => unitInCubicMetre

  override def /(timeUnit: TimeUnit): VolumeFlowUnit = VolumeFlowUnit(this, timeUnit)
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

  case object YoctoLitre extends VolumeUnit("yL", r"1e-27")
  case object ZeptoLitre extends VolumeUnit("zL", r"1e-24")
  case object AttoLitre  extends VolumeUnit("aL", r"1e-21")
  case object FemtoLitre extends VolumeUnit("fL", r"1e-18")
  case object PicoLitre  extends VolumeUnit("pL", r"1e-15")
  case object NanoLitre  extends VolumeUnit("nL", r"1e-12")
  case object MicroLitre extends VolumeUnit("μL", r"1e-9")
  case object MilliLitre extends VolumeUnit("mL", r"1e-6")
  case object CentiLitre extends VolumeUnit("cL", r"1e-5")
  case object DeciLitre  extends VolumeUnit("dL", r"1e-4")
  case object Litre      extends VolumeUnit("L" , r"1e-3")
  case object DecaLitre  extends VolumeUnit("daL", r"1e-2")
  case object HectoLitre extends VolumeUnit("hL", r"1e-1")
  case object KiloLitre  extends VolumeUnit("kL", r"1")
  case object MegaLitre  extends VolumeUnit("ML", r"1e3")
  case object GigaLitre  extends VolumeUnit("GL", r"1e6")
  case object TeraLitre  extends VolumeUnit("TL", r"1e9")
  case object PetaLitre  extends VolumeUnit("PL", r"1e12")
  case object ExaLitre   extends VolumeUnit("EL", r"1e15")
  case object ZettaLitre extends VolumeUnit("ZL", r"1e18")
  case object YottaLitre extends VolumeUnit("YL", r"1e21")

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

trait VolumeUnitInterpreter[A] extends VolumePostfixOps[Volume[A]]
  with VolumePer[TimePostfixOps[VolumeFlow[A]]]{

  def apply(unit: VolumeUnit): Volume[A]

  override protected def volumePostfixOps(volumeUnit: VolumeUnit) = apply(volumeUnit)

  override protected def volumePer(volumeUnit: VolumeUnit) =  new TimePostfixOps[VolumeFlow[A]]{
    override protected def timePostfixOps(timeUnit: TimeUnit) = apply(volumeUnit / timeUnit)
  }

  def apply(volumeFlowUnit: VolumeFlowUnit): VolumeFlow[A]
}