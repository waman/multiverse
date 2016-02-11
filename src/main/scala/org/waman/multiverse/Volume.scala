package org.waman.multiverse

import org.waman.multiverse.Context._
import spire.implicits._
import spire.math.{Fractional, Real}

trait VolumePostfixOps[A]{

  import VolumePostfixOps._

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

  def cu_in: A = volumePostfixOps(VolumeUnit.CubicInch)

  def gal(c: Context): A = volumePostfixOps(_gal(c))

//  def bal =

  def λ: A = volumePostfixOps(VolumeUnit.Lambda)
}

object VolumePostfixOps{

  lazy val _gal: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates | UnitedStates_Fluid => VolumeUnit.Gallon_US_fluid
    case UnitedStates_Dry => VolumeUnit.Gallon_US_dry
    case Imperial         => VolumeUnit.Gallon_imperial
  }
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
  with DivisibleByTime[VolumeFlow[A]]
  with UnitConverter[A]{

  override protected lazy val algebra = implicitly[Fractional[A]]

  def apply(evalUnit: VolumeUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInCubicMetre) / real(evalUnit.unitInCubicMetre)

  override protected def volumePostfixOps(volumeUnit: VolumeUnit) = apply(volumeUnit)

  override def /(timeUnit: TimeUnit): VolumeFlow[A] =
    new VolumeFlow(value, unit / timeUnit)
}

abstract class VolumeUnit(val symbol: String, val unitInCubicMetre: Real)
  extends PhysicalUnit with DivisibleByTime[VolumeFlowUnit] {

  def this(symbol: String, factor: Real, volumeUnit: VolumeUnit) =
    this(symbol, factor * volumeUnit.unitInCubicMetre)

  def this(symbol: String, lengthUnit: LengthUnit) =
    this(symbol, lengthUnit.unitInMetre**3)

  def this(symbol: String, areaUnit: AreaUnit, lengthUnit: LengthUnit) =
    this(symbol, areaUnit.unitInSquareMetre * lengthUnit.unitInMetre)

  override protected val baseUnit = VolumeUnit.CubicMetre
  override protected val inBaseUnitAccessor = () => unitInCubicMetre

  override def /(timeUnit: TimeUnit): VolumeFlowUnit = VolumeFlowUnit(this, timeUnit)
}

object VolumeUnit{

  case object CubicYoctoMetre extends VolumeUnit("ym3", LengthUnit.YoctoMetre)
  case object CubicZeptoMetre extends VolumeUnit("zm3", LengthUnit.ZeptoMetre)
  case object CubicAttoMetre  extends VolumeUnit("am3", LengthUnit.AttoMetre)
  case object CubicFemtoMetre extends VolumeUnit("fm3", LengthUnit.FemtoMetre)
  case object CubicPicoMetre  extends VolumeUnit("pm3", LengthUnit.PicoMetre)
  case object CubicNanoMetre  extends VolumeUnit("nm3", LengthUnit.NanoMetre)
  case object CubicMicroMetre extends VolumeUnit("μm3", LengthUnit.MicroMetre)
  case object CubicMilliMetre extends VolumeUnit("mm3", LengthUnit.MilliMetre)
  case object CubicCentiMetre extends VolumeUnit("cm3", LengthUnit.CentiMetre)
  case object CubicDeciMetre  extends VolumeUnit("dm3", LengthUnit.DeciMetre)
  case object CubicMetre      extends VolumeUnit("m3" , 1)
  case object CubicDecaMetre  extends VolumeUnit("dam3", LengthUnit.DecaMetre)
  case object CubicHectoMetre extends VolumeUnit("hm3", LengthUnit.HectoMetre)
  case object CubicKiloMetre  extends VolumeUnit("km3", LengthUnit.KiloMetre)
  case object CubicMegaMetre  extends VolumeUnit("Mm3", LengthUnit.MegaMetre)
  case object CubicGigaMetre  extends VolumeUnit("Gm3", LengthUnit.GigaMetre)
  case object CubicTeraMetre  extends VolumeUnit("Tm3", LengthUnit.TeraMetre)
  case object CubicPetaMetre  extends VolumeUnit("Pm3", LengthUnit.PetaMetre)
  case object CubicExaMetre   extends VolumeUnit("Em3", LengthUnit.ExaMetre)
  case object CubicZettaMetre extends VolumeUnit("Zm3", LengthUnit.ZettaMetre)
  case object CubicYottaMetre extends VolumeUnit("Ym3", LengthUnit.YottaMetre)

  case object YoctoLitre extends VolumeUnit("yL", r"1e-24", Litre)
  case object ZeptoLitre extends VolumeUnit("zL", r"1e-21", Litre)
  case object AttoLitre  extends VolumeUnit("aL", r"1e-18", Litre)
  case object FemtoLitre extends VolumeUnit("fL", r"1e-15", Litre)
  case object PicoLitre  extends VolumeUnit("pL", r"1e-12", Litre)
  case object NanoLitre  extends VolumeUnit("nL", r"1e-9" , Litre)
  case object MicroLitre extends VolumeUnit("μL", r"1e-6" , Litre)
  case object MilliLitre extends VolumeUnit("mL", r"1e-3" , Litre)
  case object CentiLitre extends VolumeUnit("cL", r"1e-2" , Litre)
  case object DeciLitre  extends VolumeUnit("dL", r"1e-1" , Litre)
  case object Litre      extends VolumeUnit("L" , r"1e-3")
  case object DecaLitre  extends VolumeUnit("daL", r"1e1", Litre)
  case object HectoLitre extends VolumeUnit("hL", r"1e2" , Litre)
  case object KiloLitre  extends VolumeUnit("kL", r"1e3" , Litre)
  case object MegaLitre  extends VolumeUnit("ML", r"1e6" , Litre)
  case object GigaLitre  extends VolumeUnit("GL", r"1e9" , Litre)
  case object TeraLitre  extends VolumeUnit("TL", r"1e12", Litre)
  case object PetaLitre  extends VolumeUnit("PL", r"1e15", Litre)
  case object ExaLitre   extends VolumeUnit("EL", r"1e18", Litre)
  case object ZettaLitre extends VolumeUnit("ZL", r"1e21", Litre)
  case object YottaLitre extends VolumeUnit("YL", r"1e24", Litre)

  case object CubicInch   extends VolumeUnit("cu_in", LengthUnit.Inch)
  case object CubicFathom extends VolumeUnit("cu_fm", LengthUnit.Fathom)
  case object CubicFoot   extends VolumeUnit("cu_ft", LengthUnit.Foot)
  case object CubicYard   extends VolumeUnit("cu_yd", LengthUnit.Yard)
  case object CubicMile   extends VolumeUnit("cu_mi", LengthUnit.Mile)

  case object BoardFoot extends VolumeUnit("fbm", 144, CubicInch)
  case object AcreFoot extends VolumeUnit("ac_ft", AreaUnit.Acre, LengthUnit.Foot)

  case object Gallon_beer     extends VolumeUnit("beer_gal", 282, CubicInch)
  case object Gallon_US_fluid extends VolumeUnit("US_gal;gal(US);gal(US_fl)", 231, CubicInch)
  case object Gallon_US_dry   extends VolumeUnit("gal(US_dry)", r"1/8", Bushel_US_dry_level)
  case object Gallon_imperial extends VolumeUnit("imp_gal;gal(imp)", r"4.54609e-3")

  case object Quart_US_fluid extends VolumeUnit("qt(US);qt(US_fl)", r"1/4", Gallon_US_fluid)
  case object Quart_US_dry   extends VolumeUnit("qt(US_dry)", r"1/4", Gallon_US_dry)
  case object Quart_imperial extends VolumeUnit("qt(imp)", r"1/4", Gallon_imperial)

  case object Pint_US_fluid extends VolumeUnit("pt(US);pt(US_fl)", r"1/8", Gallon_US_fluid)
  case object Pint_US_dry   extends VolumeUnit("pt(US_dry)", r"1/8", Gallon_US_dry)
  case object Pint_imperial extends VolumeUnit("pt(imp)", r"1/8", Gallon_imperial)

  case object Barrel          extends VolumeUnit("bl;bbl", 42, Gallon_US_fluid)
  case object Barrel_US_fluid extends VolumeUnit("fl_bl(US)", r"31.5", Gallon_US_fluid)
  case object Barrel_US_dry   extends VolumeUnit("bl(US)", 105, Quart_US_dry)
  case object Barrel_imperial extends VolumeUnit("bl(imp)", 36, Gallon_imperial)

  case object Bushel_US_dry_level extends VolumeUnit("bu(US_lvl)", r"2150.42", CubicInch)
  case object Bushel_US_dry       extends VolumeUnit("bu(US_dry)", r"1.25", Bushel_US_dry_level)
  case object Bushel_imperial     extends VolumeUnit("bu(imp)", 8, Gallon_imperial)

  case object Bucket extends VolumeUnit("bkt", 4, Gallon_imperial)

  case object Lambda extends VolumeUnit("λ", r"1e-9")
}

trait PredefinedVolumeUnit extends VolumePostfixOps[VolumeUnit]{
  override protected def volumePostfixOps(volumeUnit: VolumeUnit) = volumeUnit
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