package org.waman.multiverse

import org.waman.multiverse.Context._
import org.waman.multiverse.VolumeUnit._
import spire.implicits._
import spire.math.{Fractional, Real}

trait VolumePostfixOps[A]{
  import VolumePostfixOps._
  import VolumeUnit._

  protected def volumePostfixOps(volumeUnit: VolumeUnit): A

  // cubic
  def ym3: A = volumePostfixOps(CubicYoctoMetre)
  def zm3: A = volumePostfixOps(CubicZeptoMetre)
  def am3: A = volumePostfixOps(CubicAttoMetre)
  def fm3: A = volumePostfixOps(CubicFemtoMetre)
  def pm3: A = volumePostfixOps(CubicPicoMetre)
  def nm3: A = volumePostfixOps(CubicNanoMetre)
  def μm3: A = volumePostfixOps(CubicMicroMetre)
  def mm3: A = volumePostfixOps(CubicMilliMetre)
  def cm3: A = volumePostfixOps(CubicCentiMetre)
  def dm3: A = volumePostfixOps(CubicDeciMetre)
  def m3 : A = volumePostfixOps(CubicMetre)
  def dam3: A = volumePostfixOps(CubicDecaMetre)
  def hm3: A = volumePostfixOps(CubicHectoMetre)
  def km3: A = volumePostfixOps(CubicKiloMetre)
  def Mm3: A = volumePostfixOps(CubicMegaMetre)
  def Gm3: A = volumePostfixOps(CubicGigaMetre)
  def Tm3: A = volumePostfixOps(CubicTeraMetre)
  def Pm3: A = volumePostfixOps(CubicPetaMetre)
  def Em3: A = volumePostfixOps(CubicExaMetre)
  def Zm3: A = volumePostfixOps(CubicZettaMetre)
  def Ym3: A = volumePostfixOps(CubicYottaMetre)

  // litre
  def yL: A = volumePostfixOps(YoctoLitre)
  def zL: A = volumePostfixOps(ZeptoLitre)
  def aL: A = volumePostfixOps(AttoLitre)
  def fL: A = volumePostfixOps(FemtoLitre)
  def pL: A = volumePostfixOps(PicoLitre)
  def nL: A = volumePostfixOps(NanoLitre)
  def μL: A = volumePostfixOps(MicroLitre)
  def mL: A = volumePostfixOps(MilliLitre)
  def cL: A = volumePostfixOps(CentiLitre)
  def dL: A = volumePostfixOps(DeciLitre)
  def L : A = volumePostfixOps(Litre)
  def daL: A = volumePostfixOps(DecaLitre)
  def hL: A = volumePostfixOps(HectoLitre)
  def kL: A = volumePostfixOps(KiloLitre)
  def ML: A = volumePostfixOps(MegaLitre)
  def GL: A = volumePostfixOps(GigaLitre)
  def TL: A = volumePostfixOps(TeraLitre)
  def PL: A = volumePostfixOps(PetaLitre)
  def EL: A = volumePostfixOps(ExaLitre)
  def ZL: A = volumePostfixOps(ZettaLitre)
  def YL: A = volumePostfixOps(YottaLitre)

  def λ: A = volumePostfixOps(Lambda)

  def cu_in: A = volumePostfixOps(CubicInch)
  def cu_ft: A = volumePostfixOps(CubicFoot)
  def cu_yd: A = volumePostfixOps(CubicYard)
  def cu_fm: A = volumePostfixOps(CubicFathom)
  def cu_mi: A = volumePostfixOps(CubicMile)

  def fbm: A = volumePostfixOps(BoardFoot)
  def ac_ft: A = volumePostfixOps(AcreFoot)

  def beer_gal: A = volumePostfixOps(Gallon_beer)
  def per: A = volumePostfixOps(Perch)
  def bl : A = volumePostfixOps(Barrel)
  def bbl: A = bl

  def minim(c: Context): A = volumePostfixOps(_minim(c))
  def US_fl_oz         : A = volumePostfixOps(Fluid_Ounce_US)
  def fl_oz(c: Context): A = volumePostfixOps(_fl_oz(c))
  def gi(c: Context)   : A = volumePostfixOps(_gi(c))
  def nog              : A = volumePostfixOps(Gill_imperial)
  def pt(c: Context)   : A = volumePostfixOps(_pt(c))
  def qt(c: Context)   : A = volumePostfixOps(_qt(c))
  def gal              : A = volumePostfixOps(Gallon_US_fluid)
  def US_gal           : A = volumePostfixOps(Gallon_US_fluid)
  def imp_gal          : A = volumePostfixOps(Gallon_imperial)
  def gal(c: Context)  : A = volumePostfixOps(_gal(c))
  def pk(c: Context)   : A = volumePostfixOps(_pk(c))
  def bu(c: Context)   : A = volumePostfixOps(_bu(c))
  def fl_bl            : A = volumePostfixOps(Barrel_US_fluid)
  def fl_bl(c: Context): A = volumePostfixOps(_fl_bl(c))
  def bl(c: Context)   : A = volumePostfixOps(_bl(c))
  def hhd(c: Context)  : A = volumePostfixOps(_hhd(c))

  def fl_s: A = volumePostfixOps(FluidScruple)
  def fl_dr(c: Context): A = volumePostfixOps(_fl_dr(c))
  def bkt: A = volumePostfixOps(Bucket)
}

object VolumePostfixOps{

  lazy val _minim: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates => Minim_US
    case Imperial => Minim_imperial
  }

  lazy val _fl_oz: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates => Fluid_Ounce_US
    case Imperial => Fluid_Ounce_imperial
  }

  lazy val _gi: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates => Gill_US
    case Imperial => Gill_imperial
  }

  lazy val _pt: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates_Fluid => Pint_US_fluid
    case UnitedStates_Dry => Pint_US_dry
    case Imperial => Pint_imperial
  }

  lazy val _qt: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates_Fluid => Quart_US_fluid
    case UnitedStates_Dry => Quart_US_dry
    case Imperial => Quart_imperial
  }

  lazy val _gal: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates | UnitedStates_Fluid => Gallon_US_fluid
    case UnitedStates_Dry => Gallon_US_dry
    case Imperial => Gallon_imperial
  }

  lazy val _pk: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates_Dry => Peck_US_dry
    case Imperial => Peck_imperial
  }

  lazy val _bu: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates_Dry => Bushel_US_dry
    case UnitedStates_Dry_Level => Bushel_US_dry_level
    case Imperial => Bushel_imperial
  }

  lazy val _bl: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates_Fluid => Barrel_US_fluid
    case UnitedStates_Dry => Barrel_US_dry
    case Imperial => Barrel_imperial
  }

  lazy val _fl_bl: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates => Barrel_US_fluid
  }

  lazy val _hhd: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates => Hogshead_US
    case Imperial => Hogshead_imperial
  }

  lazy val _fl_dr: PartialFunction[Context, VolumeUnit] = {
    case UnitedStates => FluidDram_US
    case Imperial => FluidDrachm_imperial
  }
}

trait VolumePer[A]{
  import VolumeUnit._

  protected def volumePer(volumeUnit: VolumeUnit): A

  // cubic
  def ym3(per: Per): A = volumePer(CubicYoctoMetre)
  def zm3(per: Per): A = volumePer(CubicZeptoMetre)
  def am3(per: Per): A = volumePer(CubicAttoMetre)
  def fm3(per: Per): A = volumePer(CubicFemtoMetre)
  def pm3(per: Per): A = volumePer(CubicPicoMetre)
  def nm3(per: Per): A = volumePer(CubicNanoMetre)
  def μm3(per: Per): A = volumePer(CubicMicroMetre)
  def mm3(per: Per): A = volumePer(CubicMilliMetre)
  def cm3(per: Per): A = volumePer(CubicCentiMetre)
  def dm3(per: Per): A = volumePer(CubicDeciMetre)
  def m3 (per: Per): A = volumePer(CubicMetre)
  def dam3(per: Per): A = volumePer(CubicDecaMetre)
  def hm3(per: Per): A = volumePer(CubicHectoMetre)
  def km3(per: Per): A = volumePer(CubicKiloMetre)
  def Mm3(per: Per): A = volumePer(CubicMegaMetre)
  def Gm3(per: Per): A = volumePer(CubicGigaMetre)
  def Tm3(per: Per): A = volumePer(CubicTeraMetre)
  def Pm3(per: Per): A = volumePer(CubicPetaMetre)
  def Em3(per: Per): A = volumePer(CubicExaMetre)
  def Zm3(per: Per): A = volumePer(CubicZettaMetre)
  def Ym3(per: Per): A = volumePer(CubicYottaMetre)

  // litre
  def yL(per: Per): A = volumePer(YoctoLitre)
  def zL(per: Per): A = volumePer(ZeptoLitre)
  def aL(per: Per): A = volumePer(AttoLitre)
  def fL(per: Per): A = volumePer(FemtoLitre)
  def pL(per: Per): A = volumePer(PicoLitre)
  def nL(per: Per): A = volumePer(NanoLitre)
  def μL(per: Per): A = volumePer(MicroLitre)
  def mL(per: Per): A = volumePer(MilliLitre)
  def cL(per: Per): A = volumePer(CentiLitre)
  def dL(per: Per): A = volumePer(DeciLitre)
  def L (per: Per): A = volumePer(Litre)
  def daL(per: Per): A = volumePer(DecaLitre)
  def hL(per: Per): A = volumePer(HectoLitre)
  def kL(per: Per): A = volumePer(KiloLitre)
  def ML(per: Per): A = volumePer(MegaLitre)
  def GL(per: Per): A = volumePer(GigaLitre)
  def TL(per: Per): A = volumePer(TeraLitre)
  def PL(per: Per): A = volumePer(PetaLitre)
  def EL(per: Per): A = volumePer(ExaLitre)
  def ZL(per: Per): A = volumePer(ZettaLitre)
  def YL(per: Per): A = volumePer(YottaLitre)

  def λ(per: Per): A = volumePer(Lambda)

  def cu_in(per: Per): A = volumePer(CubicInch)
  def cu_ft(per: Per): A = volumePer(CubicFoot)
  def cu_yd(per: Per): A = volumePer(CubicYard)
  def cu_fm(per: Per): A = volumePer(CubicFathom)
  def cu_mi(per: Per): A = volumePer(CubicMile)

  def fbm(per: Per): A = volumePer(BoardFoot)
  def ac_ft(per: Per): A = volumePer(AcreFoot)

  def beer_gal(per: Per): A = volumePer(Gallon_beer)
  def per(per: Per): A = volumePer(Perch)
  def bl (per: Per): A = volumePer(Barrel)
  def bbl(per: Per): A = bl(per)

  def US_fl_oz(per: Per): A = volumePer(Fluid_Ounce_US)
  def gal     (per: Per): A = volumePer(Gallon_US_fluid)
  def US_gal  (per: Per): A = volumePer(Gallon_US_fluid)
  def imp_gal (per: Per): A = volumePer(Gallon_imperial)
  def nog     (per: Per): A = volumePer(Gill_imperial)
  def fl_bl   (per: Per): A = volumePer(Barrel_US_fluid)

  def fl_s(per: Per): A = volumePer(FluidScruple)
  def bkt(per: Per): A = volumePer(Bucket)
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
    extends PhysicalUnit[VolumeUnit]
    with DivisibleByTime[VolumeFlowUnit] {

  def this(symbol: String, factor: Real, volumeUnit: VolumeUnit) =
    this(symbol, factor * volumeUnit.unitInCubicMetre)

  def this(symbol: String, lengthUnit: LengthUnit) =
    this(symbol, lengthUnit.unitInMetre**3)

  def this(symbol: String, areaUnit: AreaUnit, lengthUnit: LengthUnit) =
    this(symbol, areaUnit.unitInSquareMetre * lengthUnit.unitInMetre)

  override val baseUnit = VolumeUnit.CubicMetre
  override val inBaseUnitAccessor = () => unitInCubicMetre

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

  case object Lambda extends VolumeUnit("λ", r"1e-9")

  case object CubicInch   extends VolumeUnit("cu_in", LengthUnit.Inch)
  case object CubicFoot   extends VolumeUnit("cu_ft", LengthUnit.Foot)
  case object CubicYard   extends VolumeUnit("cu_yd", LengthUnit.Yard)
  case object CubicFathom extends VolumeUnit("cu_fm", LengthUnit.Fathom)
  case object CubicMile   extends VolumeUnit("cu_mi", LengthUnit.Mile)

  case object BoardFoot extends VolumeUnit("fbm", 144, CubicInch)
  case object AcreFoot extends VolumeUnit("ac_ft", AreaUnit.Acre, LengthUnit.Foot)

  case object Gallon_beer extends VolumeUnit("beer_gal", 282, CubicInch)
  case object Perch extends VolumeUnit("per", r"33/2" * r"3/2", CubicFoot)
  case object Barrel extends VolumeUnit("bl;bbl", 42, Gallon_US_fluid)

  // US
  case object Minim_US       extends VolumeUnit("minim(US)", r"1/480", Fluid_Ounce_US)
  case object Fluid_Ounce_US extends VolumeUnit("US_fl_oz;fl_oz(US)", r"1/128", Gallon_US_fluid)
  case object Gill_US        extends VolumeUnit("gi(US)", 4, Fluid_Ounce_US)
  case object Hogshead_US    extends VolumeUnit("hhd(US)", 2, Barrel_US_fluid)
  case object FluidDram_US   extends VolumeUnit("fl_dr(US)", r"1/8", Fluid_Ounce_US)

  // US fluid
  case object Pint_US_fluid        extends VolumeUnit("pt(US_fl)", r"1/8", Gallon_US_fluid)
  case object Quart_US_fluid       extends VolumeUnit("qt(US_fl)", r"1/4", Gallon_US_fluid)
  case object Gallon_US_fluid      extends VolumeUnit("gal;US_gal;gal(US);gal(US_fl)", 231, CubicInch)
  case object Barrel_US_fluid      extends VolumeUnit("bl(US_fl);fl_bl;fl_bl(US)", r"31.5", Gallon_US_fluid)

  // US dry
  case object Pint_US_dry   extends VolumeUnit("pt(US_dry)", r"1/8", Gallon_US_dry)
  case object Quart_US_dry  extends VolumeUnit("qt(US_dry)", r"1/4", Gallon_US_dry)
  case object Gallon_US_dry extends VolumeUnit("gal(US_dry)", r"1/8", Bushel_US_dry_level)
  case object Peck_US_dry   extends VolumeUnit("pk(US_dry)", r"1/4", Bushel_US_dry_level)
  case object Bushel_US_dry extends VolumeUnit("bu(US_dry)", r"5/4", Bushel_US_dry_level)
  case object Bushel_US_dry_level extends VolumeUnit("bu(US_lvl)", r"2150.42", CubicInch)
  case object Barrel_US_dry extends VolumeUnit("bl(US);bl(US_dry)", 105, Quart_US_dry)

  // imperial
  case object Minim_imperial       extends VolumeUnit("minim(imp)", r"1/480", Fluid_Ounce_imperial)
  case object Fluid_Ounce_imperial extends VolumeUnit("fl_oz(imp)", r"1/160", Gallon_imperial)
  case object Gill_imperial        extends VolumeUnit("gi(imp);nog", 5, Fluid_Ounce_imperial)
  case object Pint_imperial        extends VolumeUnit("pt(imp)", r"1/8", Gallon_imperial)
  case object Quart_imperial       extends VolumeUnit("qt(imp)", r"1/4", Gallon_imperial)
  case object Gallon_imperial      extends VolumeUnit("imp_gal;gal(imp)", r"4.54609", Litre)
  case object Peck_imperial        extends VolumeUnit("pk(imp)", 2, Gallon_imperial)
  case object Bushel_imperial      extends VolumeUnit("bu(imp)", 8, Gallon_imperial)
  case object Barrel_imperial      extends VolumeUnit("bl(imp)", 36, Gallon_imperial)
  case object Hogshead_imperial    extends VolumeUnit("hhd(imp)", 2, Barrel_imperial)

  case object FluidScruple extends VolumeUnit("fl_s", r"1/24", Fluid_Ounce_imperial)
  case object FluidDrachm_imperial extends VolumeUnit("fl_dr(imp)", r"1/8", Fluid_Ounce_imperial)

  case object Bucket extends VolumeUnit("bkt", 4, Gallon_imperial)
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