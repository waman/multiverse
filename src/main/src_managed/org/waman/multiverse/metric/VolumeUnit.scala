package org.waman.multiverse.metric

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.time._
import org.waman.multiverse.fluid._
import org.waman.multiverse.energy._

sealed trait VolumeUnit extends PhysicalUnit[VolumeUnit]
  with MultiplicativeByPressureUnit[EnergyUnit]
  with DivisibleByTimeUnit[VolumeFlowUnit]{

  def unitInCubicMetre: Real

  override def baseUnit = org.waman.multiverse.metric.VolumeUnit.CubicMetre
  override def valueInBaseUnit = unitInCubicMetre

  override def *(unit: PressureUnit) = EnergyUnit(this, unit)

  override def /(unit: TimeUnit) = VolumeFlowUnit(this, unit)
}

object VolumeUnit extends ConstantsDefined[VolumeUnit]{

  // intrinsic
  private[VolumeUnit]
  class IntrinsicVolumeUnit(name: String, val symbols: Seq[String], val unitInCubicMetre: Real)
      extends VolumeUnit{

    def this(name: String, symbols: Seq[String], unit: VolumeUnit) =
      this(name, symbols, unit.unitInCubicMetre)

    def this(name: String, symbols: Seq[String], factor: Real, unit: VolumeUnit) =
      this(name, symbols, factor * unit.unitInCubicMetre)
  }


  case object YoctoCubicMetre extends IntrinsicVolumeUnit("YoctoCubicMetre", Seq("ym3"), r"1e-24"**3)
  case object ZeptoCubicMetre extends IntrinsicVolumeUnit("ZeptoCubicMetre", Seq("zm3"), r"1e-21"**3)
  case object AttoCubicMetre extends IntrinsicVolumeUnit("AttoCubicMetre", Seq("am3"), r"1e-18"**3)
  case object FemtoCubicMetre extends IntrinsicVolumeUnit("FemtoCubicMetre", Seq("fm3"), r"1e-15"**3)
  case object PicoCubicMetre extends IntrinsicVolumeUnit("PicoCubicMetre", Seq("pm3"), r"1e-12"**3)
  case object NanoCubicMetre extends IntrinsicVolumeUnit("NanoCubicMetre", Seq("nm3"), r"1e-9"**3)
  case object MicroCubicMetre extends IntrinsicVolumeUnit("MicroCubicMetre", Seq("microCubicMetre", "microM3", "μm3"), r"1e-6"**3)
  case object MilliCubicMetre extends IntrinsicVolumeUnit("MilliCubicMetre", Seq("mm3"), r"1e-3"**3)
  case object CentiCubicMetre extends IntrinsicVolumeUnit("CentiCubicMetre", Seq("cm3"), r"1e-2"**3)
  case object DeciCubicMetre extends IntrinsicVolumeUnit("DeciCubicMetre", Seq("dm3"), r"1e-1"**3)
  case object CubicMetre extends IntrinsicVolumeUnit("CubicMetre", Seq("m3"), r"1"**3)
  case object DecaCubicMetre extends IntrinsicVolumeUnit("DecaCubicMetre", Seq("dam3"), r"1e1"**3)
  case object HectoCubicMetre extends IntrinsicVolumeUnit("HectoCubicMetre", Seq("hm3"), r"1e2"**3)
  case object KiloCubicMetre extends IntrinsicVolumeUnit("KiloCubicMetre", Seq("km3"), r"1e3"**3)
  case object MegaCubicMetre extends IntrinsicVolumeUnit("MegaCubicMetre", Seq("Mm3"), r"1e6"**3)
  case object GigaCubicMetre extends IntrinsicVolumeUnit("GigaCubicMetre", Seq("Gm3"), r"1e9"**3)
  case object TeraCubicMetre extends IntrinsicVolumeUnit("TeraCubicMetre", Seq("Tm3"), r"1e12"**3)
  case object PetaCubicMetre extends IntrinsicVolumeUnit("PetaCubicMetre", Seq("Pm3"), r"1e15"**3)
  case object ExaCubicMetre extends IntrinsicVolumeUnit("ExaCubicMetre", Seq("Em3"), r"1e18"**3)
  case object ZettaCubicMetre extends IntrinsicVolumeUnit("ZettaCubicMetre", Seq("Zm3"), r"1e21"**3)
  case object YottaCubicMetre extends IntrinsicVolumeUnit("YottaCubicMetre", Seq("Ym3"), r"1e24"**3)
  case object YoctoLitre extends IntrinsicVolumeUnit("YoctoLitre", Seq("yL"), r"1e-24" * r"1e-3")
  case object ZeptoLitre extends IntrinsicVolumeUnit("ZeptoLitre", Seq("zL"), r"1e-21" * r"1e-3")
  case object AttoLitre extends IntrinsicVolumeUnit("AttoLitre", Seq("aL"), r"1e-18" * r"1e-3")
  case object FemtoLitre extends IntrinsicVolumeUnit("FemtoLitre", Seq("fL"), r"1e-15" * r"1e-3")
  case object PicoLitre extends IntrinsicVolumeUnit("PicoLitre", Seq("pL"), r"1e-12" * r"1e-3")
  case object NanoLitre extends IntrinsicVolumeUnit("NanoLitre", Seq("nL"), r"1e-9" * r"1e-3")
  case object MicroLitre extends IntrinsicVolumeUnit("MicroLitre", Seq("microLitre", "microL", "μL"), r"1e-6" * r"1e-3")
  case object MilliLitre extends IntrinsicVolumeUnit("MilliLitre", Seq("mL"), r"1e-3" * r"1e-3")
  case object CentiLitre extends IntrinsicVolumeUnit("CentiLitre", Seq("cL"), r"1e-2" * r"1e-3")
  case object DeciLitre extends IntrinsicVolumeUnit("DeciLitre", Seq("dL"), r"1e-1" * r"1e-3")
  case object Litre extends IntrinsicVolumeUnit("Litre", Seq("L"), r"1" * r"1e-3")
  case object DecaLitre extends IntrinsicVolumeUnit("DecaLitre", Seq("daL"), r"1e1" * r"1e-3")
  case object HectoLitre extends IntrinsicVolumeUnit("HectoLitre", Seq("hL"), r"1e2" * r"1e-3")
  case object KiloLitre extends IntrinsicVolumeUnit("KiloLitre", Seq("kL"), r"1e3" * r"1e-3")
  case object MegaLitre extends IntrinsicVolumeUnit("MegaLitre", Seq("ML"), r"1e6" * r"1e-3")
  case object GigaLitre extends IntrinsicVolumeUnit("GigaLitre", Seq("GL"), r"1e9" * r"1e-3")
  case object TeraLitre extends IntrinsicVolumeUnit("TeraLitre", Seq("TL"), r"1e12" * r"1e-3")
  case object PetaLitre extends IntrinsicVolumeUnit("PetaLitre", Seq("PL"), r"1e15" * r"1e-3")
  case object ExaLitre extends IntrinsicVolumeUnit("ExaLitre", Seq("EL"), r"1e18" * r"1e-3")
  case object ZettaLitre extends IntrinsicVolumeUnit("ZettaLitre", Seq("ZL"), r"1e21" * r"1e-3")
  case object YottaLitre extends IntrinsicVolumeUnit("YottaLitre", Seq("YL"), r"1e24" * r"1e-3")
  case object Lambda extends IntrinsicVolumeUnit("Lambda", Seq("λ", "lambda"), r"1e-9")
  case object CubicInch extends IntrinsicVolumeUnit("CubicInch", Seq("in3", "cu_in"), LengthUnit.Inch.cubic)
  case object CubicFoot extends IntrinsicVolumeUnit("CubicFoot", Seq("ft3", "cu_ft"), LengthUnit.Foot.cubic)
  case object CubicYard extends IntrinsicVolumeUnit("CubicYard", Seq("yd3", "cu_yd"), LengthUnit.Yard.cubic)
  case object CubicFathom extends IntrinsicVolumeUnit("CubicFathom", Seq("ftm3", "cu_fm"), LengthUnit.Fathom.cubic)
  case object CubicMile extends IntrinsicVolumeUnit("CubicMile", Seq("mi3", "cu_mi"), LengthUnit.Mile.cubic)
  case object BoardFoot extends IntrinsicVolumeUnit("BoardFoot", Seq("fbm"), 144, CubicInch)
  case object Gallon_beer extends IntrinsicVolumeUnit("Gallon_beer", Seq("beer_gal"), 282, CubicInch)
  case object Perch extends IntrinsicVolumeUnit("Perch", Seq("per"), r"33/2" * r"3/2", CubicFoot)
  case object Barrel extends IntrinsicVolumeUnit("Barrel", Seq("bl", "bbl"), 42, Gallon_US_fluid)
  case object Minim_US extends IntrinsicVolumeUnit("Minim_US", Seq("minim(US)"), r"1/480", Fluid_Ounce_US)
  case object Fluid_Ounce_US extends IntrinsicVolumeUnit("Fluid_Ounce_US", Seq("US_fl_oz", "fl_oz(US)"), r"1/128", Gallon_US_fluid)
  case object Gill_US extends IntrinsicVolumeUnit("Gill_US", Seq("gi(US)"), 4, Fluid_Ounce_US)
  case object Hogshead_US extends IntrinsicVolumeUnit("Hogshead_US", Seq("hhd(US)"), 2, Barrel_US_fluid)
  case object FluidDram_US extends IntrinsicVolumeUnit("FluidDram_US", Seq("fl_dr(US)"), r"1/8", Fluid_Ounce_US)
  case object Pint_US_fluid extends IntrinsicVolumeUnit("Pint_US_fluid", Seq("pt(US_fl)"), r"1/8", Gallon_US_fluid)
  case object Quart_US_fluid extends IntrinsicVolumeUnit("Quart_US_fluid", Seq("qt(US_fl)"), r"1/4", Gallon_US_fluid)
  case object Gallon_US_fluid extends IntrinsicVolumeUnit("Gallon_US_fluid", Seq("gal", "US_gal", "gal(US)", "gal(US_fl)"), 231, CubicInch)
  case object Barrel_US_fluid extends IntrinsicVolumeUnit("Barrel_US_fluid", Seq("bl(US_fl)", "fl_bl", "fl_bl(US)"), r"31.5", Gallon_US_fluid)
  case object Pint_US_dry extends IntrinsicVolumeUnit("Pint_US_dry", Seq("pt(US_dry)"), r"1/8", Gallon_US_dry)
  case object Quart_US_dry extends IntrinsicVolumeUnit("Quart_US_dry", Seq("qt(US_dry)"), r"1/4", Gallon_US_dry)
  case object Gallon_US_dry extends IntrinsicVolumeUnit("Gallon_US_dry", Seq("gal(US_dry)"), r"1/8", Bushel_US_dry_level)
  case object Peck_US_dry extends IntrinsicVolumeUnit("Peck_US_dry", Seq("pk(US_dry)"), r"1/4", Bushel_US_dry_level)
  case object Bushel_US_dry extends IntrinsicVolumeUnit("Bushel_US_dry", Seq("bu(US_dry)"), r"5/4", Bushel_US_dry_level)
  case object Barrel_US_dry extends IntrinsicVolumeUnit("Barrel_US_dry", Seq("bl(US_dry)"), 105, Quart_US_dry)
  case object Bushel_US_dry_level extends IntrinsicVolumeUnit("Bushel_US_dry_level", Seq("bu(US_lvl)"), r"2150.42", CubicInch)
  case object Minim_imperial extends IntrinsicVolumeUnit("Minim_imperial", Seq("minim(imp)"), r"1/480", Fluid_Ounce_imperial)
  case object Fluid_Ounce_imperial extends IntrinsicVolumeUnit("Fluid_Ounce_imperial", Seq("fl_oz(imp)"), r"1/160", Gallon_imperial)
  case object Gill_imperial extends IntrinsicVolumeUnit("Gill_imperial", Seq("gi(imp)", "nog"), 5, Fluid_Ounce_imperial)
  case object Pint_imperial extends IntrinsicVolumeUnit("Pint_imperial", Seq("pt(imp)"), r"1/8", Gallon_imperial)
  case object Quart_imperial extends IntrinsicVolumeUnit("Quart_imperial", Seq("qt(imp)"), r"1/4", Gallon_imperial)
  case object Gallon_imperial extends IntrinsicVolumeUnit("Gallon_imperial", Seq("imp_gal", "gal(imp)"), r"4.54609", Litre)
  case object Peck_imperial extends IntrinsicVolumeUnit("Peck_imperial", Seq("pk(imp)"), 2, Gallon_imperial)
  case object Bushel_imperial extends IntrinsicVolumeUnit("Bushel_imperial", Seq("bu(imp)"), 8, Gallon_imperial)
  case object Barrel_imperial extends IntrinsicVolumeUnit("Barrel_imperial", Seq("bl(imp)"), 36, Gallon_imperial)
  case object Hogshead_imperial extends IntrinsicVolumeUnit("Hogshead_imperial", Seq("hhd(imp)"), 2, Barrel_imperial)
  case object FluidScruple extends IntrinsicVolumeUnit("FluidScruple", Seq("fl_s"), r"1/24", Fluid_Ounce_imperial)
  case object FluidDrachm_imperial extends IntrinsicVolumeUnit("FluidDrachm_imperial", Seq("fl_dr(imp)"), r"1/8", Fluid_Ounce_imperial)
  case object Bucket extends IntrinsicVolumeUnit("Bucket", Seq("bkt"), 4, Gallon_imperial)

  override lazy val values = Seq(YoctoCubicMetre, ZeptoCubicMetre, AttoCubicMetre, FemtoCubicMetre, PicoCubicMetre, NanoCubicMetre, MicroCubicMetre, MilliCubicMetre, CentiCubicMetre, DeciCubicMetre, CubicMetre, DecaCubicMetre, HectoCubicMetre, KiloCubicMetre, MegaCubicMetre, GigaCubicMetre, TeraCubicMetre, PetaCubicMetre, ExaCubicMetre, ZettaCubicMetre, YottaCubicMetre, YoctoLitre, ZeptoLitre, AttoLitre, FemtoLitre, PicoLitre, NanoLitre, MicroLitre, MilliLitre, CentiLitre, DeciLitre, Litre, DecaLitre, HectoLitre, KiloLitre, MegaLitre, GigaLitre, TeraLitre, PetaLitre, ExaLitre, ZettaLitre, YottaLitre, Lambda, CubicInch, CubicFoot, CubicYard, CubicFathom, CubicMile, BoardFoot, Gallon_beer, Perch, Barrel, Minim_US, Fluid_Ounce_US, Gill_US, Hogshead_US, FluidDram_US, Pint_US_fluid, Quart_US_fluid, Gallon_US_fluid, Barrel_US_fluid, Pint_US_dry, Quart_US_dry, Gallon_US_dry, Peck_US_dry, Bushel_US_dry, Barrel_US_dry, Bushel_US_dry_level, Minim_imperial, Fluid_Ounce_imperial, Gill_imperial, Pint_imperial, Quart_imperial, Gallon_imperial, Peck_imperial, Bushel_imperial, Barrel_imperial, Hogshead_imperial, FluidScruple, FluidDrachm_imperial, Bucket)

  // AreaUnit * LengthUnit -> Volume
  private[VolumeUnit]
  class ProductAreaDotLengthUnit(val firstUnit: AreaUnit, val secondUnit: LengthUnit)
      extends VolumeUnit with ProductUnit[VolumeUnit, AreaUnit, LengthUnit]{

    override lazy val unitInCubicMetre: Real =
      firstUnit.valueInBaseUnit * secondUnit.valueInBaseUnit
  }

  def apply(unit1: AreaUnit, unit2: LengthUnit): VolumeUnit =
    new ProductAreaDotLengthUnit(unit1, unit2)
}

trait MultiplicativeByVolumeUnit[R]{
  def *(unit: VolumeUnit): R
}

trait DivisibleByVolumeUnit[R]{
  def /(unit: VolumeUnit): R
}

trait VolumePostfixOps[A]{
  import VolumeUnit._

  protected def volumePostfixOps(unit: VolumeUnit): A


  def ym3 : A = volumePostfixOps(YoctoCubicMetre)
  def zm3 : A = volumePostfixOps(ZeptoCubicMetre)
  def am3 : A = volumePostfixOps(AttoCubicMetre)
  def fm3 : A = volumePostfixOps(FemtoCubicMetre)
  def pm3 : A = volumePostfixOps(PicoCubicMetre)
  def nm3 : A = volumePostfixOps(NanoCubicMetre)
  def microCubicMetre : A = volumePostfixOps(MicroCubicMetre)
  def microM3 : A = volumePostfixOps(MicroCubicMetre)
  def μm3 : A = volumePostfixOps(MicroCubicMetre)
  def mm3 : A = volumePostfixOps(MilliCubicMetre)
  def cm3 : A = volumePostfixOps(CentiCubicMetre)
  def dm3 : A = volumePostfixOps(DeciCubicMetre)
  def m3 : A = volumePostfixOps(CubicMetre)
  def dam3 : A = volumePostfixOps(DecaCubicMetre)
  def hm3 : A = volumePostfixOps(HectoCubicMetre)
  def km3 : A = volumePostfixOps(KiloCubicMetre)
  def Mm3 : A = volumePostfixOps(MegaCubicMetre)
  def Gm3 : A = volumePostfixOps(GigaCubicMetre)
  def Tm3 : A = volumePostfixOps(TeraCubicMetre)
  def Pm3 : A = volumePostfixOps(PetaCubicMetre)
  def Em3 : A = volumePostfixOps(ExaCubicMetre)
  def Zm3 : A = volumePostfixOps(ZettaCubicMetre)
  def Ym3 : A = volumePostfixOps(YottaCubicMetre)
  def yL : A = volumePostfixOps(YoctoLitre)
  def zL : A = volumePostfixOps(ZeptoLitre)
  def aL : A = volumePostfixOps(AttoLitre)
  def fL : A = volumePostfixOps(FemtoLitre)
  def pL : A = volumePostfixOps(PicoLitre)
  def nL : A = volumePostfixOps(NanoLitre)
  def microLitre : A = volumePostfixOps(MicroLitre)
  def microL : A = volumePostfixOps(MicroLitre)
  def μL : A = volumePostfixOps(MicroLitre)
  def mL : A = volumePostfixOps(MilliLitre)
  def cL : A = volumePostfixOps(CentiLitre)
  def dL : A = volumePostfixOps(DeciLitre)
  def L : A = volumePostfixOps(Litre)
  def daL : A = volumePostfixOps(DecaLitre)
  def hL : A = volumePostfixOps(HectoLitre)
  def kL : A = volumePostfixOps(KiloLitre)
  def ML : A = volumePostfixOps(MegaLitre)
  def GL : A = volumePostfixOps(GigaLitre)
  def TL : A = volumePostfixOps(TeraLitre)
  def PL : A = volumePostfixOps(PetaLitre)
  def EL : A = volumePostfixOps(ExaLitre)
  def ZL : A = volumePostfixOps(ZettaLitre)
  def YL : A = volumePostfixOps(YottaLitre)
  def λ : A = volumePostfixOps(Lambda)
  def lambda : A = volumePostfixOps(Lambda)
  def in3 : A = volumePostfixOps(CubicInch)
  def cu_in : A = volumePostfixOps(CubicInch)
  def ft3 : A = volumePostfixOps(CubicFoot)
  def cu_ft : A = volumePostfixOps(CubicFoot)
  def yd3 : A = volumePostfixOps(CubicYard)
  def cu_yd : A = volumePostfixOps(CubicYard)
  def ftm3 : A = volumePostfixOps(CubicFathom)
  def cu_fm : A = volumePostfixOps(CubicFathom)
  def mi3 : A = volumePostfixOps(CubicMile)
  def cu_mi : A = volumePostfixOps(CubicMile)
  def fbm : A = volumePostfixOps(BoardFoot)
  def beer_gal : A = volumePostfixOps(Gallon_beer)
  def per : A = volumePostfixOps(Perch)
  def bl : A = volumePostfixOps(Barrel)
  def bbl : A = volumePostfixOps(Barrel)
  def US_fl_oz : A = volumePostfixOps(Fluid_Ounce_US)
  def gal : A = volumePostfixOps(Gallon_US_fluid)
  def US_gal : A = volumePostfixOps(Gallon_US_fluid)
  def fl_bl : A = volumePostfixOps(Barrel_US_fluid)
  def nog : A = volumePostfixOps(Gill_imperial)
  def imp_gal : A = volumePostfixOps(Gallon_imperial)
  def fl_s : A = volumePostfixOps(FluidScruple)
  def bkt : A = volumePostfixOps(Bucket)

  import VolumePostfixOps._
  import org.waman.multiverse.metric.MetricContext
  import MetricContext._

  def minim(c: MetricContext): A = volumePostfixOps(_minim(c))
  def fl_oz(c: MetricContext): A = volumePostfixOps(_fl_oz(c))
  def gi(c: MetricContext): A = volumePostfixOps(_gi(c))
  def hhd(c: MetricContext): A = volumePostfixOps(_hhd(c))
  def fl_dr(c: MetricContext): A = volumePostfixOps(_fl_dr(c))
  def pt(c: MetricContext): A = volumePostfixOps(_pt(c))
  def qt(c: MetricContext): A = volumePostfixOps(_qt(c))
  def gal(c: MetricContext): A = volumePostfixOps(_gal(c))
  def bl(c: MetricContext): A = volumePostfixOps(_bl(c))
  def fl_bl(c: MetricContext): A = volumePostfixOps(_fl_bl(c))
  def pk(c: MetricContext): A = volumePostfixOps(_pk(c))
  def bu(c: MetricContext): A = volumePostfixOps(_bu(c))
}

object VolumePostfixOps{
  import VolumeUnit._
  import org.waman.multiverse.metric.MetricContext
  import MetricContext._


  lazy val _fl_bl : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates => Barrel_US_fluid
  }

  lazy val _fl_oz : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates => Fluid_Ounce_US
    case Imperial => Fluid_Ounce_imperial
  }

  lazy val _minim : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates => Minim_US
    case Imperial => Minim_imperial
  }

  lazy val _hhd : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates => Hogshead_US
    case Imperial => Hogshead_imperial
  }

  lazy val _pt : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates_Fluid => Pint_US_fluid
    case UnitedStates_Dry => Pint_US_dry
    case Imperial => Pint_imperial
  }

  lazy val _fl_dr : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates => FluidDram_US
    case Imperial => FluidDrachm_imperial
  }

  lazy val _qt : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates_Fluid => Quart_US_fluid
    case UnitedStates_Dry => Quart_US_dry
    case Imperial => Quart_imperial
  }

  lazy val _pk : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates_Dry => Peck_US_dry
    case Imperial => Peck_imperial
  }

  lazy val _bu : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates_Dry => Bushel_US_dry
    case UnitedStates_Dry_Level => Bushel_US_dry_level
    case Imperial => Bushel_imperial
  }

  lazy val _gi : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates => Gill_US
    case Imperial => Gill_imperial
  }

  lazy val _bl : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates_Fluid => Barrel_US_fluid
    case UnitedStates_Dry => Barrel_US_dry
    case Imperial => Barrel_imperial
  }

  lazy val _gal : PartialFunction[MetricContext, VolumeUnit] = {
    case UnitedStates => Gallon_US_fluid
    case UnitedStates_Fluid => Gallon_US_fluid
    case UnitedStates_Dry => Gallon_US_dry
    case Imperial => Gallon_imperial
  }
}

trait VolumeDot[A]{
  import VolumeUnit._

  protected def volumeDot(unit: VolumeUnit): A

  def ym3(dot: Dot): A = volumeDot(YoctoCubicMetre)
  def zm3(dot: Dot): A = volumeDot(ZeptoCubicMetre)
  def am3(dot: Dot): A = volumeDot(AttoCubicMetre)
  def fm3(dot: Dot): A = volumeDot(FemtoCubicMetre)
  def pm3(dot: Dot): A = volumeDot(PicoCubicMetre)
  def nm3(dot: Dot): A = volumeDot(NanoCubicMetre)
  def microCubicMetre(dot: Dot): A = volumeDot(MicroCubicMetre)
  def microM3(dot: Dot): A = volumeDot(MicroCubicMetre)
  def μm3(dot: Dot): A = volumeDot(MicroCubicMetre)
  def mm3(dot: Dot): A = volumeDot(MilliCubicMetre)
  def cm3(dot: Dot): A = volumeDot(CentiCubicMetre)
  def dm3(dot: Dot): A = volumeDot(DeciCubicMetre)
  def m3(dot: Dot): A = volumeDot(CubicMetre)
  def dam3(dot: Dot): A = volumeDot(DecaCubicMetre)
  def hm3(dot: Dot): A = volumeDot(HectoCubicMetre)
  def km3(dot: Dot): A = volumeDot(KiloCubicMetre)
  def Mm3(dot: Dot): A = volumeDot(MegaCubicMetre)
  def Gm3(dot: Dot): A = volumeDot(GigaCubicMetre)
  def Tm3(dot: Dot): A = volumeDot(TeraCubicMetre)
  def Pm3(dot: Dot): A = volumeDot(PetaCubicMetre)
  def Em3(dot: Dot): A = volumeDot(ExaCubicMetre)
  def Zm3(dot: Dot): A = volumeDot(ZettaCubicMetre)
  def Ym3(dot: Dot): A = volumeDot(YottaCubicMetre)
  def yL(dot: Dot): A = volumeDot(YoctoLitre)
  def zL(dot: Dot): A = volumeDot(ZeptoLitre)
  def aL(dot: Dot): A = volumeDot(AttoLitre)
  def fL(dot: Dot): A = volumeDot(FemtoLitre)
  def pL(dot: Dot): A = volumeDot(PicoLitre)
  def nL(dot: Dot): A = volumeDot(NanoLitre)
  def microLitre(dot: Dot): A = volumeDot(MicroLitre)
  def microL(dot: Dot): A = volumeDot(MicroLitre)
  def μL(dot: Dot): A = volumeDot(MicroLitre)
  def mL(dot: Dot): A = volumeDot(MilliLitre)
  def cL(dot: Dot): A = volumeDot(CentiLitre)
  def dL(dot: Dot): A = volumeDot(DeciLitre)
  def L(dot: Dot): A = volumeDot(Litre)
  def daL(dot: Dot): A = volumeDot(DecaLitre)
  def hL(dot: Dot): A = volumeDot(HectoLitre)
  def kL(dot: Dot): A = volumeDot(KiloLitre)
  def ML(dot: Dot): A = volumeDot(MegaLitre)
  def GL(dot: Dot): A = volumeDot(GigaLitre)
  def TL(dot: Dot): A = volumeDot(TeraLitre)
  def PL(dot: Dot): A = volumeDot(PetaLitre)
  def EL(dot: Dot): A = volumeDot(ExaLitre)
  def ZL(dot: Dot): A = volumeDot(ZettaLitre)
  def YL(dot: Dot): A = volumeDot(YottaLitre)
  def λ(dot: Dot): A = volumeDot(Lambda)
  def lambda(dot: Dot): A = volumeDot(Lambda)
  def in3(dot: Dot): A = volumeDot(CubicInch)
  def cu_in(dot: Dot): A = volumeDot(CubicInch)
  def ft3(dot: Dot): A = volumeDot(CubicFoot)
  def cu_ft(dot: Dot): A = volumeDot(CubicFoot)
  def yd3(dot: Dot): A = volumeDot(CubicYard)
  def cu_yd(dot: Dot): A = volumeDot(CubicYard)
  def ftm3(dot: Dot): A = volumeDot(CubicFathom)
  def cu_fm(dot: Dot): A = volumeDot(CubicFathom)
  def mi3(dot: Dot): A = volumeDot(CubicMile)
  def cu_mi(dot: Dot): A = volumeDot(CubicMile)
  def fbm(dot: Dot): A = volumeDot(BoardFoot)
  def beer_gal(dot: Dot): A = volumeDot(Gallon_beer)
  def per(dot: Dot): A = volumeDot(Perch)
  def bl(dot: Dot): A = volumeDot(Barrel)
  def bbl(dot: Dot): A = volumeDot(Barrel)
  def US_fl_oz(dot: Dot): A = volumeDot(Fluid_Ounce_US)
  def gal(dot: Dot): A = volumeDot(Gallon_US_fluid)
  def US_gal(dot: Dot): A = volumeDot(Gallon_US_fluid)
  def fl_bl(dot: Dot): A = volumeDot(Barrel_US_fluid)
  def nog(dot: Dot): A = volumeDot(Gill_imperial)
  def imp_gal(dot: Dot): A = volumeDot(Gallon_imperial)
  def fl_s(dot: Dot): A = volumeDot(FluidScruple)
  def bkt(dot: Dot): A = volumeDot(Bucket)
}

trait VolumePer[A]{
  import VolumeUnit._

  protected def volumePer(unit: VolumeUnit): A

  def ym3(per: Per): A = volumePer(YoctoCubicMetre)
  def zm3(per: Per): A = volumePer(ZeptoCubicMetre)
  def am3(per: Per): A = volumePer(AttoCubicMetre)
  def fm3(per: Per): A = volumePer(FemtoCubicMetre)
  def pm3(per: Per): A = volumePer(PicoCubicMetre)
  def nm3(per: Per): A = volumePer(NanoCubicMetre)
  def microCubicMetre(per: Per): A = volumePer(MicroCubicMetre)
  def microM3(per: Per): A = volumePer(MicroCubicMetre)
  def μm3(per: Per): A = volumePer(MicroCubicMetre)
  def mm3(per: Per): A = volumePer(MilliCubicMetre)
  def cm3(per: Per): A = volumePer(CentiCubicMetre)
  def dm3(per: Per): A = volumePer(DeciCubicMetre)
  def m3(per: Per): A = volumePer(CubicMetre)
  def dam3(per: Per): A = volumePer(DecaCubicMetre)
  def hm3(per: Per): A = volumePer(HectoCubicMetre)
  def km3(per: Per): A = volumePer(KiloCubicMetre)
  def Mm3(per: Per): A = volumePer(MegaCubicMetre)
  def Gm3(per: Per): A = volumePer(GigaCubicMetre)
  def Tm3(per: Per): A = volumePer(TeraCubicMetre)
  def Pm3(per: Per): A = volumePer(PetaCubicMetre)
  def Em3(per: Per): A = volumePer(ExaCubicMetre)
  def Zm3(per: Per): A = volumePer(ZettaCubicMetre)
  def Ym3(per: Per): A = volumePer(YottaCubicMetre)
  def yL(per: Per): A = volumePer(YoctoLitre)
  def zL(per: Per): A = volumePer(ZeptoLitre)
  def aL(per: Per): A = volumePer(AttoLitre)
  def fL(per: Per): A = volumePer(FemtoLitre)
  def pL(per: Per): A = volumePer(PicoLitre)
  def nL(per: Per): A = volumePer(NanoLitre)
  def microLitre(per: Per): A = volumePer(MicroLitre)
  def microL(per: Per): A = volumePer(MicroLitre)
  def μL(per: Per): A = volumePer(MicroLitre)
  def mL(per: Per): A = volumePer(MilliLitre)
  def cL(per: Per): A = volumePer(CentiLitre)
  def dL(per: Per): A = volumePer(DeciLitre)
  def L(per: Per): A = volumePer(Litre)
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
  def lambda(per: Per): A = volumePer(Lambda)
  def in3(per: Per): A = volumePer(CubicInch)
  def cu_in(per: Per): A = volumePer(CubicInch)
  def ft3(per: Per): A = volumePer(CubicFoot)
  def cu_ft(per: Per): A = volumePer(CubicFoot)
  def yd3(per: Per): A = volumePer(CubicYard)
  def cu_yd(per: Per): A = volumePer(CubicYard)
  def ftm3(per: Per): A = volumePer(CubicFathom)
  def cu_fm(per: Per): A = volumePer(CubicFathom)
  def mi3(per: Per): A = volumePer(CubicMile)
  def cu_mi(per: Per): A = volumePer(CubicMile)
  def fbm(per: Per): A = volumePer(BoardFoot)
  def beer_gal(per: Per): A = volumePer(Gallon_beer)
  def per(per: Per): A = volumePer(Perch)
  def bl(per: Per): A = volumePer(Barrel)
  def bbl(per: Per): A = volumePer(Barrel)
  def US_fl_oz(per: Per): A = volumePer(Fluid_Ounce_US)
  def gal(per: Per): A = volumePer(Gallon_US_fluid)
  def US_gal(per: Per): A = volumePer(Gallon_US_fluid)
  def fl_bl(per: Per): A = volumePer(Barrel_US_fluid)
  def nog(per: Per): A = volumePer(Gill_imperial)
  def imp_gal(per: Per): A = volumePer(Gallon_imperial)
  def fl_s(per: Per): A = volumePer(FluidScruple)
  def bkt(per: Per): A = volumePer(Bucket)
}

trait PredefinedVolumeUnit extends VolumePostfixOps[VolumeUnit]{
  override protected def volumePostfixOps(unit: VolumeUnit) = unit
  
}

object PredefinedVolumeUnit extends PredefinedVolumeUnit
