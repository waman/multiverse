package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._


import org.waman.multiverse.unit.fluid.VolumeFlow
import org.waman.multiverse.unit.fluid.VolumeFlowUnit


class Volume[A: Fractional](val value: A, val unit: VolumeUnit)
    extends LinearQuantity[Volume[A], A, VolumeUnit] {

  override protected def newQuantity(value: A, unit: VolumeUnit): Volume[A] = new Volume(value, unit)

  def /(time: Time[A]): VolumeFlow[A] = new VolumeFlow(this.value / time.value, this.unit / time.unit)

}

trait VolumeUnit extends LinearUnit[VolumeUnit]{

  override def getSIUnit: VolumeUnit = VolumeUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = VolumeUnit.dimension

  def /(timeUnit: TimeUnit): VolumeFlowUnit =
    new AbstractQuotientUnit[VolumeFlowUnit, VolumeUnit, TimeUnit](VolumeUnit.this, timeUnit) with VolumeFlowUnit

}

object VolumeUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](L -> 3).withDefaultValue(0)

  val getSIUnit: VolumeUnit = AreaUnit.getSIUnit * LengthUnit.getSIUnit

  import VolumeUnitObjects._
  def getUnits: Seq[VolumeUnit] =
    Seq(cubic_metre, cubic_yoctometre, cubic_zeptometre, cubic_attometre, cubic_femtometre, cubic_picometre, cubic_nanometre, cubic_micrometre, cubic_millimetre, cubic_centimetre, cubic_decimetre, cubic_decametre, cubic_hectometre, cubic_kilometre, cubic_megametre, cubic_gigametre, cubic_terametre, cubic_petametre, cubic_exametre, cubic_zettametre, cubic_yottametre, litre, yoctolitre, zeptolitre, attolitre, femtolitre, picolitre, nanolitre, microlitre, millilitre, centilitre, decilitre, decalitre, hectolitre, kilolitre, megalitre, gigalitre, teralitre, petalitre, exalitre, zettalitre, yottalitre, lambda, cubic_inch, cubic_foot, cubic_yard, cubic_fathom, cubic_mile, board_foot, gallon_beer, perch, minim, `minim(US)`, `minim(imp)`, fluid_ounce, `fluid_ounce(US)`, `fluid_ounce(imp)`, gill, `gill(US)`, `gill(imp)`, pint, `pint(US_fl)`, `pint(US_dry)`, `pint(imp)`, quart, `quart(US_fl)`, `quart(US_dry)`, `quart(imp)`, gallon, `gallon(US)`, `gallon(US_fl)`, `gallon(US_dry)`, `gallon(imp)`, peck, `peck(US_dry)`, `peck(imp)`, bushel, `bushel(US)`, `bushel(US_lvl)`, `bushel(imp)`, barrel, `barrel(US_fl)`, `barrel(US_dry)`, `barrel(imp)`, fluid_barrel, hogshead, `hogshead(US)`, `hogshead(imp)`, fluid_dram, `fluid_dram(US)`, `fluid_dram(imp)`, fluid_scruple, bucket)
}

sealed trait minimAttribute
sealed trait fluid_ounceAttribute
sealed trait gillAttribute
sealed trait pintAttribute
sealed trait quartAttribute
sealed trait gallonAttribute
sealed trait peckAttribute
sealed trait bushelAttribute
sealed trait barrelAttribute
sealed trait hogsheadAttribute
sealed trait fluid_dramAttribute

object VolumeAttributes{
  final object US extends minimAttribute with fluid_ounceAttribute with gillAttribute with gallonAttribute with bushelAttribute with hogsheadAttribute with fluid_dramAttribute
  final object US_fl extends pintAttribute with quartAttribute with gallonAttribute with barrelAttribute
  final object US_dry extends pintAttribute with quartAttribute with gallonAttribute with peckAttribute with barrelAttribute
  final object imp extends minimAttribute with fluid_ounceAttribute with gillAttribute with pintAttribute with quartAttribute with gallonAttribute with peckAttribute with bushelAttribute with barrelAttribute with hogsheadAttribute with fluid_dramAttribute
  final object US_lvl extends bushelAttribute
}

class DefaultVolumeUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VolumeUnit

object VolumeUnitObjects{

  final object cubic_metre extends DefaultVolumeUnit("cubic metre", "m³", Seq("m3"), LengthUnitObjects.metre.interval**3)
  final object cubic_yoctometre extends DefaultVolumeUnit("cubic yoctometre", "ym³", Seq("ym3"), LengthUnitObjects.yoctometre.interval**3)
  final object cubic_zeptometre extends DefaultVolumeUnit("cubic zeptometre", "zm³", Seq("zm3"), LengthUnitObjects.zeptometre.interval**3)
  final object cubic_attometre extends DefaultVolumeUnit("cubic attometre", "am³", Seq("am3"), LengthUnitObjects.attometre.interval**3)
  final object cubic_femtometre extends DefaultVolumeUnit("cubic femtometre", "fm³", Seq("fm3"), LengthUnitObjects.femtometre.interval**3)
  final object cubic_picometre extends DefaultVolumeUnit("cubic picometre", "pm³", Seq("pm3"), LengthUnitObjects.picometre.interval**3)
  final object cubic_nanometre extends DefaultVolumeUnit("cubic nanometre", "nm³", Seq("nm3"), LengthUnitObjects.nanometre.interval**3)
  final object cubic_micrometre extends DefaultVolumeUnit("cubic micrometre", "μm³", Seq("μm3", "mcm³", "mcm3"), LengthUnitObjects.micrometre.interval**3)
  final object cubic_millimetre extends DefaultVolumeUnit("cubic millimetre", "mm³", Seq("mm3"), LengthUnitObjects.millimetre.interval**3)
  final object cubic_centimetre extends DefaultVolumeUnit("cubic centimetre", "cm³", Seq("cm3"), LengthUnitObjects.centimetre.interval**3)
  final object cubic_decimetre extends DefaultVolumeUnit("cubic decimetre", "dm³", Seq("dm3"), LengthUnitObjects.decimetre.interval**3)
  final object cubic_decametre extends DefaultVolumeUnit("cubic decametre", "dam³", Seq("dam3"), LengthUnitObjects.decametre.interval**3)
  final object cubic_hectometre extends DefaultVolumeUnit("cubic hectometre", "hm³", Seq("hm3"), LengthUnitObjects.hectometre.interval**3)
  final object cubic_kilometre extends DefaultVolumeUnit("cubic kilometre", "km³", Seq("km3", "Km³", "Km3"), LengthUnitObjects.kilometre.interval**3)
  final object cubic_megametre extends DefaultVolumeUnit("cubic megametre", "Mm³", Seq("Mm3"), LengthUnitObjects.megametre.interval**3)
  final object cubic_gigametre extends DefaultVolumeUnit("cubic gigametre", "Gm³", Seq("Gm3"), LengthUnitObjects.gigametre.interval**3)
  final object cubic_terametre extends DefaultVolumeUnit("cubic terametre", "Tm³", Seq("Tm3"), LengthUnitObjects.terametre.interval**3)
  final object cubic_petametre extends DefaultVolumeUnit("cubic petametre", "Pm³", Seq("Pm3"), LengthUnitObjects.petametre.interval**3)
  final object cubic_exametre extends DefaultVolumeUnit("cubic exametre", "Em³", Seq("Em3"), LengthUnitObjects.exametre.interval**3)
  final object cubic_zettametre extends DefaultVolumeUnit("cubic zettametre", "Zm³", Seq("Zm3"), LengthUnitObjects.zettametre.interval**3)
  final object cubic_yottametre extends DefaultVolumeUnit("cubic yottametre", "Ym³", Seq("Ym3"), LengthUnitObjects.yottametre.interval**3)
  final object litre extends DefaultVolumeUnit("litre", "L", Nil, r"1e-3")
  final object yoctolitre extends DefaultVolumeUnit("yoctolitre", "yL", Nil, r"1e-3" * r"1e-24")
  final object zeptolitre extends DefaultVolumeUnit("zeptolitre", "zL", Nil, r"1e-3" * r"1e-21")
  final object attolitre extends DefaultVolumeUnit("attolitre", "aL", Nil, r"1e-3" * r"1e-18")
  final object femtolitre extends DefaultVolumeUnit("femtolitre", "fL", Nil, r"1e-3" * r"1e-15")
  final object picolitre extends DefaultVolumeUnit("picolitre", "pL", Nil, r"1e-3" * r"1e-12")
  final object nanolitre extends DefaultVolumeUnit("nanolitre", "nL", Nil, r"1e-3" * r"1e-9")
  final object microlitre extends DefaultVolumeUnit("microlitre", "μL", Seq("mcL"), r"1e-3" * r"1e-6")
  final object millilitre extends DefaultVolumeUnit("millilitre", "mL", Nil, r"1e-3" * r"1e-3")
  final object centilitre extends DefaultVolumeUnit("centilitre", "cL", Nil, r"1e-3" * r"1e-2")
  final object decilitre extends DefaultVolumeUnit("decilitre", "dL", Nil, r"1e-3" * r"1e-1")
  final object decalitre extends DefaultVolumeUnit("decalitre", "daL", Nil, r"1e-3" * r"1e1")
  final object hectolitre extends DefaultVolumeUnit("hectolitre", "hL", Nil, r"1e-3" * r"1e2")
  final object kilolitre extends DefaultVolumeUnit("kilolitre", "kL", Seq("KL"), r"1e-3" * r"1e3")
  final object megalitre extends DefaultVolumeUnit("megalitre", "ML", Nil, r"1e-3" * r"1e6")
  final object gigalitre extends DefaultVolumeUnit("gigalitre", "GL", Nil, r"1e-3" * r"1e9")
  final object teralitre extends DefaultVolumeUnit("teralitre", "TL", Nil, r"1e-3" * r"1e12")
  final object petalitre extends DefaultVolumeUnit("petalitre", "PL", Nil, r"1e-3" * r"1e15")
  final object exalitre extends DefaultVolumeUnit("exalitre", "EL", Nil, r"1e-3" * r"1e18")
  final object zettalitre extends DefaultVolumeUnit("zettalitre", "ZL", Nil, r"1e-3" * r"1e21")
  final object yottalitre extends DefaultVolumeUnit("yottalitre", "YL", Nil, r"1e-3" * r"1e24")
  final object lambda extends DefaultVolumeUnit("lambda", "λ", Nil, r"1e-9")
  final object cubic_inch extends DefaultVolumeUnit("cubic inch", "in³", Seq("in3", "cu_in"), LengthUnitObjects.inch.interval**3)
  final object cubic_foot extends DefaultVolumeUnit("cubic foot", "ft³", Seq("ft3", "cu_ft"), LengthUnitObjects.foot.interval**3)
  final object cubic_yard extends DefaultVolumeUnit("cubic yard", "yd³", Seq("yd3", "cu_yd"), LengthUnitObjects.yard.interval**3)
  final object cubic_fathom extends DefaultVolumeUnit("cubic fathom", "ftm³", Seq("ftm3", "cu_fm"), LengthUnitObjects.fathom.interval**3)
  final object cubic_mile extends DefaultVolumeUnit("cubic mile", "mi³", Seq("mi3", "cu_mi"), LengthUnitObjects.mile.interval**3)
  final object board_foot extends DefaultVolumeUnit("board foot", "fbm", Nil, r"144" * cubic_inch.interval)
  final object gallon_beer extends DefaultVolumeUnit("gallon_beer", "beer_gal", Nil, r"282" * cubic_inch.interval)
  final object perch extends DefaultVolumeUnit("perch", "per", Nil, r"33"/r"2" * r"3"/r"2" * cubic_foot.interval)
  final object minim extends DefaultVolumeUnit("minim", "minim", Nil, `minim(US)`.interval)
  final object `minim(US)` extends DefaultVolumeUnit("minim(US)", "minim(US)", Nil, r"1"/r"480" * `fluid_ounce(US)`.interval)
  final object `minim(imp)` extends DefaultVolumeUnit("minim(imp)", "minim(imp)", Nil, r"1"/r"480" * `fluid_ounce(imp)`.interval)
  final object fluid_ounce extends DefaultVolumeUnit("fluid ounce", "fl_oz", Nil, 1)
  final object `fluid_ounce(US)` extends DefaultVolumeUnit("fluid ounce(US)", "fl_oz(US)", Nil, r"1"/r"128" * `gallon(US_fl)`.interval)
  final object `fluid_ounce(imp)` extends DefaultVolumeUnit("fluid ounce(imp)", "fl_oz(imp)", Nil, r"1"/r"160" * `gallon(imp)`.interval)
  final object gill extends DefaultVolumeUnit("gill", "gi", Nil, 1)
  final object `gill(US)` extends DefaultVolumeUnit("gill(US)", "gi(US)", Nil, r"4" * `fluid_ounce(US)`.interval)
  final object `gill(imp)` extends DefaultVolumeUnit("gill(imp)", "gi(imp)", Nil, r"5" * `fluid_ounce(imp)`.interval)
  final object pint extends DefaultVolumeUnit("pint", "pt", Nil, 1)
  final object `pint(US_fl)` extends DefaultVolumeUnit("pint(US_fl)", "pt(US_fl)", Nil, r"1"/r"8" * `gallon(US_fl)`.interval)
  final object `pint(US_dry)` extends DefaultVolumeUnit("pint(US_dry)", "pt(US_dry)", Nil, r"1"/r"8" * `gallon(US_dry)`.interval)
  final object `pint(imp)` extends DefaultVolumeUnit("pint(imp)", "pt(imp)", Nil, r"1"/r"8" * `gallon(imp)`.interval)
  final object quart extends DefaultVolumeUnit("quart", "qt", Nil, 1)
  final object `quart(US_fl)` extends DefaultVolumeUnit("quart(US_fl)", "qt(US_fl)", Nil, r"1"/r"4" * `gallon(US_fl)`.interval)
  final object `quart(US_dry)` extends DefaultVolumeUnit("quart(US_dry)", "qt(US_dry)", Nil, r"1"/r"4" * `gallon(US_dry)`.interval)
  final object `quart(imp)` extends DefaultVolumeUnit("quart(imp)", "qt(imp)", Nil, r"1"/r"4" * `gallon(imp)`.interval)
  final object gallon extends DefaultVolumeUnit("gallon", "gal", Nil, `gallon(US_fl)`.interval)
  final object `gallon(US)` extends DefaultVolumeUnit("gallon(US)", "gal(US)", Nil, `gallon(US_fl)`.interval)
  final object `gallon(US_fl)` extends DefaultVolumeUnit("gallon(US_fl)", "gal(US_fl)", Nil, r"231" * cubic_inch.interval)
  final object `gallon(US_dry)` extends DefaultVolumeUnit("gallon(US_dry)", "gal(US_dry)", Nil, r"1"/r"8" * `bushel(US_lvl)`.interval)
  final object `gallon(imp)` extends DefaultVolumeUnit("gallon(imp)", "gal(imp)", Nil, r"4.54609" * litre.interval)
  final object peck extends DefaultVolumeUnit("peck", "pk", Nil, `peck(US_dry)`.interval)
  final object `peck(US_dry)` extends DefaultVolumeUnit("peck(US_dry)", "pk(US_dry)", Nil, r"1"/r"4" * `bushel(US_lvl)`.interval)
  final object `peck(imp)` extends DefaultVolumeUnit("peck(imp)", "pk(imp)", Nil, r"2" * `gallon(imp)`.interval)
  final object bushel extends DefaultVolumeUnit("bushel", "bu", Seq("bsh"), 1)
  final object `bushel(US)` extends DefaultVolumeUnit("bushel(US)", "bu(US)", Seq("bsh(US)"), r"5"/r"4" * `bushel(US_lvl)`.interval)
  final object `bushel(US_lvl)` extends DefaultVolumeUnit("bushel(US_lvl)", "bu(US_lvl)", Seq("bsh(US_lvl)"), r"2150.42" * cubic_inch.interval)
  final object `bushel(imp)` extends DefaultVolumeUnit("bushel(imp)", "bu(imp)", Seq("bsh(imp)"), r"8" * `gallon(imp)`.interval)
  final object barrel extends DefaultVolumeUnit("barrel", "bl", Seq("bbl"), r"42" * `gallon(US_fl)`.interval)
  final object `barrel(US_fl)` extends DefaultVolumeUnit("barrel(US_fl)", "bl(US_fl)", Seq("bbl(US_fl)"), r"31.5" * `gallon(US_fl)`.interval)
  final object `barrel(US_dry)` extends DefaultVolumeUnit("barrel(US_dry)", "bl(US_dry)", Seq("bbl(US_dry)"), r"105" * `quart(US_dry)`.interval)
  final object `barrel(imp)` extends DefaultVolumeUnit("barrel(imp)", "bl(imp)", Seq("bbl(imp)"), r"36" * `gallon(imp)`.interval)
  final object fluid_barrel extends DefaultVolumeUnit("fluid barrel", "fl_bl", Nil, `barrel(US_fl)`.interval)
  final object hogshead extends DefaultVolumeUnit("hogshead", "hhd", Nil, 1)
  final object `hogshead(US)` extends DefaultVolumeUnit("hogshead(US)", "hhd(US)", Nil, r"2" * `barrel(US_fl)`.interval)
  final object `hogshead(imp)` extends DefaultVolumeUnit("hogshead(imp)", "hhd(imp)", Nil, r"2" * `barrel(imp)`.interval)
  final object fluid_dram extends DefaultVolumeUnit("fluid dram", "fl_dr", Nil, 1)
  final object `fluid_dram(US)` extends DefaultVolumeUnit("fluid dram(US)", "fl_dr(US)", Nil, r"1"/r"8" * `fluid_ounce(US)`.interval)
  final object `fluid_dram(imp)` extends DefaultVolumeUnit("fluid dram(imp)", "fl_dr(imp)", Nil, r"1"/r"8" * `fluid_ounce(imp)`.interval)
  final object fluid_scruple extends DefaultVolumeUnit("fluid scruple", "fl_s", Nil, r"1"/r"24" * `fluid_ounce(imp)`.interval)
  final object bucket extends DefaultVolumeUnit("bucket", "bkt", Nil, r"4" * `gallon(imp)`.interval)
}

object VolumeUnits{
  def `m³`: VolumeUnit = VolumeUnitObjects.cubic_metre
  def m3: VolumeUnit = VolumeUnitObjects.cubic_metre
  def `ym³`: VolumeUnit = VolumeUnitObjects.cubic_yoctometre
  def ym3: VolumeUnit = VolumeUnitObjects.cubic_yoctometre
  def `zm³`: VolumeUnit = VolumeUnitObjects.cubic_zeptometre
  def zm3: VolumeUnit = VolumeUnitObjects.cubic_zeptometre
  def `am³`: VolumeUnit = VolumeUnitObjects.cubic_attometre
  def am3: VolumeUnit = VolumeUnitObjects.cubic_attometre
  def `fm³`: VolumeUnit = VolumeUnitObjects.cubic_femtometre
  def fm3: VolumeUnit = VolumeUnitObjects.cubic_femtometre
  def `pm³`: VolumeUnit = VolumeUnitObjects.cubic_picometre
  def pm3: VolumeUnit = VolumeUnitObjects.cubic_picometre
  def `nm³`: VolumeUnit = VolumeUnitObjects.cubic_nanometre
  def nm3: VolumeUnit = VolumeUnitObjects.cubic_nanometre
  def `μm³`: VolumeUnit = VolumeUnitObjects.cubic_micrometre
  def μm3: VolumeUnit = VolumeUnitObjects.cubic_micrometre
  def `mcm³`: VolumeUnit = VolumeUnitObjects.cubic_micrometre
  def mcm3: VolumeUnit = VolumeUnitObjects.cubic_micrometre
  def `mm³`: VolumeUnit = VolumeUnitObjects.cubic_millimetre
  def mm3: VolumeUnit = VolumeUnitObjects.cubic_millimetre
  def `cm³`: VolumeUnit = VolumeUnitObjects.cubic_centimetre
  def cm3: VolumeUnit = VolumeUnitObjects.cubic_centimetre
  def `dm³`: VolumeUnit = VolumeUnitObjects.cubic_decimetre
  def dm3: VolumeUnit = VolumeUnitObjects.cubic_decimetre
  def `dam³`: VolumeUnit = VolumeUnitObjects.cubic_decametre
  def dam3: VolumeUnit = VolumeUnitObjects.cubic_decametre
  def `hm³`: VolumeUnit = VolumeUnitObjects.cubic_hectometre
  def hm3: VolumeUnit = VolumeUnitObjects.cubic_hectometre
  def `km³`: VolumeUnit = VolumeUnitObjects.cubic_kilometre
  def km3: VolumeUnit = VolumeUnitObjects.cubic_kilometre
  def `Km³`: VolumeUnit = VolumeUnitObjects.cubic_kilometre
  def Km3: VolumeUnit = VolumeUnitObjects.cubic_kilometre
  def `Mm³`: VolumeUnit = VolumeUnitObjects.cubic_megametre
  def Mm3: VolumeUnit = VolumeUnitObjects.cubic_megametre
  def `Gm³`: VolumeUnit = VolumeUnitObjects.cubic_gigametre
  def Gm3: VolumeUnit = VolumeUnitObjects.cubic_gigametre
  def `Tm³`: VolumeUnit = VolumeUnitObjects.cubic_terametre
  def Tm3: VolumeUnit = VolumeUnitObjects.cubic_terametre
  def `Pm³`: VolumeUnit = VolumeUnitObjects.cubic_petametre
  def Pm3: VolumeUnit = VolumeUnitObjects.cubic_petametre
  def `Em³`: VolumeUnit = VolumeUnitObjects.cubic_exametre
  def Em3: VolumeUnit = VolumeUnitObjects.cubic_exametre
  def `Zm³`: VolumeUnit = VolumeUnitObjects.cubic_zettametre
  def Zm3: VolumeUnit = VolumeUnitObjects.cubic_zettametre
  def `Ym³`: VolumeUnit = VolumeUnitObjects.cubic_yottametre
  def Ym3: VolumeUnit = VolumeUnitObjects.cubic_yottametre
  def L: VolumeUnit = VolumeUnitObjects.litre
  def yL: VolumeUnit = VolumeUnitObjects.yoctolitre
  def zL: VolumeUnit = VolumeUnitObjects.zeptolitre
  def aL: VolumeUnit = VolumeUnitObjects.attolitre
  def fL: VolumeUnit = VolumeUnitObjects.femtolitre
  def pL: VolumeUnit = VolumeUnitObjects.picolitre
  def nL: VolumeUnit = VolumeUnitObjects.nanolitre
  def μL: VolumeUnit = VolumeUnitObjects.microlitre
  def mcL: VolumeUnit = VolumeUnitObjects.microlitre
  def mL: VolumeUnit = VolumeUnitObjects.millilitre
  def cL: VolumeUnit = VolumeUnitObjects.centilitre
  def dL: VolumeUnit = VolumeUnitObjects.decilitre
  def daL: VolumeUnit = VolumeUnitObjects.decalitre
  def hL: VolumeUnit = VolumeUnitObjects.hectolitre
  def kL: VolumeUnit = VolumeUnitObjects.kilolitre
  def KL: VolumeUnit = VolumeUnitObjects.kilolitre
  def ML: VolumeUnit = VolumeUnitObjects.megalitre
  def GL: VolumeUnit = VolumeUnitObjects.gigalitre
  def TL: VolumeUnit = VolumeUnitObjects.teralitre
  def PL: VolumeUnit = VolumeUnitObjects.petalitre
  def EL: VolumeUnit = VolumeUnitObjects.exalitre
  def ZL: VolumeUnit = VolumeUnitObjects.zettalitre
  def YL: VolumeUnit = VolumeUnitObjects.yottalitre
  def λ: VolumeUnit = VolumeUnitObjects.lambda
  def `in³`: VolumeUnit = VolumeUnitObjects.cubic_inch
  def in3: VolumeUnit = VolumeUnitObjects.cubic_inch
  def cu_in: VolumeUnit = VolumeUnitObjects.cubic_inch
  def `ft³`: VolumeUnit = VolumeUnitObjects.cubic_foot
  def ft3: VolumeUnit = VolumeUnitObjects.cubic_foot
  def cu_ft: VolumeUnit = VolumeUnitObjects.cubic_foot
  def `yd³`: VolumeUnit = VolumeUnitObjects.cubic_yard
  def yd3: VolumeUnit = VolumeUnitObjects.cubic_yard
  def cu_yd: VolumeUnit = VolumeUnitObjects.cubic_yard
  def `ftm³`: VolumeUnit = VolumeUnitObjects.cubic_fathom
  def ftm3: VolumeUnit = VolumeUnitObjects.cubic_fathom
  def cu_fm: VolumeUnit = VolumeUnitObjects.cubic_fathom
  def `mi³`: VolumeUnit = VolumeUnitObjects.cubic_mile
  def mi3: VolumeUnit = VolumeUnitObjects.cubic_mile
  def cu_mi: VolumeUnit = VolumeUnitObjects.cubic_mile
  def fbm: VolumeUnit = VolumeUnitObjects.board_foot
  def beer_gal: VolumeUnit = VolumeUnitObjects.gallon_beer
  def per: VolumeUnit = VolumeUnitObjects.perch
  def minim: VolumeUnit = VolumeUnitObjects.minim
  def minim(a: minimAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US => VolumeUnitObjects.`minim(US)`
    case VolumeAttributes.imp => VolumeUnitObjects.`minim(imp)`
  }
  def fl_oz: VolumeUnit = VolumeUnitObjects.fluid_ounce
  def fl_oz(a: fluid_ounceAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US => VolumeUnitObjects.`fluid_ounce(US)`
    case VolumeAttributes.imp => VolumeUnitObjects.`fluid_ounce(imp)`
  }
  def gi: VolumeUnit = VolumeUnitObjects.gill
  def gi(a: gillAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US => VolumeUnitObjects.`gill(US)`
    case VolumeAttributes.imp => VolumeUnitObjects.`gill(imp)`
  }
  def pt: VolumeUnit = VolumeUnitObjects.pint
  def pt(a: pintAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US_fl => VolumeUnitObjects.`pint(US_fl)`
    case VolumeAttributes.US_dry => VolumeUnitObjects.`pint(US_dry)`
    case VolumeAttributes.imp => VolumeUnitObjects.`pint(imp)`
  }
  def qt: VolumeUnit = VolumeUnitObjects.quart
  def qt(a: quartAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US_fl => VolumeUnitObjects.`quart(US_fl)`
    case VolumeAttributes.US_dry => VolumeUnitObjects.`quart(US_dry)`
    case VolumeAttributes.imp => VolumeUnitObjects.`quart(imp)`
  }
  def gal: VolumeUnit = VolumeUnitObjects.gallon
  def gal(a: gallonAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US => VolumeUnitObjects.`gallon(US)`
    case VolumeAttributes.US_fl => VolumeUnitObjects.`gallon(US_fl)`
    case VolumeAttributes.US_dry => VolumeUnitObjects.`gallon(US_dry)`
    case VolumeAttributes.imp => VolumeUnitObjects.`gallon(imp)`
  }
  def pk: VolumeUnit = VolumeUnitObjects.peck
  def pk(a: peckAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US_dry => VolumeUnitObjects.`peck(US_dry)`
    case VolumeAttributes.imp => VolumeUnitObjects.`peck(imp)`
  }
  def bu: VolumeUnit = VolumeUnitObjects.bushel
  def bu(a: bushelAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US => VolumeUnitObjects.`bushel(US)`
    case VolumeAttributes.US_lvl => VolumeUnitObjects.`bushel(US_lvl)`
    case VolumeAttributes.imp => VolumeUnitObjects.`bushel(imp)`
  }
  def bsh: VolumeUnit = VolumeUnitObjects.bushel
  def bsh(a: bushelAttribute): VolumeUnit = bu(a)

  def bl: VolumeUnit = VolumeUnitObjects.barrel
  def bl(a: barrelAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US_fl => VolumeUnitObjects.`barrel(US_fl)`
    case VolumeAttributes.US_dry => VolumeUnitObjects.`barrel(US_dry)`
    case VolumeAttributes.imp => VolumeUnitObjects.`barrel(imp)`
  }
  def bbl: VolumeUnit = VolumeUnitObjects.barrel
  def bbl(a: barrelAttribute): VolumeUnit = bl(a)

  def fl_bl: VolumeUnit = VolumeUnitObjects.fluid_barrel
  def hhd: VolumeUnit = VolumeUnitObjects.hogshead
  def hhd(a: hogsheadAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US => VolumeUnitObjects.`hogshead(US)`
    case VolumeAttributes.imp => VolumeUnitObjects.`hogshead(imp)`
  }
  def fl_dr: VolumeUnit = VolumeUnitObjects.fluid_dram
  def fl_dr(a: fluid_dramAttribute): VolumeUnit = a match { 
    case VolumeAttributes.US => VolumeUnitObjects.`fluid_dram(US)`
    case VolumeAttributes.imp => VolumeUnitObjects.`fluid_dram(imp)`
  }
  def fl_s: VolumeUnit = VolumeUnitObjects.fluid_scruple
  def bkt: VolumeUnit = VolumeUnitObjects.bucket
}