package org.waman.multiverse.unit.defs

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs.fluid._
import org.waman.multiverse.unit.defs.mechanics._

class Volume[A: Fractional](val value: A, val unit: VolumeUnit)
    extends LinearQuantity[Volume[A], A, VolumeUnit] {

  override protected def newQuantity(value: A, unit: VolumeUnit): Volume[A] = new Volume(value, unit)

  def /(time: Time[A]): VolumeFlow[A] = new VolumeFlow(this.value / time.value, this.unit / time.unit)

  def *(pressure: Pressure[A]): Energy[A] = new Energy(this.value * pressure.value, this.unit * pressure.unit)
}

trait VolumeUnit extends LinearUnit[VolumeUnit]{

  override def getSIUnit: VolumeUnit = VolumeUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = VolumeUnit.dimension

  def /(timeUnit: TimeUnit): VolumeFlowUnit =
    new QuotientUnit[VolumeFlowUnit, VolumeUnit, TimeUnit](VolumeUnit.this, timeUnit) with VolumeFlowUnit

  def *(pressureUnit: PressureUnit): EnergyUnit =
    new ProductUnit[EnergyUnit, VolumeUnit, PressureUnit](VolumeUnit.this, pressureUnit) with EnergyUnit
}

object VolumeUnit extends UnitInfo[VolumeUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](L -> 3).withDefaultValue(0)

  val getSIUnit: VolumeUnit = AreaUnit.getSIUnit * LengthUnit.getSIUnit
  import VolumeUnitObjects._

  def getUnits: Seq[VolumeUnit] =
    Seq(cubic_metre, cubic_yoctometre, cubic_zeptometre, cubic_attometre, cubic_femtometre, cubic_picometre, cubic_nanometre, cubic_micrometre, cubic_millimetre, cubic_centimetre, cubic_decimetre, cubic_decametre, cubic_hectometre, cubic_kilometre, cubic_megametre, cubic_gigametre, cubic_terametre, cubic_petametre, cubic_exametre, cubic_zettametre, cubic_yottametre, litre, yoctolitre, zeptolitre, attolitre, femtolitre, picolitre, nanolitre, microlitre, millilitre, centilitre, decilitre, decalitre, hectolitre, kilolitre, megalitre, gigalitre, teralitre, petalitre, exalitre, zettalitre, yottalitre, lambda, cubic_inch, cubic_foot, cubic_yard, cubic_fathom, cubic_mile, acre_foot, board_foot, gallon_beer, perch, minim, `minim(US)`, `minim(imp)`, teaspoon, tablespoon, fluid_ounce, `fluid_ounce(US)`, `fluid_ounce(imp)`, shot, gill, `gill(US)`, `gill(imp)`, cup, `cup(metric)`, `cup(US)`, pint, `pint(US_fl)`, `pint(US_dry)`, `pint(imp)`, quart, `quart(US_fl)`, `quart(US_dry)`, `quart(imp)`, pottle, `pottle(US)`, `pottle(imp)`, gallon, `gallon(US)`, `gallon(US_fl)`, `gallon(US_dry)`, `gallon(imp)`, peck, `peck(US_dry)`, `peck(imp)`, bushel, `bushel(US)`, `bushel(US_lvl)`, `bushel(imp)`, barrel, `barrel(US_fl)`, `barrel(US_dry)`, `barrel(imp)`, fluid_barrel, hogshead, `hogshead(US)`, `hogshead(imp)`, fluid_dram, `fluid_dram(US)`, `fluid_dram(imp)`, fluid_scruple, bucket)
}


/** For no aliase or user defined units */
class SimpleVolumeUnit(val name: String, val symbol: String, val interval: Real) extends VolumeUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultVolumeUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends VolumeUnit
  
object VolumeUnitObjects{

  final case object cubic_metre extends LengthPoweredVolumeUnit(LengthUnitObjects.metre, Seq("m3"))
  final case object cubic_yoctometre extends LengthPoweredVolumeUnit(LengthUnitObjects.yoctometre, Seq("ym3"))
  final case object cubic_zeptometre extends LengthPoweredVolumeUnit(LengthUnitObjects.zeptometre, Seq("zm3"))
  final case object cubic_attometre extends LengthPoweredVolumeUnit(LengthUnitObjects.attometre, Seq("am3"))
  final case object cubic_femtometre extends LengthPoweredVolumeUnit(LengthUnitObjects.femtometre, Seq("fm3"))
  final case object cubic_picometre extends LengthPoweredVolumeUnit(LengthUnitObjects.picometre, Seq("pm3"))
  final case object cubic_nanometre extends LengthPoweredVolumeUnit(LengthUnitObjects.nanometre, Seq("nm3"))
  final case object cubic_micrometre extends LengthPoweredVolumeUnit(LengthUnitObjects.micrometre, Seq("mcm³", "μm3", "mcm3"))
  final case object cubic_millimetre extends LengthPoweredVolumeUnit(LengthUnitObjects.millimetre, Seq("mm3"))
  final case object cubic_centimetre extends LengthPoweredVolumeUnit(LengthUnitObjects.centimetre, Seq("cm3"))
  final case object cubic_decimetre extends LengthPoweredVolumeUnit(LengthUnitObjects.decimetre, Seq("dm3"))
  final case object cubic_decametre extends LengthPoweredVolumeUnit(LengthUnitObjects.decametre, Seq("dam3"))
  final case object cubic_hectometre extends LengthPoweredVolumeUnit(LengthUnitObjects.hectometre, Seq("hm3"))
  final case object cubic_kilometre extends LengthPoweredVolumeUnit(LengthUnitObjects.kilometre, Seq("Km³", "km3", "Km3"))
  final case object cubic_megametre extends LengthPoweredVolumeUnit(LengthUnitObjects.megametre, Seq("Mm3"))
  final case object cubic_gigametre extends LengthPoweredVolumeUnit(LengthUnitObjects.gigametre, Seq("Gm3"))
  final case object cubic_terametre extends LengthPoweredVolumeUnit(LengthUnitObjects.terametre, Seq("Tm3"))
  final case object cubic_petametre extends LengthPoweredVolumeUnit(LengthUnitObjects.petametre, Seq("Pm3"))
  final case object cubic_exametre extends LengthPoweredVolumeUnit(LengthUnitObjects.exametre, Seq("Em3"))
  final case object cubic_zettametre extends LengthPoweredVolumeUnit(LengthUnitObjects.zettametre, Seq("Zm3"))
  final case object cubic_yottametre extends LengthPoweredVolumeUnit(LengthUnitObjects.yottametre, Seq("Ym3"))
  final case object litre extends SimpleVolumeUnit("litre", "L", r"1e-3")
  final case object yoctolitre extends SimpleVolumeUnit("yoctolitre", "yL", r"1e-3" * r"1e-24")
  final case object zeptolitre extends SimpleVolumeUnit("zeptolitre", "zL", r"1e-3" * r"1e-21")
  final case object attolitre extends SimpleVolumeUnit("attolitre", "aL", r"1e-3" * r"1e-18")
  final case object femtolitre extends SimpleVolumeUnit("femtolitre", "fL", r"1e-3" * r"1e-15")
  final case object picolitre extends SimpleVolumeUnit("picolitre", "pL", r"1e-3" * r"1e-12")
  final case object nanolitre extends SimpleVolumeUnit("nanolitre", "nL", r"1e-3" * r"1e-9")
  final case object microlitre extends DefaultVolumeUnit("microlitre", "μL", Seq("mcL"), r"1e-3" * r"1e-6")
  final case object millilitre extends SimpleVolumeUnit("millilitre", "mL", r"1e-3" * r"1e-3")
  final case object centilitre extends SimpleVolumeUnit("centilitre", "cL", r"1e-3" * r"1e-2")
  final case object decilitre extends SimpleVolumeUnit("decilitre", "dL", r"1e-3" * r"1e-1")
  final case object decalitre extends SimpleVolumeUnit("decalitre", "daL", r"1e-3" * r"1e1")
  final case object hectolitre extends SimpleVolumeUnit("hectolitre", "hL", r"1e-3" * r"1e2")
  final case object kilolitre extends DefaultVolumeUnit("kilolitre", "kL", Seq("KL"), r"1e-3" * r"1e3")
  final case object megalitre extends SimpleVolumeUnit("megalitre", "ML", r"1e-3" * r"1e6")
  final case object gigalitre extends SimpleVolumeUnit("gigalitre", "GL", r"1e-3" * r"1e9")
  final case object teralitre extends SimpleVolumeUnit("teralitre", "TL", r"1e-3" * r"1e12")
  final case object petalitre extends SimpleVolumeUnit("petalitre", "PL", r"1e-3" * r"1e15")
  final case object exalitre extends SimpleVolumeUnit("exalitre", "EL", r"1e-3" * r"1e18")
  final case object zettalitre extends SimpleVolumeUnit("zettalitre", "ZL", r"1e-3" * r"1e21")
  final case object yottalitre extends SimpleVolumeUnit("yottalitre", "YL", r"1e-3" * r"1e24")
  final case object lambda extends SimpleVolumeUnit("lambda", "λ", r"1e-9")
  final case object cubic_inch extends LengthPoweredVolumeUnit(LengthUnitObjects.inch, Seq("in3", "cu_in"))
  final case object cubic_foot extends LengthPoweredVolumeUnit(LengthUnitObjects.foot, Seq("ft3", "cu_ft"))
  final case object cubic_yard extends LengthPoweredVolumeUnit(LengthUnitObjects.yard, Seq("yd3", "cu_yd"))
  final case object cubic_fathom extends LengthPoweredVolumeUnit(LengthUnitObjects.fathom, Seq("ftm3", "cu_fm"))
  final case object cubic_mile extends LengthPoweredVolumeUnit(LengthUnitObjects.mile, Seq("mi3", "cu_mi"))
  final case object acre_foot extends SimpleVolumeUnit("acre foot", "acre_ft", AreaUnitObjects.acre.interval * LengthUnitObjects.foot.interval)
  final case object board_foot extends SimpleVolumeUnit("board foot", "fbm", r"144" * cubic_inch.interval)
  final case object gallon_beer extends SimpleVolumeUnit("gallon_beer", "beer_gal", r"282" * cubic_inch.interval)
  final case object perch extends SimpleVolumeUnit("perch", "per", r"33/2" * r"3/2" * cubic_foot.interval)
  final case object minim extends SimpleVolumeUnit("minim", "minim", `minim(US)`.interval)
  final case object `minim(US)` extends SimpleVolumeUnit("minim(US)", "minim(US)", r"1/480" * `fluid_ounce(US)`.interval)
  final case object `minim(imp)` extends SimpleVolumeUnit("minim(imp)", "minim(imp)", r"1/480" * `fluid_ounce(imp)`.interval)
  final case object teaspoon extends SimpleVolumeUnit("teaspoon", "tsp", r"80" * minim.interval)
  final case object tablespoon extends SimpleVolumeUnit("tablespoon", "Tbsp", r"3" * teaspoon.interval)
  final case object fluid_ounce extends SimpleVolumeUnit("fluid ounce", "fl_oz", 1)
  final case object `fluid_ounce(US)` extends DefaultVolumeUnit("fluid ounce(US)", "fl_oz(US)", Seq("US_fl_oz"), r"1/128" * `gallon(US_fl)`.interval)
  final case object `fluid_ounce(imp)` extends SimpleVolumeUnit("fluid ounce(imp)", "fl_oz(imp)", r"1/160" * `gallon(imp)`.interval)
  final case object shot extends SimpleVolumeUnit("shot", "jig", r"3" * tablespoon.interval)
  final case object gill extends SimpleVolumeUnit("gill", "gi", 1)
  final case object `gill(US)` extends SimpleVolumeUnit("gill(US)", "gi(US)", r"4" * `fluid_ounce(US)`.interval)
  final case object `gill(imp)` extends DefaultVolumeUnit("gill(imp)", "gi(imp)", Seq("nog"), r"5" * `fluid_ounce(imp)`.interval)
  final case object cup extends SimpleVolumeUnit("cup", "cp", 1)
  final case object `cup(metric)` extends SimpleVolumeUnit("cup(metric)", "cp(metric)", r"250e-6")
  final case object `cup(US)` extends SimpleVolumeUnit("cup(US)", "cp(US)", r"2" * `gill(US)`.interval)
  final case object pint extends SimpleVolumeUnit("pint", "pt", 1)
  final case object `pint(US_fl)` extends SimpleVolumeUnit("pint(US_fl)", "pt(US_fl)", r"1/8" * `gallon(US_fl)`.interval)
  final case object `pint(US_dry)` extends SimpleVolumeUnit("pint(US_dry)", "pt(US_dry)", r"1/8" * `gallon(US_dry)`.interval)
  final case object `pint(imp)` extends SimpleVolumeUnit("pint(imp)", "pt(imp)", r"1/8" * `gallon(imp)`.interval)
  final case object quart extends SimpleVolumeUnit("quart", "qt", 1)
  final case object `quart(US_fl)` extends SimpleVolumeUnit("quart(US_fl)", "qt(US_fl)", r"1/4" * `gallon(US_fl)`.interval)
  final case object `quart(US_dry)` extends SimpleVolumeUnit("quart(US_dry)", "qt(US_dry)", r"1/4" * `gallon(US_dry)`.interval)
  final case object `quart(imp)` extends SimpleVolumeUnit("quart(imp)", "qt(imp)", r"1/4" * `gallon(imp)`.interval)
  final case object pottle extends SimpleVolumeUnit("pottle", "pot", 1)
  final case object `pottle(US)` extends SimpleVolumeUnit("pottle(US)", "pot(US)", r"1/2" * `gallon(US_fl)`.interval)
  final case object `pottle(imp)` extends SimpleVolumeUnit("pottle(imp)", "pot(imp)", r"1/2" * `gallon(imp)`.interval)
  final case object gallon extends SimpleVolumeUnit("gallon", "gal", `gallon(US_fl)`.interval)
  final case object `gallon(US)` extends SimpleVolumeUnit("gallon(US)", "gal(US)", `gallon(US_fl)`.interval)
  final case object `gallon(US_fl)` extends DefaultVolumeUnit("gallon(US_fl)", "gal(US_fl)", Seq("US_gal"), r"231" * cubic_inch.interval)
  final case object `gallon(US_dry)` extends SimpleVolumeUnit("gallon(US_dry)", "gal(US_dry)", r"1/8" * `bushel(US_lvl)`.interval)
  final case object `gallon(imp)` extends DefaultVolumeUnit("gallon(imp)", "gal(imp)", Seq("imp_gal"), r"4.54609" * litre.interval)
  final case object peck extends SimpleVolumeUnit("peck", "pk", `peck(US_dry)`.interval)
  final case object `peck(US_dry)` extends SimpleVolumeUnit("peck(US_dry)", "pk(US_dry)", r"1/4" * `bushel(US_lvl)`.interval)
  final case object `peck(imp)` extends SimpleVolumeUnit("peck(imp)", "pk(imp)", r"2" * `gallon(imp)`.interval)
  final case object bushel extends DefaultVolumeUnit("bushel", "bu", Seq("bsh"), `bushel(US_lvl)`.interval)
  final case object `bushel(US)` extends DefaultVolumeUnit("bushel(US)", "bu(US)", Seq("bsh(US)"), r"5/4" * `bushel(US_lvl)`.interval)
  final case object `bushel(US_lvl)` extends DefaultVolumeUnit("bushel(US_lvl)", "bu(US_lvl)", Seq("bsh(US_lvl)"), r"2150.42" * cubic_inch.interval)
  final case object `bushel(imp)` extends DefaultVolumeUnit("bushel(imp)", "bu(imp)", Seq("bsh(imp)"), r"8" * `gallon(imp)`.interval)
  final case object barrel extends DefaultVolumeUnit("barrel", "bbl", Seq("bl"), r"42" * `gallon(US_fl)`.interval) with Description {
    def description: String = "oil barrel"
  }
  final case object `barrel(US_fl)` extends DefaultVolumeUnit("barrel(US_fl)", "bbl(US_fl)", Seq("bl(US_fl)"), r"31.5" * `gallon(US_fl)`.interval)
  final case object `barrel(US_dry)` extends DefaultVolumeUnit("barrel(US_dry)", "bbl(US_dry)", Seq("bl(US_dry)"), r"105" * `quart(US_dry)`.interval)
  final case object `barrel(imp)` extends DefaultVolumeUnit("barrel(imp)", "bbl(imp)", Seq("bl(imp)"), r"36" * `gallon(imp)`.interval)
  final case object fluid_barrel extends SimpleVolumeUnit("fluid barrel", "fl_bl", `barrel(US_fl)`.interval) with Description {
    def description: String = "This unit is equivalent to barrel(US_fl)."
  }
  final case object hogshead extends SimpleVolumeUnit("hogshead", "hhd", 1)
  final case object `hogshead(US)` extends SimpleVolumeUnit("hogshead(US)", "hhd(US)", r"2" * `barrel(US_fl)`.interval)
  final case object `hogshead(imp)` extends SimpleVolumeUnit("hogshead(imp)", "hhd(imp)", r"2" * `barrel(imp)`.interval)
  final case object fluid_dram extends SimpleVolumeUnit("fluid dram", "fl_dr", 1)
  final case object `fluid_dram(US)` extends SimpleVolumeUnit("fluid dram(US)", "fl_dr(US)", r"1/8" * `fluid_ounce(US)`.interval)
  final case object `fluid_dram(imp)` extends SimpleVolumeUnit("fluid dram(imp)", "fl_dr(imp)", r"1/8" * `fluid_ounce(imp)`.interval)
  final case object fluid_scruple extends SimpleVolumeUnit("fluid scruple", "fl_s", r"1/24" * `fluid_ounce(imp)`.interval)
  final case object bucket extends SimpleVolumeUnit("bucket", "bkt", r"4" * `gallon(imp)`.interval)
}


object VolumeUnits{

  /** cubic metre */
  def `m³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_metre
  /** cubic metre */
  def m3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_metre
  /** cubic yoctometre */
  def `ym³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_yoctometre
  /** cubic yoctometre */
  def ym3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_yoctometre
  /** cubic zeptometre */
  def `zm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_zeptometre
  /** cubic zeptometre */
  def zm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_zeptometre
  /** cubic attometre */
  def `am³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_attometre
  /** cubic attometre */
  def am3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_attometre
  /** cubic femtometre */
  def `fm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_femtometre
  /** cubic femtometre */
  def fm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_femtometre
  /** cubic picometre */
  def `pm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_picometre
  /** cubic picometre */
  def pm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_picometre
  /** cubic nanometre */
  def `nm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_nanometre
  /** cubic nanometre */
  def nm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_nanometre
  /** cubic micrometre */
  def `μm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_micrometre
  /** cubic micrometre */
  def `mcm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_micrometre
  /** cubic micrometre */
  def μm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_micrometre
  /** cubic micrometre */
  def mcm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_micrometre
  /** cubic millimetre */
  def `mm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_millimetre
  /** cubic millimetre */
  def mm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_millimetre
  /** cubic centimetre */
  def `cm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_centimetre
  /** cubic centimetre */
  def cm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_centimetre
  /** cubic decimetre */
  def `dm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_decimetre
  /** cubic decimetre */
  def dm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_decimetre
  /** cubic decametre */
  def `dam³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_decametre
  /** cubic decametre */
  def dam3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_decametre
  /** cubic hectometre */
  def `hm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_hectometre
  /** cubic hectometre */
  def hm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_hectometre
  /** cubic kilometre */
  def `km³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_kilometre
  /** cubic kilometre */
  def `Km³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_kilometre
  /** cubic kilometre */
  def km3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_kilometre
  /** cubic kilometre */
  def Km3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_kilometre
  /** cubic megametre */
  def `Mm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_megametre
  /** cubic megametre */
  def Mm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_megametre
  /** cubic gigametre */
  def `Gm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_gigametre
  /** cubic gigametre */
  def Gm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_gigametre
  /** cubic terametre */
  def `Tm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_terametre
  /** cubic terametre */
  def Tm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_terametre
  /** cubic petametre */
  def `Pm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_petametre
  /** cubic petametre */
  def Pm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_petametre
  /** cubic exametre */
  def `Em³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_exametre
  /** cubic exametre */
  def Em3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_exametre
  /** cubic zettametre */
  def `Zm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_zettametre
  /** cubic zettametre */
  def Zm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_zettametre
  /** cubic yottametre */
  def `Ym³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_yottametre
  /** cubic yottametre */
  def Ym3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_yottametre
  /** litre */
  def L: VolumeUnit = VolumeUnitObjects.litre
  /** yoctolitre */
  def yL: VolumeUnit = VolumeUnitObjects.yoctolitre
  /** zeptolitre */
  def zL: VolumeUnit = VolumeUnitObjects.zeptolitre
  /** attolitre */
  def aL: VolumeUnit = VolumeUnitObjects.attolitre
  /** femtolitre */
  def fL: VolumeUnit = VolumeUnitObjects.femtolitre
  /** picolitre */
  def pL: VolumeUnit = VolumeUnitObjects.picolitre
  /** nanolitre */
  def nL: VolumeUnit = VolumeUnitObjects.nanolitre
  /** microlitre */
  def μL: VolumeUnit = VolumeUnitObjects.microlitre
  /** microlitre */
  def mcL: VolumeUnit = VolumeUnitObjects.microlitre
  /** millilitre */
  def mL: VolumeUnit = VolumeUnitObjects.millilitre
  /** centilitre */
  def cL: VolumeUnit = VolumeUnitObjects.centilitre
  /** decilitre */
  def dL: VolumeUnit = VolumeUnitObjects.decilitre
  /** decalitre */
  def daL: VolumeUnit = VolumeUnitObjects.decalitre
  /** hectolitre */
  def hL: VolumeUnit = VolumeUnitObjects.hectolitre
  /** kilolitre */
  def kL: VolumeUnit = VolumeUnitObjects.kilolitre
  /** kilolitre */
  def KL: VolumeUnit = VolumeUnitObjects.kilolitre
  /** megalitre */
  def ML: VolumeUnit = VolumeUnitObjects.megalitre
  /** gigalitre */
  def GL: VolumeUnit = VolumeUnitObjects.gigalitre
  /** teralitre */
  def TL: VolumeUnit = VolumeUnitObjects.teralitre
  /** petalitre */
  def PL: VolumeUnit = VolumeUnitObjects.petalitre
  /** exalitre */
  def EL: VolumeUnit = VolumeUnitObjects.exalitre
  /** zettalitre */
  def ZL: VolumeUnit = VolumeUnitObjects.zettalitre
  /** yottalitre */
  def YL: VolumeUnit = VolumeUnitObjects.yottalitre
  /** lambda */
  def λ: VolumeUnit = VolumeUnitObjects.lambda
  /** cubic inch */
  def `in³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_inch
  /** cubic inch */
  def in3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_inch
  /** cubic inch */
  def cu_in: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_inch
  /** cubic foot */
  def `ft³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_foot
  /** cubic foot */
  def ft3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_foot
  /** cubic foot */
  def cu_ft: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_foot
  /** cubic yard */
  def `yd³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_yard
  /** cubic yard */
  def yd3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_yard
  /** cubic yard */
  def cu_yd: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_yard
  /** cubic fathom */
  def `ftm³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_fathom
  /** cubic fathom */
  def ftm3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_fathom
  /** cubic fathom */
  def cu_fm: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_fathom
  /** cubic mile */
  def `mi³`: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_mile
  /** cubic mile */
  def mi3: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_mile
  /** cubic mile */
  def cu_mi: LengthPoweredVolumeUnit = VolumeUnitObjects.cubic_mile
  /** acre foot */
  def acre_ft: VolumeUnit = VolumeUnitObjects.acre_foot
  /** board foot */
  def fbm: VolumeUnit = VolumeUnitObjects.board_foot
  /** gallon_beer */
  def beer_gal: VolumeUnit = VolumeUnitObjects.gallon_beer
  /** perch */
  def per: VolumeUnit = VolumeUnitObjects.perch
  /** minim */
  def minim: VolumeUnit = VolumeUnitObjects.minim
  /** minim(US)<br/>minim(imp) */
  def minim(a: minimAttribute): VolumeUnit = a match {
    case MetricAttributes.US => VolumeUnitObjects.`minim(US)`
    case MetricAttributes.imp => VolumeUnitObjects.`minim(imp)`
  }
  /** minim(US) */
  def `minim(US)`: VolumeUnit = VolumeUnitObjects.`minim(US)`
  /** minim(imp) */
  def `minim(imp)`: VolumeUnit = VolumeUnitObjects.`minim(imp)`
  /** teaspoon */
  def tsp: VolumeUnit = VolumeUnitObjects.teaspoon
  /** tablespoon */
  def Tbsp: VolumeUnit = VolumeUnitObjects.tablespoon
  /** fluid ounce */
  def fl_oz: VolumeUnit = VolumeUnitObjects.fluid_ounce
  /** fluid_ounce(US)<br/>fluid_ounce(imp) */
  def fl_oz(a: fluid_ounceAttribute): VolumeUnit = a match {
    case MetricAttributes.US => VolumeUnitObjects.`fluid_ounce(US)`
    case MetricAttributes.imp => VolumeUnitObjects.`fluid_ounce(imp)`
  }
  /** fluid ounce(US) */
  def `fl_oz(US)`: VolumeUnit = VolumeUnitObjects.`fluid_ounce(US)`
  /** fluid ounce(US) */
  def US_fl_oz: VolumeUnit = VolumeUnitObjects.`fluid_ounce(US)`
  /** fluid ounce(imp) */
  def `fl_oz(imp)`: VolumeUnit = VolumeUnitObjects.`fluid_ounce(imp)`
  /** shot */
  def jig: VolumeUnit = VolumeUnitObjects.shot
  /** gill */
  def gi: VolumeUnit = VolumeUnitObjects.gill
  /** gill(US)<br/>gill(imp) */
  def gi(a: gillAttribute): VolumeUnit = a match {
    case MetricAttributes.US => VolumeUnitObjects.`gill(US)`
    case MetricAttributes.imp => VolumeUnitObjects.`gill(imp)`
  }
  /** gill(US) */
  def `gi(US)`: VolumeUnit = VolumeUnitObjects.`gill(US)`
  /** gill(imp) */
  def `gi(imp)`: VolumeUnit = VolumeUnitObjects.`gill(imp)`
  /** gill(imp) */
  def nog: VolumeUnit = VolumeUnitObjects.`gill(imp)`
  /** cup */
  def cp: VolumeUnit = VolumeUnitObjects.cup
  /** cup(metric)<br/>cup(US) */
  def cp(a: cupAttribute): VolumeUnit = a match {
    case MetricAttributes.metric => VolumeUnitObjects.`cup(metric)`
    case MetricAttributes.US => VolumeUnitObjects.`cup(US)`
  }
  /** cup(metric) */
  def `cp(metric)`: VolumeUnit = VolumeUnitObjects.`cup(metric)`
  /** cup(US) */
  def `cp(US)`: VolumeUnit = VolumeUnitObjects.`cup(US)`
  /** pint */
  def pt: VolumeUnit = VolumeUnitObjects.pint
  /** pint(US_fl)<br/>pint(US_dry)<br/>pint(imp) */
  def pt(a: pintAttribute): VolumeUnit = a match {
    case MetricAttributes.US_fl => VolumeUnitObjects.`pint(US_fl)`
    case MetricAttributes.US_dry => VolumeUnitObjects.`pint(US_dry)`
    case MetricAttributes.imp => VolumeUnitObjects.`pint(imp)`
  }
  /** pint(US_fl) */
  def `pt(US_fl)`: VolumeUnit = VolumeUnitObjects.`pint(US_fl)`
  /** pint(US_dry) */
  def `pt(US_dry)`: VolumeUnit = VolumeUnitObjects.`pint(US_dry)`
  /** pint(imp) */
  def `pt(imp)`: VolumeUnit = VolumeUnitObjects.`pint(imp)`
  /** quart */
  def qt: VolumeUnit = VolumeUnitObjects.quart
  /** quart(US_fl)<br/>quart(US_dry)<br/>quart(imp) */
  def qt(a: quartAttribute): VolumeUnit = a match {
    case MetricAttributes.US_fl => VolumeUnitObjects.`quart(US_fl)`
    case MetricAttributes.US_dry => VolumeUnitObjects.`quart(US_dry)`
    case MetricAttributes.imp => VolumeUnitObjects.`quart(imp)`
  }
  /** quart(US_fl) */
  def `qt(US_fl)`: VolumeUnit = VolumeUnitObjects.`quart(US_fl)`
  /** quart(US_dry) */
  def `qt(US_dry)`: VolumeUnit = VolumeUnitObjects.`quart(US_dry)`
  /** quart(imp) */
  def `qt(imp)`: VolumeUnit = VolumeUnitObjects.`quart(imp)`
  /** pottle */
  def pot: VolumeUnit = VolumeUnitObjects.pottle
  /** pottle(US)<br/>pottle(imp) */
  def pot(a: pottleAttribute): VolumeUnit = a match {
    case MetricAttributes.US => VolumeUnitObjects.`pottle(US)`
    case MetricAttributes.imp => VolumeUnitObjects.`pottle(imp)`
  }
  /** pottle(US) */
  def `pot(US)`: VolumeUnit = VolumeUnitObjects.`pottle(US)`
  /** pottle(imp) */
  def `pot(imp)`: VolumeUnit = VolumeUnitObjects.`pottle(imp)`
  /** gallon */
  def gal: VolumeUnit = VolumeUnitObjects.gallon
  /** gallon(US)<br/>gallon(US_fl)<br/>gallon(US_dry)<br/>gallon(imp) */
  def gal(a: gallonAttribute): VolumeUnit = a match {
    case MetricAttributes.US => VolumeUnitObjects.`gallon(US)`
    case MetricAttributes.US_fl => VolumeUnitObjects.`gallon(US_fl)`
    case MetricAttributes.US_dry => VolumeUnitObjects.`gallon(US_dry)`
    case MetricAttributes.imp => VolumeUnitObjects.`gallon(imp)`
  }
  /** gallon(US) */
  def `gal(US)`: VolumeUnit = VolumeUnitObjects.`gallon(US)`
  /** gallon(US_fl) */
  def `gal(US_fl)`: VolumeUnit = VolumeUnitObjects.`gallon(US_fl)`
  /** gallon(US_fl) */
  def US_gal: VolumeUnit = VolumeUnitObjects.`gallon(US_fl)`
  /** gallon(US_dry) */
  def `gal(US_dry)`: VolumeUnit = VolumeUnitObjects.`gallon(US_dry)`
  /** gallon(imp) */
  def `gal(imp)`: VolumeUnit = VolumeUnitObjects.`gallon(imp)`
  /** gallon(imp) */
  def imp_gal: VolumeUnit = VolumeUnitObjects.`gallon(imp)`
  /** peck */
  def pk: VolumeUnit = VolumeUnitObjects.peck
  /** peck(US_dry)<br/>peck(imp) */
  def pk(a: peckAttribute): VolumeUnit = a match {
    case MetricAttributes.US_dry => VolumeUnitObjects.`peck(US_dry)`
    case MetricAttributes.imp => VolumeUnitObjects.`peck(imp)`
  }
  /** peck(US_dry) */
  def `pk(US_dry)`: VolumeUnit = VolumeUnitObjects.`peck(US_dry)`
  /** peck(imp) */
  def `pk(imp)`: VolumeUnit = VolumeUnitObjects.`peck(imp)`
  /** bushel */
  def bu: VolumeUnit = VolumeUnitObjects.bushel
  /** bushel(US)<br/>bushel(US_lvl)<br/>bushel(imp) */
  def bu(a: bushelAttribute): VolumeUnit = a match {
    case MetricAttributes.US => VolumeUnitObjects.`bushel(US)`
    case MetricAttributes.US_lvl => VolumeUnitObjects.`bushel(US_lvl)`
    case MetricAttributes.imp => VolumeUnitObjects.`bushel(imp)`
  }
  /** bushel */
  def bsh: VolumeUnit = VolumeUnitObjects.bushel
  /**   bushel(US)<br/>  bushel(US_lvl)<br/>  bushel(imp) */
  def bsh(a: bushelAttribute): VolumeUnit = bu(a)
  /** bushel(US) */
  def `bu(US)`: VolumeUnit = VolumeUnitObjects.`bushel(US)`
  /** bushel(US) */
  def `bsh(US)`: VolumeUnit = VolumeUnitObjects.`bushel(US)`
  /** bushel(US_lvl) */
  def `bu(US_lvl)`: VolumeUnit = VolumeUnitObjects.`bushel(US_lvl)`
  /** bushel(US_lvl) */
  def `bsh(US_lvl)`: VolumeUnit = VolumeUnitObjects.`bushel(US_lvl)`
  /** bushel(imp) */
  def `bu(imp)`: VolumeUnit = VolumeUnitObjects.`bushel(imp)`
  /** bushel(imp) */
  def `bsh(imp)`: VolumeUnit = VolumeUnitObjects.`bushel(imp)`
  /** barrel */
  def bbl: VolumeUnit = VolumeUnitObjects.barrel
  /** barrel(US_fl)<br/>barrel(US_dry)<br/>barrel(imp) */
  def bbl(a: barrelAttribute): VolumeUnit = a match {
    case MetricAttributes.US_fl => VolumeUnitObjects.`barrel(US_fl)`
    case MetricAttributes.US_dry => VolumeUnitObjects.`barrel(US_dry)`
    case MetricAttributes.imp => VolumeUnitObjects.`barrel(imp)`
  }
  /** barrel */
  def bl: VolumeUnit = VolumeUnitObjects.barrel
  /**   barrel(US_fl)<br/>  barrel(US_dry)<br/>  barrel(imp) */
  def bl(a: barrelAttribute): VolumeUnit = bbl(a)
  /** barrel(US_fl) */
  def `bbl(US_fl)`: VolumeUnit = VolumeUnitObjects.`barrel(US_fl)`
  /** barrel(US_fl) */
  def `bl(US_fl)`: VolumeUnit = VolumeUnitObjects.`barrel(US_fl)`
  /** barrel(US_dry) */
  def `bbl(US_dry)`: VolumeUnit = VolumeUnitObjects.`barrel(US_dry)`
  /** barrel(US_dry) */
  def `bl(US_dry)`: VolumeUnit = VolumeUnitObjects.`barrel(US_dry)`
  /** barrel(imp) */
  def `bbl(imp)`: VolumeUnit = VolumeUnitObjects.`barrel(imp)`
  /** barrel(imp) */
  def `bl(imp)`: VolumeUnit = VolumeUnitObjects.`barrel(imp)`
  /** fluid barrel */
  def fl_bl: VolumeUnit = VolumeUnitObjects.fluid_barrel
  /** hogshead */
  def hhd: VolumeUnit = VolumeUnitObjects.hogshead
  /** hogshead(US)<br/>hogshead(imp) */
  def hhd(a: hogsheadAttribute): VolumeUnit = a match {
    case MetricAttributes.US => VolumeUnitObjects.`hogshead(US)`
    case MetricAttributes.imp => VolumeUnitObjects.`hogshead(imp)`
  }
  /** hogshead(US) */
  def `hhd(US)`: VolumeUnit = VolumeUnitObjects.`hogshead(US)`
  /** hogshead(imp) */
  def `hhd(imp)`: VolumeUnit = VolumeUnitObjects.`hogshead(imp)`
  /** fluid dram */
  def fl_dr: VolumeUnit = VolumeUnitObjects.fluid_dram
  /** fluid_dram(US)<br/>fluid_dram(imp) */
  def fl_dr(a: fluid_dramAttribute): VolumeUnit = a match {
    case MetricAttributes.US => VolumeUnitObjects.`fluid_dram(US)`
    case MetricAttributes.imp => VolumeUnitObjects.`fluid_dram(imp)`
  }
  /** fluid dram(US) */
  def `fl_dr(US)`: VolumeUnit = VolumeUnitObjects.`fluid_dram(US)`
  /** fluid dram(imp) */
  def `fl_dr(imp)`: VolumeUnit = VolumeUnitObjects.`fluid_dram(imp)`
  /** fluid scruple */
  def fl_s: VolumeUnit = VolumeUnitObjects.fluid_scruple
  /** bucket */
  def bkt: VolumeUnit = VolumeUnitObjects.bucket
}