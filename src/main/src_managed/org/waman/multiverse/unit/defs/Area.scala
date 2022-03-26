package org.waman.multiverse.unit.defs

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs.fluid._
import org.waman.multiverse.unit.defs.angle._
import org.waman.multiverse.unit.defs.radio.freq._
import org.waman.multiverse.Constants

class Area[A: Fractional](val value: A, val unit: AreaUnit)
    extends LinearQuantity[Area[A], A, AreaUnit] {

  override protected def newQuantity(value: A, unit: AreaUnit): Area[A] = new Area(value, unit)

  def /(time: Time[A]): KinematicViscosity[A] = new KinematicViscosity(this.value / time.value, this.unit / time.unit)

  def *(frequency: Frequency[A]): AreaFrequency[A] = new AreaFrequency(this.value * frequency.value, this.unit * frequency.unit)

  def *(length: Length[A]): Volume[A] = new Volume(this.value * length.value, this.unit * length.unit)
}

/** None */
trait AreaUnit extends LinearUnit[AreaUnit]{

  override def getSIUnit: AreaUnit = AreaUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AreaUnit.dimension

  def /(timeUnit: TimeUnit): KinematicViscosityUnit =
    new QuotientUnit[KinematicViscosityUnit, AreaUnit, TimeUnit](AreaUnit.this, timeUnit) with KinematicViscosityUnit

  def *(frequencyUnit: FrequencyUnit): AreaFrequencyUnit =
    new ProductUnit[AreaFrequencyUnit, AreaUnit, FrequencyUnit](AreaUnit.this, frequencyUnit) with AreaFrequencyUnit

  def *(lengthUnit: LengthUnit): VolumeUnit =
    new ProductUnit[VolumeUnit, AreaUnit, LengthUnit](AreaUnit.this, lengthUnit) with VolumeUnit
}

object AreaUnit extends UnitInfo[AreaUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](L -> 2).withDefaultValue(0)

  val getSIUnit: AreaUnit = LengthUnit.getSIUnit * LengthUnit.getSIUnit

  import AreaUnitObjects._

  def getUnits: Seq[AreaUnit] =
    Seq(square_metre, square_yoctometre, square_zeptometre, square_attometre, square_femtometre, square_picometre, square_nanometre, square_micrometre, square_millimetre, square_centimetre, square_decimetre, square_decametre, square_hectometre, square_kilometre, square_megametre, square_gigametre, square_terametre, square_petametre, square_exametre, square_zettametre, square_yottametre, are, hectare, barn, yoctobarn, zeptobarn, attobarn, femtobarn, picobarn, nanobarn, microbarn, millibarn, kilobarn, megabarn, gigabarn, terabarn, petabarn, exabarn, zettabarn, yottabarn, square_mil, square_inch, square_link, `square_link(US)`, square_foot, `square_foot(US)`, square_chain, `square_chain(US)`, square_yard, square_rod, `square_rod(US)`, square_mile, `square_mile(US)`, acre, `acre(US)`, rood, circular_mil, circular_inch, board, section, `section(US)`, township)
}


/** For no alias or user defined units */
class SimpleAreaUnit(val name: String, val symbol: String, val interval: Real) extends AreaUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAreaUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AreaUnit
  
object AreaUnitObjects{

  final case object square_metre extends LengthPoweredAreaUnit(LengthUnitObjects.metre, Seq("m2"))
  final case object square_yoctometre extends LengthPoweredAreaUnit(LengthUnitObjects.yoctometre, Seq("ym2"))
  final case object square_zeptometre extends LengthPoweredAreaUnit(LengthUnitObjects.zeptometre, Seq("zm2"))
  final case object square_attometre extends LengthPoweredAreaUnit(LengthUnitObjects.attometre, Seq("am2"))
  final case object square_femtometre extends LengthPoweredAreaUnit(LengthUnitObjects.femtometre, Seq("fm2"))
  final case object square_picometre extends LengthPoweredAreaUnit(LengthUnitObjects.picometre, Seq("pm2"))
  final case object square_nanometre extends LengthPoweredAreaUnit(LengthUnitObjects.nanometre, Seq("nm2"))
  final case object square_micrometre extends LengthPoweredAreaUnit(LengthUnitObjects.micrometre, Seq("mcm²", "μm2", "mcm2"))
  final case object square_millimetre extends LengthPoweredAreaUnit(LengthUnitObjects.millimetre, Seq("mm2"))
  final case object square_centimetre extends LengthPoweredAreaUnit(LengthUnitObjects.centimetre, Seq("cm2"))
  final case object square_decimetre extends LengthPoweredAreaUnit(LengthUnitObjects.decimetre, Seq("dm2"))
  final case object square_decametre extends LengthPoweredAreaUnit(LengthUnitObjects.decametre, Seq("dam2"))
  final case object square_hectometre extends LengthPoweredAreaUnit(LengthUnitObjects.hectometre, Seq("hm2"))
  final case object square_kilometre extends LengthPoweredAreaUnit(LengthUnitObjects.kilometre, Seq("Km²", "km2", "Km2"))
  final case object square_megametre extends LengthPoweredAreaUnit(LengthUnitObjects.megametre, Seq("Mm2"))
  final case object square_gigametre extends LengthPoweredAreaUnit(LengthUnitObjects.gigametre, Seq("Gm2"))
  final case object square_terametre extends LengthPoweredAreaUnit(LengthUnitObjects.terametre, Seq("Tm2"))
  final case object square_petametre extends LengthPoweredAreaUnit(LengthUnitObjects.petametre, Seq("Pm2"))
  final case object square_exametre extends LengthPoweredAreaUnit(LengthUnitObjects.exametre, Seq("Em2"))
  final case object square_zettametre extends LengthPoweredAreaUnit(LengthUnitObjects.zettametre, Seq("Zm2"))
  final case object square_yottametre extends LengthPoweredAreaUnit(LengthUnitObjects.yottametre, Seq("Ym2"))
  final case object are extends SimpleAreaUnit("are", "a", r"1e2")
  final case object hectare extends SimpleAreaUnit("hectare", "ha", r"1e4")
  final case object barn extends SimpleAreaUnit("barn", "b", r"1e-28")
  final case object yoctobarn extends SimpleAreaUnit("yoctobarn", "yb", r"1e-28" * r"1e-24")
  final case object zeptobarn extends SimpleAreaUnit("zeptobarn", "zb", r"1e-28" * r"1e-21")
  final case object attobarn extends SimpleAreaUnit("attobarn", "ab", r"1e-28" * r"1e-18")
  final case object femtobarn extends SimpleAreaUnit("femtobarn", "fb", r"1e-28" * r"1e-15")
  final case object picobarn extends SimpleAreaUnit("picobarn", "pb", r"1e-28" * r"1e-12")
  final case object nanobarn extends SimpleAreaUnit("nanobarn", "nb", r"1e-28" * r"1e-9")
  final case object microbarn extends DefaultAreaUnit("microbarn", "μb", Seq("mcb"), r"1e-28" * r"1e-6")
  final case object millibarn extends SimpleAreaUnit("millibarn", "mb", r"1e-28" * r"1e-3")
  final case object kilobarn extends DefaultAreaUnit("kilobarn", "kb", Seq("Kb"), r"1e-28" * r"1e3")
  final case object megabarn extends SimpleAreaUnit("megabarn", "Mb", r"1e-28" * r"1e6")
  final case object gigabarn extends SimpleAreaUnit("gigabarn", "Gb", r"1e-28" * r"1e9")
  final case object terabarn extends SimpleAreaUnit("terabarn", "Tb", r"1e-28" * r"1e12")
  final case object petabarn extends SimpleAreaUnit("petabarn", "Pb", r"1e-28" * r"1e15")
  final case object exabarn extends SimpleAreaUnit("exabarn", "Eb", r"1e-28" * r"1e18")
  final case object zettabarn extends SimpleAreaUnit("zettabarn", "Zb", r"1e-28" * r"1e21")
  final case object yottabarn extends SimpleAreaUnit("yottabarn", "Yb", r"1e-28" * r"1e24")
  final case object square_mil extends LengthPoweredAreaUnit(LengthUnitObjects.mil, Seq("mil2", "sq_mil"))
  final case object square_inch extends LengthPoweredAreaUnit(LengthUnitObjects.inch, Seq("in2", "sq_in"))
  final case object square_link extends LengthPoweredAreaUnit(LengthUnitObjects.link, Seq("li2", "lnk²", "lnk2", "sq_li", "sq_lnk"))
  final case object `square_link(US)` extends LengthPoweredAreaUnit(LengthUnitObjects.`link(US)`, Seq("li2(US)", "lnk²(US)", "lnk2(US)", "sq_li(US)", "sq_lnk(US)"))
  final case object square_foot extends LengthPoweredAreaUnit(LengthUnitObjects.foot, Seq("ft2", "sq_ft"))
  final case object `square_foot(US)` extends LengthPoweredAreaUnit(LengthUnitObjects.`foot(US)`, Seq("ft2(US)", "sq_ft(US)"))
  final case object square_chain extends LengthPoweredAreaUnit(LengthUnitObjects.chain, Seq("ch2", "sq_ch"))
  final case object `square_chain(US)` extends LengthPoweredAreaUnit(LengthUnitObjects.`chain(US)`, Seq("ch2(US)", "sq_ch(US)"))
  final case object square_yard extends LengthPoweredAreaUnit(LengthUnitObjects.yard, Seq("yd2", "sq_yd"))
  final case object square_rod extends LengthPoweredAreaUnit(LengthUnitObjects.rod, Seq("rd2", "sq_rd"))
  final case object `square_rod(US)` extends LengthPoweredAreaUnit(LengthUnitObjects.`rod(US)`, Seq("rd2(US)", "sq_rd(US)"))
  final case object square_mile extends LengthPoweredAreaUnit(LengthUnitObjects.mile, Seq("mi2", "sq_mi"))
  final case object `square_mile(US)` extends LengthPoweredAreaUnit(LengthUnitObjects.`mile(US)`, Seq("mi2(US)", "sq_mi(US)"))
  final case object acre extends SimpleAreaUnit("acre", "ac", r"10" * square_chain.interval)
  final case object `acre(US)` extends SimpleAreaUnit("acre(US)", "ac(US)", r"10" * `square_chain(US)`.interval)
  final case object rood extends SimpleAreaUnit("rood", "ro", r"1/4" * acre.interval)
  final case object circular_mil extends SimpleAreaUnit("circular mil", "circ_mil", Constants.Pi / r"4" * square_mil.interval)
  final case object circular_inch extends SimpleAreaUnit("circular inch", "circ_in", Constants.Pi / r"4" * square_inch.interval)
  final case object board extends SimpleAreaUnit("board", "bd", LengthUnitObjects.inch.interval * LengthUnitObjects.foot.interval)
  final case object section extends SimpleAreaUnit("section", "section", r"640" * acre.interval)
  final case object `section(US)` extends SimpleAreaUnit("section(US)", "section(US)", r"640" * `acre(US)`.interval)
  final case object township extends SimpleAreaUnit("township", "twp", r"36" * `section(US)`.interval)
}


object AreaUnits{

  /** square metre */
  def `m²`: LengthPoweredAreaUnit = AreaUnitObjects.square_metre
  /** square metre */
  def m2: LengthPoweredAreaUnit = AreaUnitObjects.square_metre
  /** square yoctometre */
  def `ym²`: LengthPoweredAreaUnit = AreaUnitObjects.square_yoctometre
  /** square yoctometre */
  def ym2: LengthPoweredAreaUnit = AreaUnitObjects.square_yoctometre
  /** square zeptometre */
  def `zm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_zeptometre
  /** square zeptometre */
  def zm2: LengthPoweredAreaUnit = AreaUnitObjects.square_zeptometre
  /** square attometre */
  def `am²`: LengthPoweredAreaUnit = AreaUnitObjects.square_attometre
  /** square attometre */
  def am2: LengthPoweredAreaUnit = AreaUnitObjects.square_attometre
  /** square femtometre */
  def `fm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_femtometre
  /** square femtometre */
  def fm2: LengthPoweredAreaUnit = AreaUnitObjects.square_femtometre
  /** square picometre */
  def `pm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_picometre
  /** square picometre */
  def pm2: LengthPoweredAreaUnit = AreaUnitObjects.square_picometre
  /** square nanometre */
  def `nm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_nanometre
  /** square nanometre */
  def nm2: LengthPoweredAreaUnit = AreaUnitObjects.square_nanometre
  /** square micrometre */
  def `μm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_micrometre
  /** square micrometre */
  def `mcm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_micrometre
  /** square micrometre */
  def μm2: LengthPoweredAreaUnit = AreaUnitObjects.square_micrometre
  /** square micrometre */
  def mcm2: LengthPoweredAreaUnit = AreaUnitObjects.square_micrometre
  /** square millimetre */
  def `mm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_millimetre
  /** square millimetre */
  def mm2: LengthPoweredAreaUnit = AreaUnitObjects.square_millimetre
  /** square centimetre */
  def `cm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_centimetre
  /** square centimetre */
  def cm2: LengthPoweredAreaUnit = AreaUnitObjects.square_centimetre
  /** square decimetre */
  def `dm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_decimetre
  /** square decimetre */
  def dm2: LengthPoweredAreaUnit = AreaUnitObjects.square_decimetre
  /** square decametre */
  def `dam²`: LengthPoweredAreaUnit = AreaUnitObjects.square_decametre
  /** square decametre */
  def dam2: LengthPoweredAreaUnit = AreaUnitObjects.square_decametre
  /** square hectometre */
  def `hm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_hectometre
  /** square hectometre */
  def hm2: LengthPoweredAreaUnit = AreaUnitObjects.square_hectometre
  /** square kilometre */
  def `km²`: LengthPoweredAreaUnit = AreaUnitObjects.square_kilometre
  /** square kilometre */
  def `Km²`: LengthPoweredAreaUnit = AreaUnitObjects.square_kilometre
  /** square kilometre */
  def km2: LengthPoweredAreaUnit = AreaUnitObjects.square_kilometre
  /** square kilometre */
  def Km2: LengthPoweredAreaUnit = AreaUnitObjects.square_kilometre
  /** square megametre */
  def `Mm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_megametre
  /** square megametre */
  def Mm2: LengthPoweredAreaUnit = AreaUnitObjects.square_megametre
  /** square gigametre */
  def `Gm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_gigametre
  /** square gigametre */
  def Gm2: LengthPoweredAreaUnit = AreaUnitObjects.square_gigametre
  /** square terametre */
  def `Tm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_terametre
  /** square terametre */
  def Tm2: LengthPoweredAreaUnit = AreaUnitObjects.square_terametre
  /** square petametre */
  def `Pm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_petametre
  /** square petametre */
  def Pm2: LengthPoweredAreaUnit = AreaUnitObjects.square_petametre
  /** square exametre */
  def `Em²`: LengthPoweredAreaUnit = AreaUnitObjects.square_exametre
  /** square exametre */
  def Em2: LengthPoweredAreaUnit = AreaUnitObjects.square_exametre
  /** square zettametre */
  def `Zm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_zettametre
  /** square zettametre */
  def Zm2: LengthPoweredAreaUnit = AreaUnitObjects.square_zettametre
  /** square yottametre */
  def `Ym²`: LengthPoweredAreaUnit = AreaUnitObjects.square_yottametre
  /** square yottametre */
  def Ym2: LengthPoweredAreaUnit = AreaUnitObjects.square_yottametre
  /** are */
  def a: AreaUnit = AreaUnitObjects.are
  /** hectare */
  def ha: AreaUnit = AreaUnitObjects.hectare
  /** barn */
  def b: AreaUnit = AreaUnitObjects.barn
  /** yoctobarn */
  def yb: AreaUnit = AreaUnitObjects.yoctobarn
  /** zeptobarn */
  def zb: AreaUnit = AreaUnitObjects.zeptobarn
  /** attobarn */
  def ab: AreaUnit = AreaUnitObjects.attobarn
  /** femtobarn */
  def fb: AreaUnit = AreaUnitObjects.femtobarn
  /** picobarn */
  def pb: AreaUnit = AreaUnitObjects.picobarn
  /** nanobarn */
  def nb: AreaUnit = AreaUnitObjects.nanobarn
  /** microbarn */
  def μb: AreaUnit = AreaUnitObjects.microbarn
  /** microbarn */
  def mcb: AreaUnit = AreaUnitObjects.microbarn
  /** millibarn */
  def mb: AreaUnit = AreaUnitObjects.millibarn
  /** kilobarn */
  def kb: AreaUnit = AreaUnitObjects.kilobarn
  /** kilobarn */
  def Kb: AreaUnit = AreaUnitObjects.kilobarn
  /** megabarn */
  def Mb: AreaUnit = AreaUnitObjects.megabarn
  /** gigabarn */
  def Gb: AreaUnit = AreaUnitObjects.gigabarn
  /** terabarn */
  def Tb: AreaUnit = AreaUnitObjects.terabarn
  /** petabarn */
  def Pb: AreaUnit = AreaUnitObjects.petabarn
  /** exabarn */
  def Eb: AreaUnit = AreaUnitObjects.exabarn
  /** zettabarn */
  def Zb: AreaUnit = AreaUnitObjects.zettabarn
  /** yottabarn */
  def Yb: AreaUnit = AreaUnitObjects.yottabarn
  /** square mil */
  def `mil²`: LengthPoweredAreaUnit = AreaUnitObjects.square_mil
  /** square mil */
  def mil2: LengthPoweredAreaUnit = AreaUnitObjects.square_mil
  /** square mil */
  def sq_mil: LengthPoweredAreaUnit = AreaUnitObjects.square_mil
  /** square inch */
  def `in²`: LengthPoweredAreaUnit = AreaUnitObjects.square_inch
  /** square inch */
  def in2: LengthPoweredAreaUnit = AreaUnitObjects.square_inch
  /** square inch */
  def sq_in: LengthPoweredAreaUnit = AreaUnitObjects.square_inch
  /** square link */
  def `li²`: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  /** square_link(US) */
  def `li²`(a: square_linkAttribute): LengthPoweredAreaUnit = a match {
    case MetricAttributes.US => AreaUnitObjects.`square_link(US)`
  }
  /** square link */
  def li2: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  /**   square_link(US) */
  def li2(a: square_linkAttribute): LengthPoweredAreaUnit = `li²`(a)
  /** square link */
  def `lnk²`: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  /**   square_link(US) */
  def `lnk²`(a: square_linkAttribute): LengthPoweredAreaUnit = `li²`(a)
  /** square link */
  def lnk2: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  /**   square_link(US) */
  def lnk2(a: square_linkAttribute): LengthPoweredAreaUnit = `li²`(a)
  /** square link */
  def sq_li: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  /**   square_link(US) */
  def sq_li(a: square_linkAttribute): LengthPoweredAreaUnit = `li²`(a)
  /** square link */
  def sq_lnk: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  /**   square_link(US) */
  def sq_lnk(a: square_linkAttribute): LengthPoweredAreaUnit = `li²`(a)
  /** square link(US) */
  def `li²(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_link(US)`
  /** square link(US) */
  def `li2(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_link(US)`
  /** square link(US) */
  def `lnk²(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_link(US)`
  /** square link(US) */
  def `lnk2(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_link(US)`
  /** square link(US) */
  def `sq_li(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_link(US)`
  /** square link(US) */
  def `sq_lnk(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_link(US)`
  /** square foot */
  def `ft²`: LengthPoweredAreaUnit = AreaUnitObjects.square_foot
  /** square_foot(US) */
  def `ft²`(a: square_footAttribute): LengthPoweredAreaUnit = a match {
    case MetricAttributes.US => AreaUnitObjects.`square_foot(US)`
  }
  /** square foot */
  def ft2: LengthPoweredAreaUnit = AreaUnitObjects.square_foot
  /**   square_foot(US) */
  def ft2(a: square_footAttribute): LengthPoweredAreaUnit = `ft²`(a)
  /** square foot */
  def sq_ft: LengthPoweredAreaUnit = AreaUnitObjects.square_foot
  /**   square_foot(US) */
  def sq_ft(a: square_footAttribute): LengthPoweredAreaUnit = `ft²`(a)
  /** square foot(US) */
  def `ft²(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_foot(US)`
  /** square foot(US) */
  def `ft2(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_foot(US)`
  /** square foot(US) */
  def `sq_ft(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_foot(US)`
  /** square chain */
  def `ch²`: LengthPoweredAreaUnit = AreaUnitObjects.square_chain
  /** square_chain(US) */
  def `ch²`(a: square_chainAttribute): LengthPoweredAreaUnit = a match {
    case MetricAttributes.US => AreaUnitObjects.`square_chain(US)`
  }
  /** square chain */
  def ch2: LengthPoweredAreaUnit = AreaUnitObjects.square_chain
  /**   square_chain(US) */
  def ch2(a: square_chainAttribute): LengthPoweredAreaUnit = `ch²`(a)
  /** square chain */
  def sq_ch: LengthPoweredAreaUnit = AreaUnitObjects.square_chain
  /**   square_chain(US) */
  def sq_ch(a: square_chainAttribute): LengthPoweredAreaUnit = `ch²`(a)
  /** square chain(US) */
  def `ch²(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_chain(US)`
  /** square chain(US) */
  def `ch2(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_chain(US)`
  /** square chain(US) */
  def `sq_ch(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_chain(US)`
  /** square yard */
  def `yd²`: LengthPoweredAreaUnit = AreaUnitObjects.square_yard
  /** square yard */
  def yd2: LengthPoweredAreaUnit = AreaUnitObjects.square_yard
  /** square yard */
  def sq_yd: LengthPoweredAreaUnit = AreaUnitObjects.square_yard
  /** square rod */
  def `rd²`: LengthPoweredAreaUnit = AreaUnitObjects.square_rod
  /** square_rod(US) */
  def `rd²`(a: square_rodAttribute): LengthPoweredAreaUnit = a match {
    case MetricAttributes.US => AreaUnitObjects.`square_rod(US)`
  }
  /** square rod */
  def rd2: LengthPoweredAreaUnit = AreaUnitObjects.square_rod
  /**   square_rod(US) */
  def rd2(a: square_rodAttribute): LengthPoweredAreaUnit = `rd²`(a)
  /** square rod */
  def sq_rd: LengthPoweredAreaUnit = AreaUnitObjects.square_rod
  /**   square_rod(US) */
  def sq_rd(a: square_rodAttribute): LengthPoweredAreaUnit = `rd²`(a)
  /** square rod(US) */
  def `rd²(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_rod(US)`
  /** square rod(US) */
  def `rd2(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_rod(US)`
  /** square rod(US) */
  def `sq_rd(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_rod(US)`
  /** square mile */
  def `mi²`: LengthPoweredAreaUnit = AreaUnitObjects.square_mile
  /** square_mile(US) */
  def `mi²`(a: square_mileAttribute): LengthPoweredAreaUnit = a match {
    case MetricAttributes.US => AreaUnitObjects.`square_mile(US)`
  }
  /** square mile */
  def mi2: LengthPoweredAreaUnit = AreaUnitObjects.square_mile
  /**   square_mile(US) */
  def mi2(a: square_mileAttribute): LengthPoweredAreaUnit = `mi²`(a)
  /** square mile */
  def sq_mi: LengthPoweredAreaUnit = AreaUnitObjects.square_mile
  /**   square_mile(US) */
  def sq_mi(a: square_mileAttribute): LengthPoweredAreaUnit = `mi²`(a)
  /** square mile(US) */
  def `mi²(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_mile(US)`
  /** square mile(US) */
  def `mi2(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_mile(US)`
  /** square mile(US) */
  def `sq_mi(US)`: LengthPoweredAreaUnit = AreaUnitObjects.`square_mile(US)`
  /** acre */
  def ac: AreaUnit = AreaUnitObjects.acre
  /** acre(US) */
  def ac(a: acreAttribute): AreaUnit = a match {
    case MetricAttributes.US => AreaUnitObjects.`acre(US)`
  }
  /** acre(US) */
  def `ac(US)`: AreaUnit = AreaUnitObjects.`acre(US)`
  /** rood */
  def ro: AreaUnit = AreaUnitObjects.rood
  /** circular mil */
  def circ_mil: AreaUnit = AreaUnitObjects.circular_mil
  /** circular inch */
  def circ_in: AreaUnit = AreaUnitObjects.circular_inch
  /** board */
  def bd: AreaUnit = AreaUnitObjects.board
  /** section */
  def section: AreaUnit = AreaUnitObjects.section
  /** section(US) */
  def section(a: sectionAttribute): AreaUnit = a match {
    case MetricAttributes.US => AreaUnitObjects.`section(US)`
  }
  /** section(US) */
  def `section(US)`: AreaUnit = AreaUnitObjects.`section(US)`
  /** township */
  def twp: AreaUnit = AreaUnitObjects.township
}