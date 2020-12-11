package multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional

import multiverse._


import multiverse.unit.angle.Frequency
import multiverse.unit.angle.FrequencyUnit


import multiverse.unit.radiometry.AreaFrequency
import multiverse.unit.radiometry.AreaFrequencyUnit


import multiverse.unit.fluid.KinematicViscosity
import multiverse.unit.fluid.KinematicViscosityUnit


class Area[A: Fractional](val value: A, val unit: AreaUnit)
    extends LinearQuantity[Area[A], A, AreaUnit] {

  import spire.implicits._

  override protected def newQuantity(value: A, unit: AreaUnit): Area[A] = new Area(value, unit)

  def *(length: Length[A]): Volume[A] = new Volume(this.value * length.value, this.unit * length.unit)

  def *(frequency: Frequency[A]): AreaFrequency[A] = new AreaFrequency(this.value * frequency.value, this.unit * frequency.unit)

  def /(time: Time[A]): KinematicViscosity[A] = new KinematicViscosity(this.value / time.value, this.unit / time.unit)
}

trait AreaUnit extends LinearUnit[AreaUnit]{

  override def getSIUnit: AreaUnit = AreaUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AreaUnit.dimension

  def *(lengthUnit: LengthUnit): VolumeUnit =
    new ProductUnit[VolumeUnit, AreaUnit, LengthUnit](AreaUnit.this, lengthUnit) with VolumeUnit

  def *(frequencyUnit: FrequencyUnit): AreaFrequencyUnit =
    new ProductUnit[AreaFrequencyUnit, AreaUnit, FrequencyUnit](AreaUnit.this, frequencyUnit) with AreaFrequencyUnit

  def /(timeUnit: TimeUnit): KinematicViscosityUnit =
    new QuotientUnit[KinematicViscosityUnit, AreaUnit, TimeUnit](AreaUnit.this, timeUnit) with KinematicViscosityUnit
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

/** For no aliase or user defined units */
class SimpleAreaUnit(val name: String, val symbol: String, val interval: Real) extends AreaUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAreaUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AreaUnit

object AreaUnitObjects{

  import spire.implicits._

  import multiverse.unit.Constants
  import multiverse.unit.basic.LengthUnitObjects._

  final case object square_metre extends LengthPoweredAreaUnit(metre, Seq("m2"))
  final case object square_yoctometre extends LengthPoweredAreaUnit(yoctometre, Seq("ym2"))
  final case object square_zeptometre extends LengthPoweredAreaUnit(zeptometre, Seq("zm2"))
  final case object square_attometre extends LengthPoweredAreaUnit(attometre, Seq("am2"))
  final case object square_femtometre extends LengthPoweredAreaUnit(femtometre, Seq("fm2"))
  final case object square_picometre extends LengthPoweredAreaUnit(picometre, Seq("pm2"))
  final case object square_nanometre extends LengthPoweredAreaUnit(nanometre, Seq("nm2"))
  final case object square_micrometre extends LengthPoweredAreaUnit(micrometre, Seq("μm2", "mcm²", "mcm2"))
  final case object square_millimetre extends LengthPoweredAreaUnit(millimetre, Seq("mm2"))
  final case object square_centimetre extends LengthPoweredAreaUnit(centimetre, Seq("cm2"))
  final case object square_decimetre extends LengthPoweredAreaUnit(decimetre, Seq("dm2"))
  final case object square_decametre extends LengthPoweredAreaUnit(decametre, Seq("dam2"))
  final case object square_hectometre extends LengthPoweredAreaUnit(hectometre, Seq("hm2"))
  final case object square_kilometre extends LengthPoweredAreaUnit(kilometre, Seq("km2", "Km²", "Km2"))
  final case object square_megametre extends LengthPoweredAreaUnit(megametre, Seq("Mm2"))
  final case object square_gigametre extends LengthPoweredAreaUnit(gigametre, Seq("Gm2"))
  final case object square_terametre extends LengthPoweredAreaUnit(terametre, Seq("Tm2"))
  final case object square_petametre extends LengthPoweredAreaUnit(petametre, Seq("Pm2"))
  final case object square_exametre extends LengthPoweredAreaUnit(exametre, Seq("Em2"))
  final case object square_zettametre extends LengthPoweredAreaUnit(zettametre, Seq("Zm2"))
  final case object square_yottametre extends LengthPoweredAreaUnit(yottametre, Seq("Ym2"))
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
  final case object square_mil extends LengthPoweredAreaUnit(mil, Seq("mil2", "sq_mil"))
  final case object square_inch extends LengthPoweredAreaUnit(inch, Seq("in2", "sq_in"))
  final case object square_link extends LengthPoweredAreaUnit(link, Seq("li2", "lnk²", "lnk2", "sq_li", "sq_lnk"))
  final case object `square_link(US)` extends LengthPoweredAreaUnit(`link(US)`, Seq("li2(US)", "lnk²(US)", "lnk2(US)", "sq_li(US)", "sq_lnk(US)"))
  final case object square_foot extends LengthPoweredAreaUnit(foot, Seq("ft2", "sq_ft"))
  final case object `square_foot(US)` extends LengthPoweredAreaUnit(`foot(US)`, Seq("ft2(US)", "sq_ft(US)"))
  final case object square_chain extends LengthPoweredAreaUnit(chain, Seq("ch2", "sq_ch"))
  final case object `square_chain(US)` extends LengthPoweredAreaUnit(`chain(US)`, Seq("ch2(US)", "sq_ch(US)"))
  final case object square_yard extends LengthPoweredAreaUnit(yard, Seq("yd2", "sq_yd"))
  final case object square_rod extends LengthPoweredAreaUnit(rod, Seq("rd2", "sq_rd"))
  final case object `square_rod(US)` extends LengthPoweredAreaUnit(`rod(US)`, Seq("rd2(US)", "sq_rd(US)"))
  final case object square_mile extends LengthPoweredAreaUnit(mile, Seq("mi2", "sq_mi"))
  final case object `square_mile(US)` extends LengthPoweredAreaUnit(`mile(US)`, Seq("mi2(US)", "sq_mi(US)"))
  final case object acre extends SimpleAreaUnit("acre", "ac", r"10" * square_chain.interval)
  final case object `acre(US)` extends SimpleAreaUnit("acre(US)", "ac(US)", r"10" * `square_chain(US)`.interval)
  final case object rood extends SimpleAreaUnit("rood", "ro", r"1"/r"4" * acre.interval)
  final case object circular_mil extends SimpleAreaUnit("circular mil", "circ_mil", Constants.Pi / r"4" * square_mil.interval)
  final case object circular_inch extends SimpleAreaUnit("circular inch", "circ_in", Constants.Pi / r"4" * square_inch.interval)
  final case object board extends SimpleAreaUnit("board", "bd", inch.interval * foot.interval)
  final case object section extends SimpleAreaUnit("section", "section", r"640" * acre.interval)
  final case object `section(US)` extends SimpleAreaUnit("section(US)", "section(US)", r"640" * `acre(US)`.interval)
  final case object township extends SimpleAreaUnit("township", "twp", r"36" * `section(US)`.interval)
}

object AreaUnits{

  def `m²`: LengthPoweredAreaUnit = AreaUnitObjects.square_metre
  def m2: LengthPoweredAreaUnit = AreaUnitObjects.square_metre
  def `ym²`: LengthPoweredAreaUnit = AreaUnitObjects.square_yoctometre
  def ym2: LengthPoweredAreaUnit = AreaUnitObjects.square_yoctometre
  def `zm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_zeptometre
  def zm2: LengthPoweredAreaUnit = AreaUnitObjects.square_zeptometre
  def `am²`: LengthPoweredAreaUnit = AreaUnitObjects.square_attometre
  def am2: LengthPoweredAreaUnit = AreaUnitObjects.square_attometre
  def `fm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_femtometre
  def fm2: LengthPoweredAreaUnit = AreaUnitObjects.square_femtometre
  def `pm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_picometre
  def pm2: LengthPoweredAreaUnit = AreaUnitObjects.square_picometre
  def `nm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_nanometre
  def nm2: LengthPoweredAreaUnit = AreaUnitObjects.square_nanometre
  def `μm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_micrometre
  def `μm2`: LengthPoweredAreaUnit = AreaUnitObjects.square_micrometre
  def `mcm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_micrometre
  def mcm2: LengthPoweredAreaUnit = AreaUnitObjects.square_micrometre
  def `mm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_millimetre
  def mm2: LengthPoweredAreaUnit = AreaUnitObjects.square_millimetre
  def `cm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_centimetre
  def cm2: LengthPoweredAreaUnit = AreaUnitObjects.square_centimetre
  def `dm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_decimetre
  def dm2: LengthPoweredAreaUnit = AreaUnitObjects.square_decimetre
  def `dam²`: LengthPoweredAreaUnit = AreaUnitObjects.square_decametre
  def dam2: LengthPoweredAreaUnit = AreaUnitObjects.square_decametre
  def `hm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_hectometre
  def hm2: LengthPoweredAreaUnit = AreaUnitObjects.square_hectometre
  def `km²`: LengthPoweredAreaUnit = AreaUnitObjects.square_kilometre
  def km2: LengthPoweredAreaUnit = AreaUnitObjects.square_kilometre
  def `Km²`: LengthPoweredAreaUnit = AreaUnitObjects.square_kilometre
  def Km2: LengthPoweredAreaUnit = AreaUnitObjects.square_kilometre
  def `Mm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_megametre
  def Mm2: LengthPoweredAreaUnit = AreaUnitObjects.square_megametre
  def `Gm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_gigametre
  def Gm2: LengthPoweredAreaUnit = AreaUnitObjects.square_gigametre
  def `Tm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_terametre
  def Tm2: LengthPoweredAreaUnit = AreaUnitObjects.square_terametre
  def `Pm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_petametre
  def Pm2: LengthPoweredAreaUnit = AreaUnitObjects.square_petametre
  def `Em²`: LengthPoweredAreaUnit = AreaUnitObjects.square_exametre
  def Em2: LengthPoweredAreaUnit = AreaUnitObjects.square_exametre
  def `Zm²`: LengthPoweredAreaUnit = AreaUnitObjects.square_zettametre
  def Zm2: LengthPoweredAreaUnit = AreaUnitObjects.square_zettametre
  def `Ym²`: LengthPoweredAreaUnit = AreaUnitObjects.square_yottametre
  def Ym2: LengthPoweredAreaUnit = AreaUnitObjects.square_yottametre
  def a: AreaUnit = AreaUnitObjects.are
  def ha: AreaUnit = AreaUnitObjects.hectare
  def b: AreaUnit = AreaUnitObjects.barn
  def yb: AreaUnit = AreaUnitObjects.yoctobarn
  def zb: AreaUnit = AreaUnitObjects.zeptobarn
  def ab: AreaUnit = AreaUnitObjects.attobarn
  def fb: AreaUnit = AreaUnitObjects.femtobarn
  def pb: AreaUnit = AreaUnitObjects.picobarn
  def nb: AreaUnit = AreaUnitObjects.nanobarn
  def `μb`: AreaUnit = AreaUnitObjects.microbarn
  def mcb: AreaUnit = AreaUnitObjects.microbarn
  def mb: AreaUnit = AreaUnitObjects.millibarn
  def kb: AreaUnit = AreaUnitObjects.kilobarn
  def Kb: AreaUnit = AreaUnitObjects.kilobarn
  def Mb: AreaUnit = AreaUnitObjects.megabarn
  def Gb: AreaUnit = AreaUnitObjects.gigabarn
  def Tb: AreaUnit = AreaUnitObjects.terabarn
  def Pb: AreaUnit = AreaUnitObjects.petabarn
  def Eb: AreaUnit = AreaUnitObjects.exabarn
  def Zb: AreaUnit = AreaUnitObjects.zettabarn
  def Yb: AreaUnit = AreaUnitObjects.yottabarn
  def `mil²`: LengthPoweredAreaUnit = AreaUnitObjects.square_mil
  def mil2: LengthPoweredAreaUnit = AreaUnitObjects.square_mil
  def sq_mil: LengthPoweredAreaUnit = AreaUnitObjects.square_mil
  def `in²`: LengthPoweredAreaUnit = AreaUnitObjects.square_inch
  def in2: LengthPoweredAreaUnit = AreaUnitObjects.square_inch
  def sq_in: LengthPoweredAreaUnit = AreaUnitObjects.square_inch
  def `li²`: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  def `li²`(a: square_linkAttribute): LengthPoweredAreaUnit = a match { 
    case MetricAttributes.US => AreaUnitObjects.`square_link(US)`
  }
  def li2: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  def li2(a: square_linkAttribute): LengthPoweredAreaUnit = `li²`(a)

  def `lnk²`: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  def `lnk²`(a: square_linkAttribute): LengthPoweredAreaUnit = `li²`(a)

  def lnk2: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  def lnk2(a: square_linkAttribute): LengthPoweredAreaUnit = `li²`(a)

  def sq_li: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  def sq_li(a: square_linkAttribute): LengthPoweredAreaUnit = `li²`(a)

  def sq_lnk: LengthPoweredAreaUnit = AreaUnitObjects.square_link
  def sq_lnk(a: square_linkAttribute): LengthPoweredAreaUnit = `li²`(a)

  def `ft²`: LengthPoweredAreaUnit = AreaUnitObjects.square_foot
  def `ft²`(a: square_footAttribute): LengthPoweredAreaUnit = a match { 
    case MetricAttributes.US => AreaUnitObjects.`square_foot(US)`
  }
  def ft2: LengthPoweredAreaUnit = AreaUnitObjects.square_foot
  def ft2(a: square_footAttribute): LengthPoweredAreaUnit = `ft²`(a)

  def sq_ft: LengthPoweredAreaUnit = AreaUnitObjects.square_foot
  def sq_ft(a: square_footAttribute): LengthPoweredAreaUnit = `ft²`(a)

  def `ch²`: LengthPoweredAreaUnit = AreaUnitObjects.square_chain
  def `ch²`(a: square_chainAttribute): LengthPoweredAreaUnit = a match { 
    case MetricAttributes.US => AreaUnitObjects.`square_chain(US)`
  }
  def ch2: LengthPoweredAreaUnit = AreaUnitObjects.square_chain
  def ch2(a: square_chainAttribute): LengthPoweredAreaUnit = `ch²`(a)

  def sq_ch: LengthPoweredAreaUnit = AreaUnitObjects.square_chain
  def sq_ch(a: square_chainAttribute): LengthPoweredAreaUnit = `ch²`(a)

  def `yd²`: LengthPoweredAreaUnit = AreaUnitObjects.square_yard
  def yd2: LengthPoweredAreaUnit = AreaUnitObjects.square_yard
  def sq_yd: LengthPoweredAreaUnit = AreaUnitObjects.square_yard
  def `rd²`: LengthPoweredAreaUnit = AreaUnitObjects.square_rod
  def `rd²`(a: square_rodAttribute): LengthPoweredAreaUnit = a match { 
    case MetricAttributes.US => AreaUnitObjects.`square_rod(US)`
  }
  def rd2: LengthPoweredAreaUnit = AreaUnitObjects.square_rod
  def rd2(a: square_rodAttribute): LengthPoweredAreaUnit = `rd²`(a)

  def sq_rd: LengthPoweredAreaUnit = AreaUnitObjects.square_rod
  def sq_rd(a: square_rodAttribute): LengthPoweredAreaUnit = `rd²`(a)

  def `mi²`: LengthPoweredAreaUnit = AreaUnitObjects.square_mile
  def `mi²`(a: square_mileAttribute): LengthPoweredAreaUnit = a match { 
    case MetricAttributes.US => AreaUnitObjects.`square_mile(US)`
  }
  def mi2: LengthPoweredAreaUnit = AreaUnitObjects.square_mile
  def mi2(a: square_mileAttribute): LengthPoweredAreaUnit = `mi²`(a)

  def sq_mi: LengthPoweredAreaUnit = AreaUnitObjects.square_mile
  def sq_mi(a: square_mileAttribute): LengthPoweredAreaUnit = `mi²`(a)

  def ac: AreaUnit = AreaUnitObjects.acre
  def ac(a: acreAttribute): AreaUnit = a match { 
    case MetricAttributes.US => AreaUnitObjects.`acre(US)`
  }
  def ro: AreaUnit = AreaUnitObjects.rood
  def circ_mil: AreaUnit = AreaUnitObjects.circular_mil
  def circ_in: AreaUnit = AreaUnitObjects.circular_inch
  def bd: AreaUnit = AreaUnitObjects.board
  def section: AreaUnit = AreaUnitObjects.section
  def section(a: sectionAttribute): AreaUnit = a match { 
    case MetricAttributes.US => AreaUnitObjects.`section(US)`
  }
  def twp: AreaUnit = AreaUnitObjects.township
}