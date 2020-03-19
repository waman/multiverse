package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.angle.Frequency
import org.waman.multiverse.unit.angle.FrequencyUnit

import org.waman.multiverse.unit.radiometry.AreaFrequency
import org.waman.multiverse.unit.radiometry.AreaFrequencyUnit

import org.waman.multiverse.unit.fluid.KinematicViscosity
import org.waman.multiverse.unit.fluid.KinematicViscosityUnit


class Area[A: Fractional](val value: A, val unit: AreaUnit)
    extends LinearQuantity[Area[A], A, AreaUnit] {

  override protected def newQuantity(value: A, unit: AreaUnit): Area[A] = new Area(value, unit)

  def *(length: Length[A]): Volume[A] = new Volume(this.value * length.value, this.unit * length.unit)

  def *(frequency: Frequency[A]): AreaFrequency[A] = new AreaFrequency(this.value * frequency.value, this.unit * frequency.unit)

  def /(time: Time[A]): KinematicViscosity[A] = new KinematicViscosity(this.value / time.value, this.unit / time.unit)
}

/** null */
trait AreaUnit extends LinearUnit[AreaUnit]{

  override def getSIUnit: AreaUnit = AreaUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = AreaUnit.dimension

  def *(lengthUnit: LengthUnit): VolumeUnit =
    new AbstractProductUnit[VolumeUnit, AreaUnit, LengthUnit](AreaUnit.this, lengthUnit) with VolumeUnit

  def *(frequencyUnit: FrequencyUnit): AreaFrequencyUnit =
    new AbstractProductUnit[AreaFrequencyUnit, AreaUnit, FrequencyUnit](AreaUnit.this, frequencyUnit) with AreaFrequencyUnit

  def /(timeUnit: TimeUnit): KinematicViscosityUnit =
    new AbstractQuotientUnit[KinematicViscosityUnit, AreaUnit, TimeUnit](AreaUnit.this, timeUnit) with KinematicViscosityUnit
}

object AreaUnit extends UnitInfo[AreaUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](L -> 2).withDefaultValue(0)

  val getSIUnit: AreaUnit = LengthUnit.getSIUnit * LengthUnit.getSIUnit

  import AreaUnitObjects._
  def getUnits: Seq[AreaUnit] =
    Seq(square_metre, square_yoctometre, square_zeptometre, square_attometre, square_femtometre, square_picometre, square_nanometre, square_micrometre, square_millimetre, square_centimetre, square_decimetre, square_decametre, square_hectometre, square_kilometre, square_megametre, square_gigametre, square_terametre, square_petametre, square_exametre, square_zettametre, square_yottametre, are, hectare, barn, yoctobarn, zeptobarn, attobarn, femtobarn, picobarn, nanobarn, microbarn, millibarn, kilobarn, megabarn, gigabarn, terabarn, petabarn, exabarn, zettabarn, yottabarn, square_mil, square_inch, square_link, `square_link(US)`, square_foot, `square_foot(US)`, square_chain, `square_chain(US)`, square_yard, square_rod, square_mile, `square_mile(US)`, acre, `acre(US)`, rood, circular_mil, circular_inch, board)
}

/** For no aliase or user defined units */
class SimpleAreaUnit(val name: String, val symbol: String, val interval: Real) extends AreaUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultAreaUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends AreaUnit

object AreaUnitObjects{
  import org.waman.multiverse.unit.Constants
  import org.waman.multiverse.unit.basic.LengthUnitObjects._

  final case object square_metre extends DefaultAreaUnit("square metre", "m²", Seq("m2"), metre.interval**2)
  final case object square_yoctometre extends DefaultAreaUnit("square yoctometre", "ym²", Seq("ym2"), yoctometre.interval**2)
  final case object square_zeptometre extends DefaultAreaUnit("square zeptometre", "zm²", Seq("zm2"), zeptometre.interval**2)
  final case object square_attometre extends DefaultAreaUnit("square attometre", "am²", Seq("am2"), attometre.interval**2)
  final case object square_femtometre extends DefaultAreaUnit("square femtometre", "fm²", Seq("fm2"), femtometre.interval**2)
  final case object square_picometre extends DefaultAreaUnit("square picometre", "pm²", Seq("pm2"), picometre.interval**2)
  final case object square_nanometre extends DefaultAreaUnit("square nanometre", "nm²", Seq("nm2"), nanometre.interval**2)
  final case object square_micrometre extends DefaultAreaUnit("square micrometre", "μm²", Seq("μm2", "mcm²", "mcm2"), micrometre.interval**2)
  final case object square_millimetre extends DefaultAreaUnit("square millimetre", "mm²", Seq("mm2"), millimetre.interval**2)
  final case object square_centimetre extends DefaultAreaUnit("square centimetre", "cm²", Seq("cm2"), centimetre.interval**2)
  final case object square_decimetre extends DefaultAreaUnit("square decimetre", "dm²", Seq("dm2"), decimetre.interval**2)
  final case object square_decametre extends DefaultAreaUnit("square decametre", "dam²", Seq("dam2"), decametre.interval**2)
  final case object square_hectometre extends DefaultAreaUnit("square hectometre", "hm²", Seq("hm2"), hectometre.interval**2)
  final case object square_kilometre extends DefaultAreaUnit("square kilometre", "km²", Seq("km2", "Km²", "Km2"), kilometre.interval**2)
  final case object square_megametre extends DefaultAreaUnit("square megametre", "Mm²", Seq("Mm2"), megametre.interval**2)
  final case object square_gigametre extends DefaultAreaUnit("square gigametre", "Gm²", Seq("Gm2"), gigametre.interval**2)
  final case object square_terametre extends DefaultAreaUnit("square terametre", "Tm²", Seq("Tm2"), terametre.interval**2)
  final case object square_petametre extends DefaultAreaUnit("square petametre", "Pm²", Seq("Pm2"), petametre.interval**2)
  final case object square_exametre extends DefaultAreaUnit("square exametre", "Em²", Seq("Em2"), exametre.interval**2)
  final case object square_zettametre extends DefaultAreaUnit("square zettametre", "Zm²", Seq("Zm2"), zettametre.interval**2)
  final case object square_yottametre extends DefaultAreaUnit("square yottametre", "Ym²", Seq("Ym2"), yottametre.interval**2)
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
  final case object square_mil extends DefaultAreaUnit("square mil", "mil²", Seq("mil2", "sq_mil"), mil.interval**2)
  final case object square_inch extends DefaultAreaUnit("square inch", "in²", Seq("in2", "sq_in"), inch.interval**2)
  final case object square_link extends DefaultAreaUnit("square link", "li²", Seq("li2", "lnk²", "lnk2", "sq_li", "sq_lnk"), link.interval**2)
  final case object `square_link(US)` extends DefaultAreaUnit("square link(US)", "li²(US)", Seq("li2(US)", "lnk²(US)", "lnk2(US)", "sq_li(US)", "sq_lnk(US)"), `link(US)`.interval**2)
  final case object square_foot extends DefaultAreaUnit("square foot", "ft²", Seq("ft2", "sq_ft"), foot.interval**2)
  final case object `square_foot(US)` extends DefaultAreaUnit("square foot(US)", "ft²(US)", Seq("ft2(US)", "sq_ft(US)"), `foot(US)`.interval**2)
  final case object square_chain extends DefaultAreaUnit("square chain", "ch²", Seq("ch2", "sq_ch"), chain.interval**2)
  final case object `square_chain(US)` extends DefaultAreaUnit("square chain(US)", "ch²(US)", Seq("ch2(US)", "sq_ch(US)"), `chain(US)`.interval**2)
  final case object square_yard extends DefaultAreaUnit("square yard", "yd²", Seq("yd2", "sq_yd"), yard.interval**2)
  final case object square_rod extends DefaultAreaUnit("square rod", "rd²", Seq("rd2", "sq_rd"), rod.interval**2)
  final case object square_mile extends DefaultAreaUnit("square mile", "mi²", Seq("mi2", "sq_mi"), mile.interval**2)
  final case object `square_mile(US)` extends DefaultAreaUnit("square mile(US)", "mi²(US)", Seq("mi2(US)", "sq_mi(US)"), `mile(US)`.interval**2)
  final case object acre extends SimpleAreaUnit("acre", "ac", r"10" * square_chain.interval)
  final case object `acre(US)` extends SimpleAreaUnit("acre(US)", "ac(US)", r"10" * `square_chain(US)`.interval)
  final case object rood extends SimpleAreaUnit("rood", "ro", r"1"/r"4" * acre.interval)
  final case object circular_mil extends SimpleAreaUnit("circular mil", "circ_mil", Constants.Pi / r"4" * square_mil.interval)
  final case object circular_inch extends SimpleAreaUnit("circular inch", "circ_in", Constants.Pi / r"4" * square_inch.interval)
  final case object board extends SimpleAreaUnit("board", "bd", inch.interval * foot.interval)
}

object AreaUnits{
  def `m²`: AreaUnit = AreaUnitObjects.square_metre
  def m2: AreaUnit = AreaUnitObjects.square_metre
  def `ym²`: AreaUnit = AreaUnitObjects.square_yoctometre
  def ym2: AreaUnit = AreaUnitObjects.square_yoctometre
  def `zm²`: AreaUnit = AreaUnitObjects.square_zeptometre
  def zm2: AreaUnit = AreaUnitObjects.square_zeptometre
  def `am²`: AreaUnit = AreaUnitObjects.square_attometre
  def am2: AreaUnit = AreaUnitObjects.square_attometre
  def `fm²`: AreaUnit = AreaUnitObjects.square_femtometre
  def fm2: AreaUnit = AreaUnitObjects.square_femtometre
  def `pm²`: AreaUnit = AreaUnitObjects.square_picometre
  def pm2: AreaUnit = AreaUnitObjects.square_picometre
  def `nm²`: AreaUnit = AreaUnitObjects.square_nanometre
  def nm2: AreaUnit = AreaUnitObjects.square_nanometre
  def `μm²`: AreaUnit = AreaUnitObjects.square_micrometre
  def `μm2`: AreaUnit = AreaUnitObjects.square_micrometre
  def `mcm²`: AreaUnit = AreaUnitObjects.square_micrometre
  def mcm2: AreaUnit = AreaUnitObjects.square_micrometre
  def `mm²`: AreaUnit = AreaUnitObjects.square_millimetre
  def mm2: AreaUnit = AreaUnitObjects.square_millimetre
  def `cm²`: AreaUnit = AreaUnitObjects.square_centimetre
  def cm2: AreaUnit = AreaUnitObjects.square_centimetre
  def `dm²`: AreaUnit = AreaUnitObjects.square_decimetre
  def dm2: AreaUnit = AreaUnitObjects.square_decimetre
  def `dam²`: AreaUnit = AreaUnitObjects.square_decametre
  def dam2: AreaUnit = AreaUnitObjects.square_decametre
  def `hm²`: AreaUnit = AreaUnitObjects.square_hectometre
  def hm2: AreaUnit = AreaUnitObjects.square_hectometre
  def `km²`: AreaUnit = AreaUnitObjects.square_kilometre
  def km2: AreaUnit = AreaUnitObjects.square_kilometre
  def `Km²`: AreaUnit = AreaUnitObjects.square_kilometre
  def Km2: AreaUnit = AreaUnitObjects.square_kilometre
  def `Mm²`: AreaUnit = AreaUnitObjects.square_megametre
  def Mm2: AreaUnit = AreaUnitObjects.square_megametre
  def `Gm²`: AreaUnit = AreaUnitObjects.square_gigametre
  def Gm2: AreaUnit = AreaUnitObjects.square_gigametre
  def `Tm²`: AreaUnit = AreaUnitObjects.square_terametre
  def Tm2: AreaUnit = AreaUnitObjects.square_terametre
  def `Pm²`: AreaUnit = AreaUnitObjects.square_petametre
  def Pm2: AreaUnit = AreaUnitObjects.square_petametre
  def `Em²`: AreaUnit = AreaUnitObjects.square_exametre
  def Em2: AreaUnit = AreaUnitObjects.square_exametre
  def `Zm²`: AreaUnit = AreaUnitObjects.square_zettametre
  def Zm2: AreaUnit = AreaUnitObjects.square_zettametre
  def `Ym²`: AreaUnit = AreaUnitObjects.square_yottametre
  def Ym2: AreaUnit = AreaUnitObjects.square_yottametre
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
  def `mil²`: AreaUnit = AreaUnitObjects.square_mil
  def mil2: AreaUnit = AreaUnitObjects.square_mil
  def sq_mil: AreaUnit = AreaUnitObjects.square_mil
  def `in²`: AreaUnit = AreaUnitObjects.square_inch
  def in2: AreaUnit = AreaUnitObjects.square_inch
  def sq_in: AreaUnit = AreaUnitObjects.square_inch
  def `li²`: AreaUnit = AreaUnitObjects.square_link
  def `li²`(a: square_linkAttribute): AreaUnit = a match { 
    case LengthAttributes.US => AreaUnitObjects.`square_link(US)`
  }
  def li2: AreaUnit = AreaUnitObjects.square_link
  def li2(a: square_linkAttribute): AreaUnit = `li²`(a)

  def `lnk²`: AreaUnit = AreaUnitObjects.square_link
  def `lnk²`(a: square_linkAttribute): AreaUnit = `li²`(a)

  def lnk2: AreaUnit = AreaUnitObjects.square_link
  def lnk2(a: square_linkAttribute): AreaUnit = `li²`(a)

  def sq_li: AreaUnit = AreaUnitObjects.square_link
  def sq_lnk: AreaUnit = AreaUnitObjects.square_link
  def `ft²`: AreaUnit = AreaUnitObjects.square_foot
  def `ft²`(a: square_footAttribute): AreaUnit = a match { 
    case LengthAttributes.US => AreaUnitObjects.`square_foot(US)`
  }
  def ft2: AreaUnit = AreaUnitObjects.square_foot
  def ft2(a: square_footAttribute): AreaUnit = `ft²`(a)

  def sq_ft: AreaUnit = AreaUnitObjects.square_foot
  def `ch²`: AreaUnit = AreaUnitObjects.square_chain
  def `ch²`(a: square_chainAttribute): AreaUnit = a match { 
    case LengthAttributes.US => AreaUnitObjects.`square_chain(US)`
  }
  def ch2: AreaUnit = AreaUnitObjects.square_chain
  def ch2(a: square_chainAttribute): AreaUnit = `ch²`(a)

  def sq_ch: AreaUnit = AreaUnitObjects.square_chain
  def `yd²`: AreaUnit = AreaUnitObjects.square_yard
  def yd2: AreaUnit = AreaUnitObjects.square_yard
  def sq_yd: AreaUnit = AreaUnitObjects.square_yard
  def `rd²`: AreaUnit = AreaUnitObjects.square_rod
  def rd2: AreaUnit = AreaUnitObjects.square_rod
  def sq_rd: AreaUnit = AreaUnitObjects.square_rod
  def `mi²`: AreaUnit = AreaUnitObjects.square_mile
  def `mi²`(a: square_mileAttribute): AreaUnit = a match { 
    case LengthAttributes.US => AreaUnitObjects.`square_mile(US)`
  }
  def mi2: AreaUnit = AreaUnitObjects.square_mile
  def mi2(a: square_mileAttribute): AreaUnit = `mi²`(a)

  def sq_mi: AreaUnit = AreaUnitObjects.square_mile
  def ac: AreaUnit = AreaUnitObjects.acre
  def ac(a: acreAttribute): AreaUnit = a match { 
    case LengthAttributes.US => AreaUnitObjects.`acre(US)`
  }
  def ro: AreaUnit = AreaUnitObjects.rood
  def circ_mil: AreaUnit = AreaUnitObjects.circular_mil
  def circ_in: AreaUnit = AreaUnitObjects.circular_inch
  def bd: AreaUnit = AreaUnitObjects.board
}