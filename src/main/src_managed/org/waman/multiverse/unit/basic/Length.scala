package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional

import org.waman.multiverse._


import org.waman.multiverse.unit.mechanics.Force
import org.waman.multiverse.unit.mechanics.ForceUnit


import org.waman.multiverse.unit.mechanics.Energy
import org.waman.multiverse.unit.mechanics.EnergyUnit


import org.waman.multiverse.unit.mechanics.TimeSquared
import org.waman.multiverse.unit.mechanics.TimeSquaredUnit


import org.waman.multiverse.unit.mechanics.Acceleration
import org.waman.multiverse.unit.mechanics.AccelerationUnit


class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends LinearQuantity[Length[A], A, LengthUnit] {

  import spire.implicits._

  override protected def newQuantity(value: A, unit: LengthUnit): Length[A] = new Length(value, unit)

  def *(length: Length[A]): Area[A] = new Area(this.value * length.value, this.unit * length.unit)
  def squared: Area[A] = this * this
  def cubic: Volume[A] = this * this * this


  def *(force: Force[A]): Energy[A] = new Energy(this.value * force.value, this.unit * force.unit)

  def /(time: Time[A]): Velocity[A] = new Velocity(this.value / time.value, this.unit / time.unit)

  def /(timeSquared: TimeSquared[A]): Acceleration[A] = new Acceleration(this.value / timeSquared.value, this.unit / timeSquared.unit)
}

/** 'US' attribute contains 'US Survey' metric. */
trait LengthUnit extends LinearUnit[LengthUnit]{

  override def getSIUnit: LengthUnit = LengthUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = LengthUnit.dimension
  def squared: AreaUnit =
    new AreaUnit{
      override val name: String = "square " + LengthUnit.this.name
      override val symbol: String = LengthUnit.this.symbol + "²"
      override val interval: Real = LengthUnit.this.interval**2
      override def aliases: Seq[String] = LengthUnit.this.symbols.map(_+".squared")

      override def *(lengthUnit: LengthUnit): VolumeUnit = {
        if (lengthUnit == LengthUnit.this)
          LengthUnit.this.cubic
        else
          super.*(lengthUnit)
      }
    }

  def cubic: VolumeUnit =
    new VolumeUnit{
      override val name: String = "cubic " + LengthUnit.this.name
      override val symbol: String = LengthUnit.this.symbol + "³"
      override val interval: Real = LengthUnit.this.interval**3
      override def aliases: Seq[String] = LengthUnit.this.symbols.map(_+".cubic")
    }

  def *(lengthUnit: LengthUnit): AreaUnit =
    if(this == lengthUnit)
      this.squared
    else
      new AbstractProductUnit[AreaUnit, LengthUnit, LengthUnit](LengthUnit.this, lengthUnit) with AreaUnit


  def *(forceUnit: ForceUnit): EnergyUnit =
    new AbstractProductUnit[EnergyUnit, LengthUnit, ForceUnit](LengthUnit.this, forceUnit) with EnergyUnit

  def /(timeUnit: TimeUnit): VelocityUnit =
    new AbstractQuotientUnit[VelocityUnit, LengthUnit, TimeUnit](LengthUnit.this, timeUnit) with VelocityUnit

  def /(timeSquaredUnit: TimeSquaredUnit): AccelerationUnit =
    new AbstractQuotientUnit[AccelerationUnit, LengthUnit, TimeSquaredUnit](LengthUnit.this, timeSquaredUnit) with AccelerationUnit
}

object LengthUnit extends UnitInfo[LengthUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](L -> 1).withDefaultValue(0)

  def getSIUnit: LengthUnit = LengthUnitObjects.metre

  import LengthUnitObjects._
  def getUnits: Seq[LengthUnit] =
    Seq(metre, yoctometre, zeptometre, attometre, femtometre, picometre, nanometre, micrometre, millimetre, centimetre, decimetre, decametre, hectometre, kilometre, megametre, gigametre, terametre, petametre, exametre, zettametre, yottametre, micron, angstrom, atomic_unit_of_length, xunit, `xunit(CuKα1)`, `xunit(MoKα1)`, planck_length, astronomical_unit, light_year, parsec, mil, twip, point, pica, line, inch, link, `link(US)`, foot, `foot(US)`, yard, ell, fathom, rod, `rod(US)`, rope, chain, `chain(US)`, mile, `mile(US)`, cable, `cable(US)`, `cable(imp)`, league, `league(US)`, nautical_mile, `nautical_mile(Adm)`, nautical_league, metric_foot, short_metric_foot, long_metric_foot, french, furlong, `furlong(US)`)
}

/** For no aliase or user defined units */
class SimpleLengthUnit(val name: String, val symbol: String, val interval: Real) extends LengthUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultLengthUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LengthUnit

sealed trait square_linkAttribute
sealed trait square_footAttribute
sealed trait square_chainAttribute
sealed trait square_rodAttribute
sealed trait square_mileAttribute
sealed trait acreAttribute
sealed trait sectionAttribute
sealed trait xunitAttribute
sealed trait linkAttribute
sealed trait footAttribute
sealed trait rodAttribute
sealed trait chainAttribute
sealed trait mileAttribute
sealed trait cableAttribute
sealed trait leagueAttribute
sealed trait nautical_mileAttribute
sealed trait furlongAttribute
sealed trait minimAttribute
sealed trait fluid_ounceAttribute
sealed trait gillAttribute
sealed trait cupAttribute
sealed trait pintAttribute
sealed trait quartAttribute
sealed trait pottleAttribute
sealed trait gallonAttribute
sealed trait peckAttribute
sealed trait bushelAttribute
sealed trait barrelAttribute
sealed trait hogsheadAttribute
sealed trait fluid_dramAttribute

object MetricAttributes{
  final object MoKα1 extends xunitAttribute
  final object Adm extends nautical_mileAttribute
  final object US extends square_linkAttribute with square_footAttribute with square_chainAttribute with square_rodAttribute with square_mileAttribute with acreAttribute with sectionAttribute with linkAttribute with footAttribute with rodAttribute with chainAttribute with mileAttribute with cableAttribute with leagueAttribute with furlongAttribute with minimAttribute with fluid_ounceAttribute with gillAttribute with cupAttribute with pottleAttribute with gallonAttribute with bushelAttribute with hogsheadAttribute with fluid_dramAttribute
  final object US_fl extends pintAttribute with quartAttribute with gallonAttribute with barrelAttribute
  final object US_dry extends pintAttribute with quartAttribute with gallonAttribute with peckAttribute with barrelAttribute
  final object CuKα1 extends xunitAttribute
  final object metric extends cupAttribute
  final object imp extends cableAttribute with minimAttribute with fluid_ounceAttribute with gillAttribute with pintAttribute with quartAttribute with pottleAttribute with gallonAttribute with peckAttribute with bushelAttribute with barrelAttribute with hogsheadAttribute with fluid_dramAttribute
  final object US_lvl extends bushelAttribute
}

object LengthUnitObjects{

  import spire.implicits._


  final case object metre extends SimpleLengthUnit("metre", "m", 1)
  final case object yoctometre extends SimpleLengthUnit("yoctometre", "ym", r"1e-24")
  final case object zeptometre extends SimpleLengthUnit("zeptometre", "zm", r"1e-21")
  final case object attometre extends SimpleLengthUnit("attometre", "am", r"1e-18")
  final case object femtometre extends SimpleLengthUnit("femtometre", "fm", r"1e-15")
  final case object picometre extends SimpleLengthUnit("picometre", "pm", r"1e-12")
  final case object nanometre extends SimpleLengthUnit("nanometre", "nm", r"1e-9")
  final case object micrometre extends DefaultLengthUnit("micrometre", "μm", Seq("mcm"), r"1e-6")
  final case object millimetre extends SimpleLengthUnit("millimetre", "mm", r"1e-3")
  final case object centimetre extends SimpleLengthUnit("centimetre", "cm", r"1e-2")
  final case object decimetre extends SimpleLengthUnit("decimetre", "dm", r"1e-1")
  final case object decametre extends SimpleLengthUnit("decametre", "dam", r"1e1")
  final case object hectometre extends SimpleLengthUnit("hectometre", "hm", r"1e2")
  final case object kilometre extends DefaultLengthUnit("kilometre", "km", Seq("Km"), r"1e3")
  final case object megametre extends SimpleLengthUnit("megametre", "Mm", r"1e6")
  final case object gigametre extends SimpleLengthUnit("gigametre", "Gm", r"1e9")
  final case object terametre extends SimpleLengthUnit("terametre", "Tm", r"1e12")
  final case object petametre extends SimpleLengthUnit("petametre", "Pm", r"1e15")
  final case object exametre extends SimpleLengthUnit("exametre", "Em", r"1e18")
  final case object zettametre extends SimpleLengthUnit("zettametre", "Zm", r"1e21")
  final case object yottametre extends SimpleLengthUnit("yottametre", "Ym", r"1e24")
  final case object micron extends SimpleLengthUnit("micron", "µ", r"1e-6")
  final case object angstrom extends SimpleLengthUnit("angstrom", "Å", r"1e-10")
  final case object atomic_unit_of_length extends SimpleLengthUnit("atomic unit of length", "a_0", r"5.291772109217e-11") with NotExact
  final case object xunit extends SimpleLengthUnit("xunit", "xu", r"1.0021e-13") with NotExact
  final case object `xunit(CuKα1)` extends SimpleLengthUnit("xunit(CuKα1)", "xu(CuKα1)", r"1.0020769928e-13") with NotExact
  final case object `xunit(MoKα1)` extends SimpleLengthUnit("xunit(MoKα1)", "xu(MoKα1)", r"1.0020995553e-13") with NotExact
  final case object planck_length extends SimpleLengthUnit("planck length", "l_p", r"1.61624e-35") with NotExact
  final case object astronomical_unit extends DefaultLengthUnit("astronomical unit", "AU", Seq("ua"), r"149597870700")
  final case object light_year extends SimpleLengthUnit("light year", "ly", r"9.4607304725808e15")
  final case object parsec extends SimpleLengthUnit("parsec", "pc", r"3.08567782e16") with NotExact
  final case object mil extends DefaultLengthUnit("mil", "mil", Seq("thou"), r"1"/r"1000" * inch.interval)
  final case object twip extends SimpleLengthUnit("twip", "twp", r"1"/r"20" * point.interval)
  final case object point extends DefaultLengthUnit("point", "pt", Seq("p"), r"1"/r"72" * inch.interval)
  final case object pica extends DefaultLengthUnit("pica", "pica", Seq("P"), r"12" * point.interval)
  final case object line extends SimpleLengthUnit("line", "ln", r"1"/r"12" * inch.interval)
  final case object inch extends SimpleLengthUnit("inch", "in", r"2.54" * centimetre.interval)
  final case object link extends DefaultLengthUnit("link", "li", Seq("lnk"), r"0.66" * foot.interval)
  final case object `link(US)` extends DefaultLengthUnit("link(US)", "li(US)", Seq("lnk(US)"), r"0.66" * `foot(US)`.interval)
  final case object foot extends SimpleLengthUnit("foot", "ft", r"12" * inch.interval)
  final case object `foot(US)` extends SimpleLengthUnit("foot(US)", "ft(US)", r"1200"/r"3937")
  final case object yard extends SimpleLengthUnit("yard", "yd", r"3" * foot.interval)
  final case object ell extends SimpleLengthUnit("ell", "ell", r"45" * inch.interval)
  final case object fathom extends SimpleLengthUnit("fathom", "ftm", r"2" * yard.interval)
  final case object rod extends SimpleLengthUnit("rod", "rd", r"16.5" * foot.interval)
  final case object `rod(US)` extends SimpleLengthUnit("rod(US)", "rd(US)", r"16.5" * `foot(US)`.interval)
  final case object rope extends SimpleLengthUnit("rope", "rope", r"20" * foot.interval)
  final case object chain extends SimpleLengthUnit("chain", "ch", r"66" * foot.interval)
  final case object `chain(US)` extends SimpleLengthUnit("chain(US)", "ch(US)", r"66" * `foot(US)`.interval)
  final case object mile extends SimpleLengthUnit("mile", "mi", r"1760" * yard.interval)
  final case object `mile(US)` extends SimpleLengthUnit("mile(US)", "mi(US)", r"5280" * `foot(US)`.interval)
  final case object cable extends SimpleLengthUnit("cable", "cb", r"120" * fathom.interval)
  final case object `cable(US)` extends SimpleLengthUnit("cable(US)", "cb(US)", r"720" * foot.interval)
  final case object `cable(imp)` extends SimpleLengthUnit("cable(imp)", "cb(imp)", r"608" * foot.interval)
  final case object league extends SimpleLengthUnit("league", "lea", r"3" * mile.interval)
  final case object `league(US)` extends SimpleLengthUnit("league(US)", "lea(US)", r"3" * `mile(US)`.interval)
  final case object nautical_mile extends DefaultLengthUnit("nautical mile", "NM", Seq("nmi"), r"1852")
  final case object `nautical_mile(Adm)` extends DefaultLengthUnit("nautical mile(Adm)", "NM(Adm)", Seq("nmi(Adm)"), r"6080" * foot.interval)
  final case object nautical_league extends DefaultLengthUnit("nautical league", "NL", Seq("nl"), r"3" * nautical_mile.interval)
  final case object metric_foot extends SimpleLengthUnit("metric foot", "mf", Real("1/10").sqrt())
  final case object short_metric_foot extends SimpleLengthUnit("short metric foot", "smf", r"0.3")
  final case object long_metric_foot extends SimpleLengthUnit("long metric foot", "lmf", r"1"/r"3")
  final case object french extends SimpleLengthUnit("french", "Fr", r"1"/r"3" * millimetre.interval)
  final case object furlong extends SimpleLengthUnit("furlong", "fur", r"10" * chain.interval)
  final case object `furlong(US)` extends SimpleLengthUnit("furlong(US)", "fur(US)", r"10" * `chain(US)`.interval)
}

object LengthUnits{

  def m: LengthUnit = LengthUnitObjects.metre
  def ym: LengthUnit = LengthUnitObjects.yoctometre
  def zm: LengthUnit = LengthUnitObjects.zeptometre
  def am: LengthUnit = LengthUnitObjects.attometre
  def fm: LengthUnit = LengthUnitObjects.femtometre
  def pm: LengthUnit = LengthUnitObjects.picometre
  def nm: LengthUnit = LengthUnitObjects.nanometre
  def `μm`: LengthUnit = LengthUnitObjects.micrometre
  def mcm: LengthUnit = LengthUnitObjects.micrometre
  def mm: LengthUnit = LengthUnitObjects.millimetre
  def cm: LengthUnit = LengthUnitObjects.centimetre
  def dm: LengthUnit = LengthUnitObjects.decimetre
  def dam: LengthUnit = LengthUnitObjects.decametre
  def hm: LengthUnit = LengthUnitObjects.hectometre
  def km: LengthUnit = LengthUnitObjects.kilometre
  def Km: LengthUnit = LengthUnitObjects.kilometre
  def Mm: LengthUnit = LengthUnitObjects.megametre
  def Gm: LengthUnit = LengthUnitObjects.gigametre
  def Tm: LengthUnit = LengthUnitObjects.terametre
  def Pm: LengthUnit = LengthUnitObjects.petametre
  def Em: LengthUnit = LengthUnitObjects.exametre
  def Zm: LengthUnit = LengthUnitObjects.zettametre
  def Ym: LengthUnit = LengthUnitObjects.yottametre
  def `µ`: LengthUnit = LengthUnitObjects.micron
  def `Å`: LengthUnit = LengthUnitObjects.angstrom
  def a_0: LengthUnit = LengthUnitObjects.atomic_unit_of_length
  def xu: LengthUnit = LengthUnitObjects.xunit
  def xu(a: xunitAttribute): LengthUnit = a match { 
    case MetricAttributes.CuKα1 => LengthUnitObjects.`xunit(CuKα1)`
    case MetricAttributes.MoKα1 => LengthUnitObjects.`xunit(MoKα1)`
  }
  def l_p: LengthUnit = LengthUnitObjects.planck_length
  def AU: LengthUnit = LengthUnitObjects.astronomical_unit
  def ua: LengthUnit = LengthUnitObjects.astronomical_unit
  def ly: LengthUnit = LengthUnitObjects.light_year
  def pc: LengthUnit = LengthUnitObjects.parsec
  def mil: LengthUnit = LengthUnitObjects.mil
  def thou: LengthUnit = LengthUnitObjects.mil
  def twp: LengthUnit = LengthUnitObjects.twip
  def pt: LengthUnit = LengthUnitObjects.point
  def p: LengthUnit = LengthUnitObjects.point
  def pica: LengthUnit = LengthUnitObjects.pica
  def P: LengthUnit = LengthUnitObjects.pica
  def ln: LengthUnit = LengthUnitObjects.line
  def in: LengthUnit = LengthUnitObjects.inch
  def li: LengthUnit = LengthUnitObjects.link
  def li(a: linkAttribute): LengthUnit = a match { 
    case MetricAttributes.US => LengthUnitObjects.`link(US)`
  }
  def lnk: LengthUnit = LengthUnitObjects.link
  def lnk(a: linkAttribute): LengthUnit = li(a)

  def ft: LengthUnit = LengthUnitObjects.foot
  def ft(a: footAttribute): LengthUnit = a match { 
    case MetricAttributes.US => LengthUnitObjects.`foot(US)`
  }
  def yd: LengthUnit = LengthUnitObjects.yard
  def ell: LengthUnit = LengthUnitObjects.ell
  def ftm: LengthUnit = LengthUnitObjects.fathom
  def rd: LengthUnit = LengthUnitObjects.rod
  def rd(a: rodAttribute): LengthUnit = a match { 
    case MetricAttributes.US => LengthUnitObjects.`rod(US)`
  }
  def rope: LengthUnit = LengthUnitObjects.rope
  def ch: LengthUnit = LengthUnitObjects.chain
  def ch(a: chainAttribute): LengthUnit = a match { 
    case MetricAttributes.US => LengthUnitObjects.`chain(US)`
  }
  def mi: LengthUnit = LengthUnitObjects.mile
  def mi(a: mileAttribute): LengthUnit = a match { 
    case MetricAttributes.US => LengthUnitObjects.`mile(US)`
  }
  def cb: LengthUnit = LengthUnitObjects.cable
  def cb(a: cableAttribute): LengthUnit = a match { 
    case MetricAttributes.US => LengthUnitObjects.`cable(US)`
    case MetricAttributes.imp => LengthUnitObjects.`cable(imp)`
  }
  def lea: LengthUnit = LengthUnitObjects.league
  def lea(a: leagueAttribute): LengthUnit = a match { 
    case MetricAttributes.US => LengthUnitObjects.`league(US)`
  }
  def NM: LengthUnit = LengthUnitObjects.nautical_mile
  def NM(a: nautical_mileAttribute): LengthUnit = a match { 
    case MetricAttributes.Adm => LengthUnitObjects.`nautical_mile(Adm)`
  }
  def nmi: LengthUnit = LengthUnitObjects.nautical_mile
  def nmi(a: nautical_mileAttribute): LengthUnit = NM(a)

  def NL: LengthUnit = LengthUnitObjects.nautical_league
  def nl: LengthUnit = LengthUnitObjects.nautical_league
  def mf: LengthUnit = LengthUnitObjects.metric_foot
  def smf: LengthUnit = LengthUnitObjects.short_metric_foot
  def lmf: LengthUnit = LengthUnitObjects.long_metric_foot
  def Fr: LengthUnit = LengthUnitObjects.french
  def fur: LengthUnit = LengthUnitObjects.furlong
  def fur(a: furlongAttribute): LengthUnit = a match { 
    case MetricAttributes.US => LengthUnitObjects.`furlong(US)`
  }
}