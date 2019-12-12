package org.waman.multiverse.unit.basic

import spire.math.Real
import spire.math.Fractional
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.unit.mechanics.TimeSquared
import org.waman.multiverse.unit.mechanics.TimeSquaredUnit

import org.waman.multiverse.unit.mechanics.Acceleration
import org.waman.multiverse.unit.mechanics.AccelerationUnit

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends LinearQuantity[Length[A], A, LengthUnit] {

  override protected def newQuantity(value: A, unit: LengthUnit): Length[A] = new Length(value, unit)
  def *(length: Length[A]): Area[A] = new Area(this.value * length.value, this.unit * length.unit)

  def squared: Area[A] = this * this
  def cubic: Volume[A] = this * this * this

  def /(time: Time[A]): Velocity[A] = new Velocity(this.value / time.value, this.unit / time.unit)

  def /(timeSquared: TimeSquared[A]): Acceleration[A] = new Acceleration(this.value / timeSquared.value, this.unit / timeSquared.unit)

}

trait LengthUnit extends LinearUnit[LengthUnit]{

  override def getSIUnit: LengthUnit = LengthUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = LengthUnit.dimension

  def squared: AreaUnit =
    new AreaUnit{
      override val name: String = "square " + LengthUnit.this.name
      override val symbol: String = LengthUnit.this.symbol + "²"
      override val interval: Real = LengthUnit.this.interval**2
      override def aliases: Seq[String] = {
        val heads = if (LengthUnit.this.name == "metre") Seq("m2") else Nil

        val symbols = LengthUnit.this.symbol +: LengthUnit.this.aliases
        val squares = symbols.map(_+".squared")
        val prods = symbols.map(a => a+"*"+a)

        heads ++: squares ++: prods
      }

      override def *(lengthUnit: LengthUnit): VolumeUnit = {
        if (lengthUnit == LengthUnit.this){
          LengthUnit.this.cubic
        } else {
          super.*(lengthUnit)
        }
      }
    }

  def cubic: VolumeUnit =
    new VolumeUnit{
      override val name: String = "cubic " + LengthUnit.this.name
      override val symbol: String = LengthUnit.this.symbol + "³"
      override val interval: Real = LengthUnit.this.interval**3
      override def aliases: Seq[String] = {
        val heads = if (LengthUnit.this.name == "metre") Seq("m3") else Nil

        val symbols = LengthUnit.this.symbol +: LengthUnit.this.aliases
        val cubics = symbols.map(_+".cubic")
        val prods = symbols.map(a => a+"*"+a+"*"+a)

        heads ++: cubics ++: prods
      }
    }

  def *(lengthUnit: LengthUnit): AreaUnit =
    if(this == lengthUnit)
      this.squared
    else
      new ProductUnit[AreaUnit, LengthUnit, LengthUnit](LengthUnit.this, lengthUnit) with AreaUnit

  def /(timeUnit: TimeUnit): VelocityUnit =
    new QuotientUnit[VelocityUnit, LengthUnit, TimeUnit](LengthUnit.this, timeUnit) with VelocityUnit

  def /(timeSquaredUnit: TimeSquaredUnit): AccelerationUnit =
    new QuotientUnit[AccelerationUnit, LengthUnit, TimeSquaredUnit](LengthUnit.this, timeSquaredUnit) with AccelerationUnit

}

object LengthUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](L -> 1).withDefaultValue(0)

  def getSIUnit: LengthUnit = LengthUnitObjects.metre

  import LengthUnitObjects._
  def getUnits: Seq[LengthUnit] =
    Seq(metre, yoctometre, zeptometre, attometre, femtometre, picometre, nanometre, micrometre, millimetre, centimetre, decimetre, decametre, hectometre, kilometre, megametre, gigametre, terametre, petametre, exametre, zettametre, yottametre, micron, Angstrom, atomic_unit_of_length, xunit, `xunit(CuKα1)`, `xunit(MoKα1)`, planck_length, astronomical_unit, light_year, parsec, mil, twip, point, line, inch, foot, yard, ell, fathom, rod, rope, chain, mile, league, nautical_mile, `nautical_mile(Adm)`, nautical_league, metric_foot, short_metric_foot, long_metric_foot, french, furlong)
}


sealed trait xunitAttribute
sealed trait nautical_mileAttribute

object LengthAttributes{
  final object MoKα1 extends xunitAttribute
  final object CuKα1 extends xunitAttribute
  final object Adm extends nautical_mileAttribute
}

class DefaultLengthUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LengthUnit

object LengthUnitObjects{

  final object metre extends DefaultLengthUnit("metre", "m", Nil, 1)
  final object yoctometre extends DefaultLengthUnit("yoctometre", "ym", Nil, 1 * r"1e-24")
  final object zeptometre extends DefaultLengthUnit("zeptometre", "zm", Nil, 1 * r"1e-21")
  final object attometre extends DefaultLengthUnit("attometre", "am", Nil, 1 * r"1e-18")
  final object femtometre extends DefaultLengthUnit("femtometre", "fm", Nil, 1 * r"1e-15")
  final object picometre extends DefaultLengthUnit("picometre", "pm", Nil, 1 * r"1e-12")
  final object nanometre extends DefaultLengthUnit("nanometre", "nm", Nil, 1 * r"1e-9")
  final object micrometre extends DefaultLengthUnit("micrometre", "μm", Seq("mcm"), 1 * r"1e-6")
  final object millimetre extends DefaultLengthUnit("millimetre", "mm", Nil, 1 * r"1e-3")
  final object centimetre extends DefaultLengthUnit("centimetre", "cm", Nil, 1 * r"1e-2")
  final object decimetre extends DefaultLengthUnit("decimetre", "dm", Nil, 1 * r"1e-1")
  final object decametre extends DefaultLengthUnit("decametre", "dam", Nil, 1 * r"1e1")
  final object hectometre extends DefaultLengthUnit("hectometre", "hm", Nil, 1 * r"1e2")
  final object kilometre extends DefaultLengthUnit("kilometre", "km", Seq("Km"), 1 * r"1e3")
  final object megametre extends DefaultLengthUnit("megametre", "Mm", Nil, 1 * r"1e6")
  final object gigametre extends DefaultLengthUnit("gigametre", "Gm", Nil, 1 * r"1e9")
  final object terametre extends DefaultLengthUnit("terametre", "Tm", Nil, 1 * r"1e12")
  final object petametre extends DefaultLengthUnit("petametre", "Pm", Nil, 1 * r"1e15")
  final object exametre extends DefaultLengthUnit("exametre", "Em", Nil, 1 * r"1e18")
  final object zettametre extends DefaultLengthUnit("zettametre", "Zm", Nil, 1 * r"1e21")
  final object yottametre extends DefaultLengthUnit("yottametre", "Ym", Nil, 1 * r"1e24")
  final object micron extends DefaultLengthUnit("micron", "µ", Nil, r"1e-6")
  final object Angstrom extends DefaultLengthUnit("Angstrom", "Å", Nil, r"1e-10")
  final object atomic_unit_of_length extends DefaultLengthUnit("atomic unit of length", "a_0", Nil, r"5.291772109217e-11") with NotExact
  final object xunit extends DefaultLengthUnit("xunit", "xu", Nil, r"1.0021e-13") with NotExact
  final object `xunit(CuKα1)` extends DefaultLengthUnit("xunit(CuKα1)", "xu(CuKα1)", Nil, r"1.0020769928e-13") with NotExact
  final object `xunit(MoKα1)` extends DefaultLengthUnit("xunit(MoKα1)", "xu(MoKα1)", Nil, r"1.0020995553e-13") with NotExact
  final object planck_length extends DefaultLengthUnit("planck length", "l_p", Nil, r"1.61624e-35") with NotExact
  final object astronomical_unit extends DefaultLengthUnit("astronomical unit", "au", Nil, r"149597870700")
  final object light_year extends DefaultLengthUnit("light year", "ly", Nil, r"9.4607304725808e15")
  final object parsec extends DefaultLengthUnit("parsec", "pc", Nil, r"3.08567782e16") with NotExact
  final object mil extends DefaultLengthUnit("mil", "mil", Seq("thou"), r"1"/r"1000" * inch.interval)
  final object twip extends DefaultLengthUnit("twip", "twp", Nil, r"1"/r"20" * point.interval)
  final object point extends DefaultLengthUnit("point", "pt", Nil, r"1"/r"72" * inch.interval)
  final object line extends DefaultLengthUnit("line", "ln", Nil, r"1"/r"12" * inch.interval)
  final object inch extends DefaultLengthUnit("inch", "in", Nil, r"2.54" * centimetre.interval)
  final object foot extends DefaultLengthUnit("foot", "ft", Nil, r"12" * inch.interval)
  final object yard extends DefaultLengthUnit("yard", "yd", Nil, r"3" * foot.interval)
  final object ell extends DefaultLengthUnit("ell", "ell", Nil, r"45" * inch.interval)
  final object fathom extends DefaultLengthUnit("fathom", "ftm", Nil, r"6" * foot.interval)
  final object rod extends DefaultLengthUnit("rod", "rd", Nil, r"16.5" * foot.interval)
  final object rope extends DefaultLengthUnit("rope", "rope", Nil, r"20" * foot.interval)
  final object chain extends DefaultLengthUnit("chain", "ch", Nil, r"66" * foot.interval)
  final object mile extends DefaultLengthUnit("mile", "mi", Nil, r"1760" * yard.interval)
  final object league extends DefaultLengthUnit("league", "lea", Nil, r"3" * mile.interval)
  final object nautical_mile extends DefaultLengthUnit("nautical mile", "NM", Seq("nmi"), r"1852")
  final object `nautical_mile(Adm)` extends DefaultLengthUnit("nautical mile(Adm)", "NM(Adm)", Seq("nmi(Adm)"), r"6080" * foot.interval)
  final object nautical_league extends DefaultLengthUnit("nautical league", "NL", Seq("nl"), r"3" * nautical_mile.interval)
  final object metric_foot extends DefaultLengthUnit("metric foot", "mf", Nil, Real("1/10").sqrt())
  final object short_metric_foot extends DefaultLengthUnit("short metric foot", "smf", Nil, r"0.3")
  final object long_metric_foot extends DefaultLengthUnit("long metric foot", "lmf", Nil, r"1"/r"3")
  final object french extends DefaultLengthUnit("french", "Fr", Nil, r"1"/r"3" * millimetre.interval)
  final object furlong extends DefaultLengthUnit("furlong", "fur", Nil, r"660" * foot.interval)
}

object LengthUnits{
  def m: LengthUnit = LengthUnitObjects.metre
  def ym: LengthUnit = LengthUnitObjects.yoctometre
  def zm: LengthUnit = LengthUnitObjects.zeptometre
  def am: LengthUnit = LengthUnitObjects.attometre
  def fm: LengthUnit = LengthUnitObjects.femtometre
  def pm: LengthUnit = LengthUnitObjects.picometre
  def nm: LengthUnit = LengthUnitObjects.nanometre
  def μm: LengthUnit = LengthUnitObjects.micrometre
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
  def µ: LengthUnit = LengthUnitObjects.micron
  def Å: LengthUnit = LengthUnitObjects.Angstrom
  def a_0: LengthUnit = LengthUnitObjects.atomic_unit_of_length
  def xu: LengthUnit = LengthUnitObjects.xunit
  def xu(a: xunitAttribute): LengthUnit = a match { 
    case LengthAttributes.CuKα1 => LengthUnitObjects.`xunit(CuKα1)`
    case LengthAttributes.MoKα1 => LengthUnitObjects.`xunit(MoKα1)`
  }
  def l_p: LengthUnit = LengthUnitObjects.planck_length
  def au: LengthUnit = LengthUnitObjects.astronomical_unit
  def ly: LengthUnit = LengthUnitObjects.light_year
  def pc: LengthUnit = LengthUnitObjects.parsec
  def mil: LengthUnit = LengthUnitObjects.mil
  def thou: LengthUnit = LengthUnitObjects.mil
  def twp: LengthUnit = LengthUnitObjects.twip
  def pt: LengthUnit = LengthUnitObjects.point
  def ln: LengthUnit = LengthUnitObjects.line
  def in: LengthUnit = LengthUnitObjects.inch
  def ft: LengthUnit = LengthUnitObjects.foot
  def yd: LengthUnit = LengthUnitObjects.yard
  def ell: LengthUnit = LengthUnitObjects.ell
  def ftm: LengthUnit = LengthUnitObjects.fathom
  def rd: LengthUnit = LengthUnitObjects.rod
  def rope: LengthUnit = LengthUnitObjects.rope
  def ch: LengthUnit = LengthUnitObjects.chain
  def mi: LengthUnit = LengthUnitObjects.mile
  def lea: LengthUnit = LengthUnitObjects.league
  def NM: LengthUnit = LengthUnitObjects.nautical_mile
  def NM(a: nautical_mileAttribute): LengthUnit = a match { 
    case LengthAttributes.Adm => LengthUnitObjects.`nautical_mile(Adm)`
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
}