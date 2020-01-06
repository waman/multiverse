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
      override def aliases: Seq[String] = LengthUnit.this.symbols.map(_+".squared")

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
      override def aliases: Seq[String] = LengthUnit.this.symbols.map(_+".cubic")
    }

  def *(lengthUnit: LengthUnit): AreaUnit =
    if(this == lengthUnit)
      this.squared
    else
      new AbstractProductUnit[AreaUnit, LengthUnit, LengthUnit](LengthUnit.this, lengthUnit) with AreaUnit

  def /(timeUnit: TimeUnit): VelocityUnit =
    new AbstractQuotientUnit[VelocityUnit, LengthUnit, TimeUnit](LengthUnit.this, timeUnit) with VelocityUnit

  def /(timeSquaredUnit: TimeSquaredUnit): AccelerationUnit =
    new AbstractQuotientUnit[AccelerationUnit, LengthUnit, TimeSquaredUnit](LengthUnit.this, timeSquaredUnit) with AccelerationUnit

}

/** For user defined units */
class SimpleLengthUnit(val name: String, val symbol: String, val interval: Real) extends LengthUnit {
  override def aliases: Seq[String] = Nil
}

class DefaultLengthUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends LengthUnit

object LengthUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](L -> 1).withDefaultValue(0)

  def getSIUnit: LengthUnit = LengthUnitObjects.metre

  import LengthUnitObjects._
  def getUnits: Seq[LengthUnit] =
    Seq(metre, yoctometre, zeptometre, attometre, femtometre, picometre, nanometre, micrometre, millimetre, centimetre, decimetre, decametre, hectometre, kilometre, megametre, gigametre, terametre, petametre, exametre, zettametre, yottametre, micron, Angstrom, atomic_unit_of_length, xunit, `xunit(CuKα1)`, `xunit(MoKα1)`, planck_length, astronomical_unit, light_year, parsec, mil, twip, point, line, inch, link, `link(US)`, foot, `foot(US)`, yard, ell, fathom, rod, rope, chain, `chain(US)`, mile, `mile(US)`, cable, `cable(US)`, `cable(imp)`, league, nautical_mile, `nautical_mile(Adm)`, nautical_league, metric_foot, short_metric_foot, long_metric_foot, french, furlong)
}

sealed trait xunitAttribute
sealed trait linkAttribute
sealed trait footAttribute
sealed trait chainAttribute
sealed trait mileAttribute
sealed trait cableAttribute
sealed trait nautical_mileAttribute

object LengthAttributes{
  final object MoKα1 extends xunitAttribute
  final object Adm extends nautical_mileAttribute
  final object US extends linkAttribute with footAttribute with chainAttribute with mileAttribute with cableAttribute
  final object CuKα1 extends xunitAttribute
  final object imp extends cableAttribute
}

object LengthUnitObjects{

  final case object metre extends DefaultLengthUnit("metre", "m", Nil, 1)
  final case object yoctometre extends DefaultLengthUnit("yoctometre", "ym", Nil, r"1e-24")
  final case object zeptometre extends DefaultLengthUnit("zeptometre", "zm", Nil, r"1e-21")
  final case object attometre extends DefaultLengthUnit("attometre", "am", Nil, r"1e-18")
  final case object femtometre extends DefaultLengthUnit("femtometre", "fm", Nil, r"1e-15")
  final case object picometre extends DefaultLengthUnit("picometre", "pm", Nil, r"1e-12")
  final case object nanometre extends DefaultLengthUnit("nanometre", "nm", Nil, r"1e-9")
  final case object micrometre extends DefaultLengthUnit("micrometre", "μm", Seq("mcm"), r"1e-6")
  final case object millimetre extends DefaultLengthUnit("millimetre", "mm", Nil, r"1e-3")
  final case object centimetre extends DefaultLengthUnit("centimetre", "cm", Nil, r"1e-2")
  final case object decimetre extends DefaultLengthUnit("decimetre", "dm", Nil, r"1e-1")
  final case object decametre extends DefaultLengthUnit("decametre", "dam", Nil, r"1e1")
  final case object hectometre extends DefaultLengthUnit("hectometre", "hm", Nil, r"1e2")
  final case object kilometre extends DefaultLengthUnit("kilometre", "km", Seq("Km"), r"1e3")
  final case object megametre extends DefaultLengthUnit("megametre", "Mm", Nil, r"1e6")
  final case object gigametre extends DefaultLengthUnit("gigametre", "Gm", Nil, r"1e9")
  final case object terametre extends DefaultLengthUnit("terametre", "Tm", Nil, r"1e12")
  final case object petametre extends DefaultLengthUnit("petametre", "Pm", Nil, r"1e15")
  final case object exametre extends DefaultLengthUnit("exametre", "Em", Nil, r"1e18")
  final case object zettametre extends DefaultLengthUnit("zettametre", "Zm", Nil, r"1e21")
  final case object yottametre extends DefaultLengthUnit("yottametre", "Ym", Nil, r"1e24")
  final case object micron extends DefaultLengthUnit("micron", "µ", Nil, r"1e-6")
  final case object Angstrom extends DefaultLengthUnit("Angstrom", "Å", Nil, r"1e-10")
  final case object atomic_unit_of_length extends DefaultLengthUnit("atomic unit of length", "a_0", Nil, r"5.291772109217e-11") with NotExact
  final case object xunit extends DefaultLengthUnit("xunit", "xu", Nil, r"1.0021e-13") with NotExact
  final case object `xunit(CuKα1)` extends DefaultLengthUnit("xunit(CuKα1)", "xu(CuKα1)", Nil, r"1.0020769928e-13") with NotExact
  final case object `xunit(MoKα1)` extends DefaultLengthUnit("xunit(MoKα1)", "xu(MoKα1)", Nil, r"1.0020995553e-13") with NotExact
  final case object planck_length extends DefaultLengthUnit("planck length", "l_p", Nil, r"1.61624e-35") with NotExact
  final case object astronomical_unit extends DefaultLengthUnit("astronomical unit", "au", Nil, r"149597870700")
  final case object light_year extends DefaultLengthUnit("light year", "ly", Nil, r"9.4607304725808e15")
  final case object parsec extends DefaultLengthUnit("parsec", "pc", Nil, r"3.08567782e16") with NotExact
  final case object mil extends DefaultLengthUnit("mil", "mil", Seq("thou"), r"1"/r"1000" * inch.interval)
  final case object twip extends DefaultLengthUnit("twip", "twp", Nil, r"1"/r"20" * point.interval)
  final case object point extends DefaultLengthUnit("point", "pt", Nil, r"1"/r"72" * inch.interval)
  final case object line extends DefaultLengthUnit("line", "ln", Nil, r"1"/r"12" * inch.interval)
  final case object inch extends DefaultLengthUnit("inch", "in", Nil, r"2.54" * centimetre.interval)
  final case object link extends DefaultLengthUnit("link", "li", Seq("lnk"), r"0.66" * foot.interval)
  final case object `link(US)` extends DefaultLengthUnit("link(US)", "li(US)", Seq("lnk(US)"), r"0.66" * `foot(US)`.interval)
  final case object foot extends DefaultLengthUnit("foot", "ft", Nil, r"12" * inch.interval)
  final case object `foot(US)` extends DefaultLengthUnit("foot(US)", "ft(US)", Nil, r"1200"/r"3937")
  final case object yard extends DefaultLengthUnit("yard", "yd", Nil, r"3" * foot.interval)
  final case object ell extends DefaultLengthUnit("ell", "ell", Nil, r"45" * inch.interval)
  final case object fathom extends DefaultLengthUnit("fathom", "ftm", Nil, r"6" * foot.interval)
  final case object rod extends DefaultLengthUnit("rod", "rd", Nil, r"16.5" * foot.interval)
  final case object rope extends DefaultLengthUnit("rope", "rope", Nil, r"20" * foot.interval)
  final case object chain extends DefaultLengthUnit("chain", "ch", Nil, r"66" * foot.interval)
  final case object `chain(US)` extends DefaultLengthUnit("chain(US)", "ch(US)", Nil, r"66" * `foot(US)`.interval)
  final case object mile extends DefaultLengthUnit("mile", "mi", Nil, r"1760" * yard.interval)
  final case object `mile(US)` extends DefaultLengthUnit("mile(US)", "mi(US)", Nil, r"5280" * `foot(US)`.interval)
  final case object cable extends DefaultLengthUnit("cable", "cb", Nil, r"1"/r"10" * nautical_mile.interval)
  final case object `cable(US)` extends DefaultLengthUnit("cable(US)", "cb(US)", Nil, r"720" * foot.interval)
  final case object `cable(imp)` extends DefaultLengthUnit("cable(imp)", "cb(imp)", Nil, r"608" * foot.interval)
  final case object league extends DefaultLengthUnit("league", "lea", Nil, r"3" * mile.interval)
  final case object nautical_mile extends DefaultLengthUnit("nautical mile", "NM", Seq("nmi"), r"1852")
  final case object `nautical_mile(Adm)` extends DefaultLengthUnit("nautical mile(Adm)", "NM(Adm)", Seq("nmi(Adm)"), r"6080" * foot.interval)
  final case object nautical_league extends DefaultLengthUnit("nautical league", "NL", Seq("nl"), r"3" * nautical_mile.interval)
  final case object metric_foot extends DefaultLengthUnit("metric foot", "mf", Nil, Real("1/10").sqrt())
  final case object short_metric_foot extends DefaultLengthUnit("short metric foot", "smf", Nil, r"0.3")
  final case object long_metric_foot extends DefaultLengthUnit("long metric foot", "lmf", Nil, r"1"/r"3")
  final case object french extends DefaultLengthUnit("french", "Fr", Nil, r"1"/r"3" * millimetre.interval)
  final case object furlong extends DefaultLengthUnit("furlong", "fur", Nil, r"660" * foot.interval)
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
  def `Å`: LengthUnit = LengthUnitObjects.Angstrom
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
  def li: LengthUnit = LengthUnitObjects.link
  def li(a: linkAttribute): LengthUnit = a match { 
    case LengthAttributes.US => LengthUnitObjects.`link(US)`
  }
  def lnk: LengthUnit = LengthUnitObjects.link
  def lnk(a: linkAttribute): LengthUnit = li(a)

  def ft: LengthUnit = LengthUnitObjects.foot
  def ft(a: footAttribute): LengthUnit = a match { 
    case LengthAttributes.US => LengthUnitObjects.`foot(US)`
  }
  def yd: LengthUnit = LengthUnitObjects.yard
  def ell: LengthUnit = LengthUnitObjects.ell
  def ftm: LengthUnit = LengthUnitObjects.fathom
  def rd: LengthUnit = LengthUnitObjects.rod
  def rope: LengthUnit = LengthUnitObjects.rope
  def ch: LengthUnit = LengthUnitObjects.chain
  def ch(a: chainAttribute): LengthUnit = a match { 
    case LengthAttributes.US => LengthUnitObjects.`chain(US)`
  }
  def mi: LengthUnit = LengthUnitObjects.mile
  def mi(a: mileAttribute): LengthUnit = a match { 
    case LengthAttributes.US => LengthUnitObjects.`mile(US)`
  }
  def cb: LengthUnit = LengthUnitObjects.cable
  def cb(a: cableAttribute): LengthUnit = a match { 
    case LengthAttributes.US => LengthUnitObjects.`cable(US)`
    case LengthAttributes.imp => LengthUnitObjects.`cable(imp)`
  }
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