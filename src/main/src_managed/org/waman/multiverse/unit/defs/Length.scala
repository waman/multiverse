package org.waman.multiverse.unit.defs

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs.em._
import org.waman.multiverse.unit.defs.mechanics._

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends LinearQuantity[Length[A], A, LengthUnit] {


  def toCapacitance: Capacitance[A] = new Capacitance(
      apply(LengthUnitObjects.centimetre),
      CapacitanceUnitObjects.statfarad)

  override protected def newQuantity(value: A, unit: LengthUnit): Length[A] = new Length(value, unit)

  def *(length: Length[A]): Area[A] = new Area(this.value * length.value, this.unit * length.unit)
  def squared: Area[A] = this * this
  def cubic: Volume[A] = this * this * this


  def *(electricCharge: ElectricCharge[A]): ElectricDipole[A] = new ElectricDipole(this.value * electricCharge.value, electricCharge.unit * this.unit)

  def /(timeSquared: TimeSquared[A]): Acceleration[A] = new Acceleration(this.value / timeSquared.value, this.unit / timeSquared.unit)

  def *(force: Force[A]): Energy[A] = new Energy(this.value * force.value, this.unit * force.unit)

  def *(mass: Mass[A]): MassTorque[A] = new MassTorque(this.value * mass.value, mass.unit * this.unit)

  def /(time: Time[A]): Velocity[A] = new Velocity(this.value / time.value, this.unit / time.unit)

  def *(area: Area[A]): Volume[A] = new Volume(this.value * area.value, area.unit * this.unit)
}

/** Some('US' attribute contains 'US Survey' metric.) */
trait LengthUnit extends LinearUnit[LengthUnit] with LengthUnitCanSquare with LengthUnitCanCubic{

  override def getSIUnit: LengthUnit = LengthUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = LengthUnit.dimension

  def /(timeSquaredUnit: TimeSquaredUnit): AccelerationUnit =
    new QuotientUnit[AccelerationUnit, LengthUnit, TimeSquaredUnit](LengthUnit.this, timeSquaredUnit) with AccelerationUnit

  def *(forceUnit: ForceUnit): EnergyUnit =
    new ProductUnit[EnergyUnit, LengthUnit, ForceUnit](LengthUnit.this, forceUnit) with EnergyUnit

  def /(timeUnit: TimeUnit): VelocityUnit =
    new QuotientUnit[VelocityUnit, LengthUnit, TimeUnit](LengthUnit.this, timeUnit) with VelocityUnit
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
  
object LengthUnitObjects{

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
  final case object atomic_unit_of_length extends SimpleLengthUnit("atomic unit of length", "a0", r"5.291772109217e-11") with NotExact
  final case object xunit extends SimpleLengthUnit("xunit", "xu", r"1.0021e-13") with NotExact
  final case object `xunit(CuKα1)` extends SimpleLengthUnit("xunit(CuKα1)", "xu(CuKα1)", r"1.0020769928e-13") with NotExact
  final case object `xunit(MoKα1)` extends SimpleLengthUnit("xunit(MoKα1)", "xu(MoKα1)", r"1.0020995553e-13") with NotExact
  final case object planck_length extends SimpleLengthUnit("planck length", "l_p", r"1.61624e-35") with NotExact
  final case object astronomical_unit extends DefaultLengthUnit("astronomical unit", "AU", Seq("ua"), r"149597870700")
  final case object light_year extends SimpleLengthUnit("light year", "ly", r"9.4607304725808e15")
  final case object parsec extends SimpleLengthUnit("parsec", "pc", r"3.08567782e16") with NotExact
  final case object mil extends DefaultLengthUnit("mil", "mil", Seq("thou"), r"1/1000" * inch.interval)
  final case object twip extends SimpleLengthUnit("twip", "twp", r"1/20" * point.interval)
  final case object point extends DefaultLengthUnit("point", "pt", Seq("p"), r"1/72" * inch.interval)
  final case object pica extends DefaultLengthUnit("pica", "pica", Seq("P"), r"12" * point.interval)
  final case object line extends SimpleLengthUnit("line", "ln", r"1/12" * inch.interval)
  final case object inch extends SimpleLengthUnit("inch", "in", r"2.54" * centimetre.interval)
  final case object link extends DefaultLengthUnit("link", "li", Seq("lnk"), r"0.66" * foot.interval)
  final case object `link(US)` extends DefaultLengthUnit("link(US)", "li(US)", Seq("lnk(US)"), r"0.66" * `foot(US)`.interval)
  final case object foot extends SimpleLengthUnit("foot", "ft", r"12" * inch.interval)
  final case object `foot(US)` extends SimpleLengthUnit("foot(US)", "ft(US)", r"1200/3937")
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
  final case object long_metric_foot extends SimpleLengthUnit("long metric foot", "lmf", r"1/3")
  final case object french extends SimpleLengthUnit("french", "Fr", r"1/3" * millimetre.interval)
  final case object furlong extends SimpleLengthUnit("furlong", "fur", r"10" * chain.interval)
  final case object `furlong(US)` extends SimpleLengthUnit("furlong(US)", "fur(US)", r"10" * `chain(US)`.interval)
}


object LengthUnits{

  /** metre */
  def m: LengthUnit = LengthUnitObjects.metre
  /** yoctometre */
  def ym: LengthUnit = LengthUnitObjects.yoctometre
  /** zeptometre */
  def zm: LengthUnit = LengthUnitObjects.zeptometre
  /** attometre */
  def am: LengthUnit = LengthUnitObjects.attometre
  /** femtometre */
  def fm: LengthUnit = LengthUnitObjects.femtometre
  /** picometre */
  def pm: LengthUnit = LengthUnitObjects.picometre
  /** nanometre */
  def nm: LengthUnit = LengthUnitObjects.nanometre
  /** micrometre */
  def μm: LengthUnit = LengthUnitObjects.micrometre
  /** micrometre */
  def mcm: LengthUnit = LengthUnitObjects.micrometre
  /** millimetre */
  def mm: LengthUnit = LengthUnitObjects.millimetre
  /** centimetre */
  def cm: LengthUnit = LengthUnitObjects.centimetre
  /** decimetre */
  def dm: LengthUnit = LengthUnitObjects.decimetre
  /** decametre */
  def dam: LengthUnit = LengthUnitObjects.decametre
  /** hectometre */
  def hm: LengthUnit = LengthUnitObjects.hectometre
  /** kilometre */
  def km: LengthUnit = LengthUnitObjects.kilometre
  /** kilometre */
  def Km: LengthUnit = LengthUnitObjects.kilometre
  /** megametre */
  def Mm: LengthUnit = LengthUnitObjects.megametre
  /** gigametre */
  def Gm: LengthUnit = LengthUnitObjects.gigametre
  /** terametre */
  def Tm: LengthUnit = LengthUnitObjects.terametre
  /** petametre */
  def Pm: LengthUnit = LengthUnitObjects.petametre
  /** exametre */
  def Em: LengthUnit = LengthUnitObjects.exametre
  /** zettametre */
  def Zm: LengthUnit = LengthUnitObjects.zettametre
  /** yottametre */
  def Ym: LengthUnit = LengthUnitObjects.yottametre
  /** micron */
  def µ: LengthUnit = LengthUnitObjects.micron
  /** angstrom */
  def Å: LengthUnit = LengthUnitObjects.angstrom
  /** atomic unit of length */
  def a0: LengthUnit = LengthUnitObjects.atomic_unit_of_length
  /** xunit */
  def xu: LengthUnit = LengthUnitObjects.xunit
  /** xunit(CuKα1)<br/>xunit(MoKα1) */
  def xu(a: xunitAttribute): LengthUnit = a match {
    case MetricAttributes.CuKα1 => LengthUnitObjects.`xunit(CuKα1)`
    case MetricAttributes.MoKα1 => LengthUnitObjects.`xunit(MoKα1)`
  }
  /** xunit(CuKα1) */
  def `xu(CuKα1)`: LengthUnit = LengthUnitObjects.`xunit(CuKα1)`
  /** xunit(MoKα1) */
  def `xu(MoKα1)`: LengthUnit = LengthUnitObjects.`xunit(MoKα1)`
  /** planck length */
  def l_p: LengthUnit = LengthUnitObjects.planck_length
  /** astronomical unit */
  def AU: LengthUnit = LengthUnitObjects.astronomical_unit
  /** astronomical unit */
  def ua: LengthUnit = LengthUnitObjects.astronomical_unit
  /** light year */
  def ly: LengthUnit = LengthUnitObjects.light_year
  /** parsec */
  def pc: LengthUnit = LengthUnitObjects.parsec
  /** mil */
  def mil: LengthUnit = LengthUnitObjects.mil
  /** mil */
  def thou: LengthUnit = LengthUnitObjects.mil
  /** twip */
  def twp: LengthUnit = LengthUnitObjects.twip
  /** point */
  def pt: LengthUnit = LengthUnitObjects.point
  /** point */
  def p: LengthUnit = LengthUnitObjects.point
  /** pica */
  def pica: LengthUnit = LengthUnitObjects.pica
  /** pica */
  def P: LengthUnit = LengthUnitObjects.pica
  /** line */
  def ln: LengthUnit = LengthUnitObjects.line
  /** inch */
  def in: LengthUnit = LengthUnitObjects.inch
  /** link */
  def li: LengthUnit = LengthUnitObjects.link
  /** link(US) */
  def li(a: linkAttribute): LengthUnit = a match {
    case MetricAttributes.US => LengthUnitObjects.`link(US)`
  }
  /** link */
  def lnk: LengthUnit = LengthUnitObjects.link
  /**   link(US) */
  def lnk(a: linkAttribute): LengthUnit = li(a)
  /** link(US) */
  def `li(US)`: LengthUnit = LengthUnitObjects.`link(US)`
  /** link(US) */
  def `lnk(US)`: LengthUnit = LengthUnitObjects.`link(US)`
  /** foot */
  def ft: LengthUnit = LengthUnitObjects.foot
  /** foot(US) */
  def ft(a: footAttribute): LengthUnit = a match {
    case MetricAttributes.US => LengthUnitObjects.`foot(US)`
  }
  /** foot(US) */
  def `ft(US)`: LengthUnit = LengthUnitObjects.`foot(US)`
  /** yard */
  def yd: LengthUnit = LengthUnitObjects.yard
  /** ell */
  def ell: LengthUnit = LengthUnitObjects.ell
  /** fathom */
  def ftm: LengthUnit = LengthUnitObjects.fathom
  /** rod */
  def rd: LengthUnit = LengthUnitObjects.rod
  /** rod(US) */
  def rd(a: rodAttribute): LengthUnit = a match {
    case MetricAttributes.US => LengthUnitObjects.`rod(US)`
  }
  /** rod(US) */
  def `rd(US)`: LengthUnit = LengthUnitObjects.`rod(US)`
  /** rope */
  def rope: LengthUnit = LengthUnitObjects.rope
  /** chain */
  def ch: LengthUnit = LengthUnitObjects.chain
  /** chain(US) */
  def ch(a: chainAttribute): LengthUnit = a match {
    case MetricAttributes.US => LengthUnitObjects.`chain(US)`
  }
  /** chain(US) */
  def `ch(US)`: LengthUnit = LengthUnitObjects.`chain(US)`
  /** mile */
  def mi: LengthUnit = LengthUnitObjects.mile
  /** mile(US) */
  def mi(a: mileAttribute): LengthUnit = a match {
    case MetricAttributes.US => LengthUnitObjects.`mile(US)`
  }
  /** mile(US) */
  def `mi(US)`: LengthUnit = LengthUnitObjects.`mile(US)`
  /** cable */
  def cb: LengthUnit = LengthUnitObjects.cable
  /** cable(US)<br/>cable(imp) */
  def cb(a: cableAttribute): LengthUnit = a match {
    case MetricAttributes.US => LengthUnitObjects.`cable(US)`
    case MetricAttributes.imp => LengthUnitObjects.`cable(imp)`
  }
  /** cable(US) */
  def `cb(US)`: LengthUnit = LengthUnitObjects.`cable(US)`
  /** cable(imp) */
  def `cb(imp)`: LengthUnit = LengthUnitObjects.`cable(imp)`
  /** league */
  def lea: LengthUnit = LengthUnitObjects.league
  /** league(US) */
  def lea(a: leagueAttribute): LengthUnit = a match {
    case MetricAttributes.US => LengthUnitObjects.`league(US)`
  }
  /** league(US) */
  def `lea(US)`: LengthUnit = LengthUnitObjects.`league(US)`
  /** nautical mile */
  def NM: LengthUnit = LengthUnitObjects.nautical_mile
  /** nautical_mile(Adm) */
  def NM(a: nautical_mileAttribute): LengthUnit = a match {
    case MetricAttributes.Adm => LengthUnitObjects.`nautical_mile(Adm)`
  }
  /** nautical mile */
  def nmi: LengthUnit = LengthUnitObjects.nautical_mile
  /**   nautical_mile(Adm) */
  def nmi(a: nautical_mileAttribute): LengthUnit = NM(a)
  /** nautical mile(Adm) */
  def `NM(Adm)`: LengthUnit = LengthUnitObjects.`nautical_mile(Adm)`
  /** nautical mile(Adm) */
  def `nmi(Adm)`: LengthUnit = LengthUnitObjects.`nautical_mile(Adm)`
  /** nautical league */
  def NL: LengthUnit = LengthUnitObjects.nautical_league
  /** nautical league */
  def nl: LengthUnit = LengthUnitObjects.nautical_league
  /** metric foot */
  def mf: LengthUnit = LengthUnitObjects.metric_foot
  /** short metric foot */
  def smf: LengthUnit = LengthUnitObjects.short_metric_foot
  /** long metric foot */
  def lmf: LengthUnit = LengthUnitObjects.long_metric_foot
  /** french */
  def Fr: LengthUnit = LengthUnitObjects.french
  /** furlong */
  def fur: LengthUnit = LengthUnitObjects.furlong
  /** furlong(US) */
  def fur(a: furlongAttribute): LengthUnit = a match {
    case MetricAttributes.US => LengthUnitObjects.`furlong(US)`
  }
  /** furlong(US) */
  def `fur(US)`: LengthUnit = LengthUnitObjects.`furlong(US)`
}