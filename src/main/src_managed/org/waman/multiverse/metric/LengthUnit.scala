package org.waman.multiverse.metric

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._

import org.waman.multiverse.time._
import org.waman.multiverse.mechanics._
import org.waman.multiverse.energy._

sealed trait LengthUnit extends PhysicalUnit[LengthUnit]
  with MultiplicativeByLengthUnit[AreaUnit]
  with MultiplicativeByForceUnit[EnergyUnit]
  with DivisibleByTimeUnit[VelocityUnit]
  with DivisibleByTimeSquaredUnit[AccelerationUnit]
  with CanSquare[AreaUnit]
  with CanCubic[VolumeUnit]{

  override def getSIUnit = org.waman.multiverse.metric.LengthUnit.Metre

  override def *(unit: LengthUnit) = AreaUnit(this, unit)

  override def *(unit: ForceUnit) = EnergyUnit(this, unit)

  override def /(unit: TimeUnit) = VelocityUnit(this, unit)

  override def /(unit: TimeSquaredUnit) = AccelerationUnit(this, unit)

  override def square: AreaUnit = this * this

  override def cubic: VolumeUnit = this * this * this
}

object LengthUnit extends ConstantsDefined[LengthUnit]{

  // intrinsic
  private[LengthUnit]
  class IntrinsicLengthUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends LengthUnit{

    def this(name: String, symbols: Seq[String], unit: LengthUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: LengthUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoMetre extends IntrinsicLengthUnit("YoctoMetre", Seq("ym"), r"1e-24")
  case object ZeptoMetre extends IntrinsicLengthUnit("ZeptoMetre", Seq("zm"), r"1e-21")
  case object AttoMetre extends IntrinsicLengthUnit("AttoMetre", Seq("am"), r"1e-18")
  case object FemtoMetre extends IntrinsicLengthUnit("FemtoMetre", Seq("fm"), r"1e-15")
  case object PicoMetre extends IntrinsicLengthUnit("PicoMetre", Seq("pm"), r"1e-12")
  case object NanoMetre extends IntrinsicLengthUnit("NanoMetre", Seq("nm"), r"1e-9")
  case object MicroMetre extends IntrinsicLengthUnit("MicroMetre", Seq("μm", "mcm"), r"1e-6")
  case object MilliMetre extends IntrinsicLengthUnit("MilliMetre", Seq("mm"), r"1e-3")
  case object CentiMetre extends IntrinsicLengthUnit("CentiMetre", Seq("cm"), r"1e-2")
  case object DeciMetre extends IntrinsicLengthUnit("DeciMetre", Seq("dm"), r"1e-1")
  case object Metre extends IntrinsicLengthUnit("Metre", Seq("m"), r"1")
  case object DecaMetre extends IntrinsicLengthUnit("DecaMetre", Seq("dam"), r"1e1")
  case object HectoMetre extends IntrinsicLengthUnit("HectoMetre", Seq("hm"), r"1e2")
  case object KiloMetre extends IntrinsicLengthUnit("KiloMetre", Seq("km"), r"1e3")
  case object MegaMetre extends IntrinsicLengthUnit("MegaMetre", Seq("Mm"), r"1e6")
  case object GigaMetre extends IntrinsicLengthUnit("GigaMetre", Seq("Gm"), r"1e9")
  case object TeraMetre extends IntrinsicLengthUnit("TeraMetre", Seq("Tm"), r"1e12")
  case object PetaMetre extends IntrinsicLengthUnit("PetaMetre", Seq("Pm"), r"1e15")
  case object ExaMetre extends IntrinsicLengthUnit("ExaMetre", Seq("Em"), r"1e18")
  case object ZettaMetre extends IntrinsicLengthUnit("ZettaMetre", Seq("Zm"), r"1e21")
  case object YottaMetre extends IntrinsicLengthUnit("YottaMetre", Seq("Ym"), r"1e24")
  case object Micron extends IntrinsicLengthUnit("Micron", Seq("µ"), r"1e-6")
  case object Angstrom extends IntrinsicLengthUnit("Angstrom", Seq("Å"), r"1e-10")
  case object AtomicUnitOfLength extends IntrinsicLengthUnit("AtomicUnitOfLength", Seq("a0"), r"5.291772109217e-11") with NotExact
  case object XUnit extends IntrinsicLengthUnit("XUnit", Seq("xu"), r"1.0021e-13") with NotExact
  case object XUnit_CuKAlpha1 extends IntrinsicLengthUnit("XUnit_CuKAlpha1", Seq("xu(CuKα1)"), r"1.0020769928e-13") with NotExact
  case object XUnit_MoKAlpha1 extends IntrinsicLengthUnit("XUnit_MoKAlpha1", Seq("xu(MoKα1)"), r"1.0020995553e-13") with NotExact
  case object PlanckLength extends IntrinsicLengthUnit("PlanckLength", Seq("lp", "l_p"), r"1.61624e-35") with NotExact
  case object AstronomicalUnit extends IntrinsicLengthUnit("AstronomicalUnit", Seq("au"), r"149597870700")
  case object LightYear extends IntrinsicLengthUnit("LightYear", Seq("ly"), r"9.4607304725808e15")
  case object Parsec extends IntrinsicLengthUnit("Parsec", Seq("pc"), r"3.08567782e16") with NotExact
  case object Mil extends IntrinsicLengthUnit("Mil", Seq("mil", "thou"), r"1/1000", Inch)
  case object Twip extends IntrinsicLengthUnit("Twip", Seq("twp"), r"1/20", Point)
  case object Point extends IntrinsicLengthUnit("Point", Seq("pt"), r"1/72", Inch)
  case object Line extends IntrinsicLengthUnit("Line", Seq("ln"), r"1/12", Inch)
  case object Inch extends IntrinsicLengthUnit("Inch", Seq("in"), r"2.54", CentiMetre)
  case object Link extends IntrinsicLengthUnit("Link", Seq("li", "lnk"), r"0.66", Foot)
  case object Foot extends IntrinsicLengthUnit("Foot", Seq("ft"), 12, Inch)
  case object Yard extends IntrinsicLengthUnit("Yard", Seq("yd"), 3, Foot)
  case object Ell extends IntrinsicLengthUnit("Ell", Seq("ell"), 45, Inch)
  case object Fathom extends IntrinsicLengthUnit("Fathom", Seq("ftm"), 6, Foot)
  case object Rod extends IntrinsicLengthUnit("Rod", Seq("rd"), r"16.5", Foot)
  case object Rope extends IntrinsicLengthUnit("Rope", Seq("rope"), 20, Foot)
  case object Chain extends IntrinsicLengthUnit("Chain", Seq("ch"), 66, Foot)
  case object Mile extends IntrinsicLengthUnit("Mile", Seq("mi"), 1760, Yard)
  case object League extends IntrinsicLengthUnit("League", Seq("lea"), 3, Mile)
  case object NauticalMile extends IntrinsicLengthUnit("NauticalMile", Seq("NM", "nmi"), 1852)
  case object NauticalMile_Admiralty extends IntrinsicLengthUnit("NauticalMile_Admiralty", Seq("NM(Adm)", "nmi(Adm)"), 6080, Foot)
  case object NauticalLeague extends IntrinsicLengthUnit("NauticalLeague", Seq("NL", "nl"), 3, NauticalMile)
  case object Cable extends IntrinsicLengthUnit("Cable", Seq("cb"), r"1/10", NauticalMile)
  case object Cable_US extends IntrinsicLengthUnit("Cable_US", Seq("cb(US)"), 720, Foot)
  case object Cable_imperial extends IntrinsicLengthUnit("Cable_imperial", Seq("cb(imp)"), 608, Foot)
  case object Link_US_Survey extends IntrinsicLengthUnit("Link_US_Survey", Seq("li(US)", "lnk(US)"), r"0.66", Foot_US_Survey)
  case object Foot_US_Survey extends IntrinsicLengthUnit("Foot_US_Survey", Seq("ft(US)"), r"1200/3937")
  case object Chain_US_Survey extends IntrinsicLengthUnit("Chain_US_Survey", Seq("ch(US)"), 66, Foot_US_Survey)
  case object Mile_US_Survey extends IntrinsicLengthUnit("Mile_US_Survey", Seq("mi(US)"), 5280, Foot_US_Survey)
  case object MetricFoot extends IntrinsicLengthUnit("MetricFoot", Seq("mf"), Real(r"1/10").sqrt())
  case object ShortMetricFoot extends IntrinsicLengthUnit("ShortMetricFoot", Seq("smf"), r"0.3")
  case object LongMetricFoot extends IntrinsicLengthUnit("LongMetricFoot", Seq("lmf"), r"1/3")
  case object French extends IntrinsicLengthUnit("French", Seq("Fr"), r"1/3", MilliMetre)
  case object Furlong extends IntrinsicLengthUnit("Furlong", Seq("fur"), 660, Foot)

  override lazy val values = Seq(YoctoMetre, ZeptoMetre, AttoMetre, FemtoMetre, PicoMetre, NanoMetre, MicroMetre, MilliMetre, CentiMetre, DeciMetre, Metre, DecaMetre, HectoMetre, KiloMetre, MegaMetre, GigaMetre, TeraMetre, PetaMetre, ExaMetre, ZettaMetre, YottaMetre, Micron, Angstrom, AtomicUnitOfLength, XUnit, XUnit_CuKAlpha1, XUnit_MoKAlpha1, PlanckLength, AstronomicalUnit, LightYear, Parsec, Mil, Twip, Point, Line, Inch, Link, Foot, Yard, Ell, Fathom, Rod, Rope, Chain, Mile, League, NauticalMile, NauticalMile_Admiralty, NauticalLeague, Cable, Cable_US, Cable_imperial, Link_US_Survey, Foot_US_Survey, Chain_US_Survey, Mile_US_Survey, MetricFoot, ShortMetricFoot, LongMetricFoot, French, Furlong)
}

trait MultiplicativeByLengthUnit[R]{
  def *(unit: LengthUnit): R
}

trait DivisibleByLengthUnit[R]{
  def /(unit: LengthUnit): R
}

trait LengthPostfixOps[A]{
  import LengthUnit._

  protected def lengthPostfixOps(unit: LengthUnit): A


  def ym : A = lengthPostfixOps(YoctoMetre)
  def zm : A = lengthPostfixOps(ZeptoMetre)
  def am : A = lengthPostfixOps(AttoMetre)
  def fm : A = lengthPostfixOps(FemtoMetre)
  def pm : A = lengthPostfixOps(PicoMetre)
  def nm : A = lengthPostfixOps(NanoMetre)
  def μm : A = lengthPostfixOps(MicroMetre)
  def mcm : A = lengthPostfixOps(MicroMetre)
  def mm : A = lengthPostfixOps(MilliMetre)
  def cm : A = lengthPostfixOps(CentiMetre)
  def dm : A = lengthPostfixOps(DeciMetre)
  def m : A = lengthPostfixOps(Metre)
  def dam : A = lengthPostfixOps(DecaMetre)
  def hm : A = lengthPostfixOps(HectoMetre)
  def km : A = lengthPostfixOps(KiloMetre)
  def Mm : A = lengthPostfixOps(MegaMetre)
  def Gm : A = lengthPostfixOps(GigaMetre)
  def Tm : A = lengthPostfixOps(TeraMetre)
  def Pm : A = lengthPostfixOps(PetaMetre)
  def Em : A = lengthPostfixOps(ExaMetre)
  def Zm : A = lengthPostfixOps(ZettaMetre)
  def Ym : A = lengthPostfixOps(YottaMetre)
  def µ : A = lengthPostfixOps(Micron)
  def Å : A = lengthPostfixOps(Angstrom)
  def a0 : A = lengthPostfixOps(AtomicUnitOfLength)
  def xu : A = lengthPostfixOps(XUnit)
  def lp : A = lengthPostfixOps(PlanckLength)
  def l_p : A = lengthPostfixOps(PlanckLength)
  def au : A = lengthPostfixOps(AstronomicalUnit)
  def ly : A = lengthPostfixOps(LightYear)
  def pc : A = lengthPostfixOps(Parsec)
  def mil : A = lengthPostfixOps(Mil)
  def thou : A = lengthPostfixOps(Mil)
  def twp : A = lengthPostfixOps(Twip)
  def pt : A = lengthPostfixOps(Point)
  def ln : A = lengthPostfixOps(Line)
  def in : A = lengthPostfixOps(Inch)
  def li : A = lengthPostfixOps(Link)
  def lnk : A = lengthPostfixOps(Link)
  def ft : A = lengthPostfixOps(Foot)
  def yd : A = lengthPostfixOps(Yard)
  def ell : A = lengthPostfixOps(Ell)
  def ftm : A = lengthPostfixOps(Fathom)
  def rd : A = lengthPostfixOps(Rod)
  def rope : A = lengthPostfixOps(Rope)
  def ch : A = lengthPostfixOps(Chain)
  def mi : A = lengthPostfixOps(Mile)
  def lea : A = lengthPostfixOps(League)
  def NM : A = lengthPostfixOps(NauticalMile)
  def nmi : A = lengthPostfixOps(NauticalMile)
  def NL : A = lengthPostfixOps(NauticalLeague)
  def nl : A = lengthPostfixOps(NauticalLeague)
  def cb : A = lengthPostfixOps(Cable)
  def mf : A = lengthPostfixOps(MetricFoot)
  def smf : A = lengthPostfixOps(ShortMetricFoot)
  def lmf : A = lengthPostfixOps(LongMetricFoot)
  def Fr : A = lengthPostfixOps(French)
  def fur : A = lengthPostfixOps(Furlong)

  import LengthPostfixOps._
  import org.waman.multiverse.metric.MetricContext
  import MetricContext._

  def xu(c: MetricContext): A = lengthPostfixOps(_xu(c))
  def NM(c: MetricContext): A = lengthPostfixOps(_NM(c))
  def nmi(c: MetricContext): A = lengthPostfixOps(_nmi(c))
  def cb(c: MetricContext): A = lengthPostfixOps(_cb(c))
  def li(c: MetricContext): A = lengthPostfixOps(_li(c))
  def lnk(c: MetricContext): A = lengthPostfixOps(_lnk(c))
  def ft(c: MetricContext): A = lengthPostfixOps(_ft(c))
  def ch(c: MetricContext): A = lengthPostfixOps(_ch(c))
  def mi(c: MetricContext): A = lengthPostfixOps(_mi(c))
}

object LengthPostfixOps{
  import LengthUnit._
  import org.waman.multiverse.metric.MetricContext
  import MetricContext._


  lazy val _cb : PartialFunction[MetricContext, LengthUnit] = {
    case UnitedStates => Cable_US
    case Imperial => Cable_imperial
  }

  lazy val _NM : PartialFunction[MetricContext, LengthUnit] = {
    case Admiralty => NauticalMile_Admiralty
  }

  lazy val _nmi : PartialFunction[MetricContext, LengthUnit] = {
    case Admiralty => NauticalMile_Admiralty
  }

  lazy val _li : PartialFunction[MetricContext, LengthUnit] = {
    case UnitedStates => Link_US_Survey
  }

  lazy val _lnk : PartialFunction[MetricContext, LengthUnit] = {
    case UnitedStates => Link_US_Survey
  }

  lazy val _ch : PartialFunction[MetricContext, LengthUnit] = {
    case UnitedStates => Chain_US_Survey
  }

  lazy val _ft : PartialFunction[MetricContext, LengthUnit] = {
    case UnitedStates => Foot_US_Survey
  }

  lazy val _mi : PartialFunction[MetricContext, LengthUnit] = {
    case UnitedStates => Mile_US_Survey
  }

  lazy val _xu : PartialFunction[MetricContext, LengthUnit] = {
    case Cu_KAlpha1 => XUnit_CuKAlpha1
    case Mo_KAlpha1 => XUnit_MoKAlpha1
  }
}

trait LengthDot[A]{
  import LengthUnit._

  protected def lengthDot(unit: LengthUnit): A

  def ym(dot: Dot): A = lengthDot(YoctoMetre)
  def zm(dot: Dot): A = lengthDot(ZeptoMetre)
  def am(dot: Dot): A = lengthDot(AttoMetre)
  def fm(dot: Dot): A = lengthDot(FemtoMetre)
  def pm(dot: Dot): A = lengthDot(PicoMetre)
  def nm(dot: Dot): A = lengthDot(NanoMetre)
  def μm(dot: Dot): A = lengthDot(MicroMetre)
  def mcm(dot: Dot): A = lengthDot(MicroMetre)
  def mm(dot: Dot): A = lengthDot(MilliMetre)
  def cm(dot: Dot): A = lengthDot(CentiMetre)
  def dm(dot: Dot): A = lengthDot(DeciMetre)
  def m(dot: Dot): A = lengthDot(Metre)
  def dam(dot: Dot): A = lengthDot(DecaMetre)
  def hm(dot: Dot): A = lengthDot(HectoMetre)
  def km(dot: Dot): A = lengthDot(KiloMetre)
  def Mm(dot: Dot): A = lengthDot(MegaMetre)
  def Gm(dot: Dot): A = lengthDot(GigaMetre)
  def Tm(dot: Dot): A = lengthDot(TeraMetre)
  def Pm(dot: Dot): A = lengthDot(PetaMetre)
  def Em(dot: Dot): A = lengthDot(ExaMetre)
  def Zm(dot: Dot): A = lengthDot(ZettaMetre)
  def Ym(dot: Dot): A = lengthDot(YottaMetre)
  def µ(dot: Dot): A = lengthDot(Micron)
  def Å(dot: Dot): A = lengthDot(Angstrom)
  def a0(dot: Dot): A = lengthDot(AtomicUnitOfLength)
  def xu(dot: Dot): A = lengthDot(XUnit)
  def lp(dot: Dot): A = lengthDot(PlanckLength)
  def l_p(dot: Dot): A = lengthDot(PlanckLength)
  def au(dot: Dot): A = lengthDot(AstronomicalUnit)
  def ly(dot: Dot): A = lengthDot(LightYear)
  def pc(dot: Dot): A = lengthDot(Parsec)
  def mil(dot: Dot): A = lengthDot(Mil)
  def thou(dot: Dot): A = lengthDot(Mil)
  def twp(dot: Dot): A = lengthDot(Twip)
  def pt(dot: Dot): A = lengthDot(Point)
  def ln(dot: Dot): A = lengthDot(Line)
  def in(dot: Dot): A = lengthDot(Inch)
  def li(dot: Dot): A = lengthDot(Link)
  def lnk(dot: Dot): A = lengthDot(Link)
  def ft(dot: Dot): A = lengthDot(Foot)
  def yd(dot: Dot): A = lengthDot(Yard)
  def ell(dot: Dot): A = lengthDot(Ell)
  def ftm(dot: Dot): A = lengthDot(Fathom)
  def rd(dot: Dot): A = lengthDot(Rod)
  def rope(dot: Dot): A = lengthDot(Rope)
  def ch(dot: Dot): A = lengthDot(Chain)
  def mi(dot: Dot): A = lengthDot(Mile)
  def lea(dot: Dot): A = lengthDot(League)
  def NM(dot: Dot): A = lengthDot(NauticalMile)
  def nmi(dot: Dot): A = lengthDot(NauticalMile)
  def NL(dot: Dot): A = lengthDot(NauticalLeague)
  def nl(dot: Dot): A = lengthDot(NauticalLeague)
  def cb(dot: Dot): A = lengthDot(Cable)
  def mf(dot: Dot): A = lengthDot(MetricFoot)
  def smf(dot: Dot): A = lengthDot(ShortMetricFoot)
  def lmf(dot: Dot): A = lengthDot(LongMetricFoot)
  def Fr(dot: Dot): A = lengthDot(French)
  def fur(dot: Dot): A = lengthDot(Furlong)
}

trait LengthPer[A]{
  import LengthUnit._

  protected def lengthPer(unit: LengthUnit): A

  def ym(per: Per): A = lengthPer(YoctoMetre)
  def zm(per: Per): A = lengthPer(ZeptoMetre)
  def am(per: Per): A = lengthPer(AttoMetre)
  def fm(per: Per): A = lengthPer(FemtoMetre)
  def pm(per: Per): A = lengthPer(PicoMetre)
  def nm(per: Per): A = lengthPer(NanoMetre)
  def μm(per: Per): A = lengthPer(MicroMetre)
  def mcm(per: Per): A = lengthPer(MicroMetre)
  def mm(per: Per): A = lengthPer(MilliMetre)
  def cm(per: Per): A = lengthPer(CentiMetre)
  def dm(per: Per): A = lengthPer(DeciMetre)
  def m(per: Per): A = lengthPer(Metre)
  def dam(per: Per): A = lengthPer(DecaMetre)
  def hm(per: Per): A = lengthPer(HectoMetre)
  def km(per: Per): A = lengthPer(KiloMetre)
  def Mm(per: Per): A = lengthPer(MegaMetre)
  def Gm(per: Per): A = lengthPer(GigaMetre)
  def Tm(per: Per): A = lengthPer(TeraMetre)
  def Pm(per: Per): A = lengthPer(PetaMetre)
  def Em(per: Per): A = lengthPer(ExaMetre)
  def Zm(per: Per): A = lengthPer(ZettaMetre)
  def Ym(per: Per): A = lengthPer(YottaMetre)
  def µ(per: Per): A = lengthPer(Micron)
  def Å(per: Per): A = lengthPer(Angstrom)
  def a0(per: Per): A = lengthPer(AtomicUnitOfLength)
  def xu(per: Per): A = lengthPer(XUnit)
  def lp(per: Per): A = lengthPer(PlanckLength)
  def l_p(per: Per): A = lengthPer(PlanckLength)
  def au(per: Per): A = lengthPer(AstronomicalUnit)
  def ly(per: Per): A = lengthPer(LightYear)
  def pc(per: Per): A = lengthPer(Parsec)
  def mil(per: Per): A = lengthPer(Mil)
  def thou(per: Per): A = lengthPer(Mil)
  def twp(per: Per): A = lengthPer(Twip)
  def pt(per: Per): A = lengthPer(Point)
  def ln(per: Per): A = lengthPer(Line)
  def in(per: Per): A = lengthPer(Inch)
  def li(per: Per): A = lengthPer(Link)
  def lnk(per: Per): A = lengthPer(Link)
  def ft(per: Per): A = lengthPer(Foot)
  def yd(per: Per): A = lengthPer(Yard)
  def ell(per: Per): A = lengthPer(Ell)
  def ftm(per: Per): A = lengthPer(Fathom)
  def rd(per: Per): A = lengthPer(Rod)
  def rope(per: Per): A = lengthPer(Rope)
  def ch(per: Per): A = lengthPer(Chain)
  def mi(per: Per): A = lengthPer(Mile)
  def lea(per: Per): A = lengthPer(League)
  def NM(per: Per): A = lengthPer(NauticalMile)
  def nmi(per: Per): A = lengthPer(NauticalMile)
  def NL(per: Per): A = lengthPer(NauticalLeague)
  def nl(per: Per): A = lengthPer(NauticalLeague)
  def cb(per: Per): A = lengthPer(Cable)
  def mf(per: Per): A = lengthPer(MetricFoot)
  def smf(per: Per): A = lengthPer(ShortMetricFoot)
  def lmf(per: Per): A = lengthPer(LongMetricFoot)
  def Fr(per: Per): A = lengthPer(French)
  def fur(per: Per): A = lengthPer(Furlong)
}

trait PredefinedLengthUnit extends LengthPostfixOps[LengthUnit]{
  override protected def lengthPostfixOps(unit: LengthUnit) = unit
  
}

object PredefinedLengthUnit extends PredefinedLengthUnit
