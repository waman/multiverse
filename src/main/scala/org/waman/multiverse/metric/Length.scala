package org.waman.multiverse.metric

import org.waman.multiverse.Context._
import org.waman.multiverse._
import org.waman.multiverse.mechanics.{Acceleration, AccelerationUnit, Velocity, VelocityUnit}
import org.waman.multiverse.time.{TimePostfixOps, TimeSquaredPostfixOps, TimeSquaredUnit, TimeUnit}
import spire.implicits._
import spire.math.{Fractional, Real}

/*
 * +++++ For Length +++++
 * 1.0.m --- LengthUnitInterpreter.m
 * 1.0 m --- LengthUnitInterpreter.m
 * 1.0 (m) ( = 1.0.apply(m)) --- LengthUnitInterpreter.apply(LengthUnit)
 *
 * length.m --- Length.m
 * length m --- Length.m
 * length (m) ( = length.apply(m)) --- Length.apply(LengthUnit)
 *
 * +++++ For Velocity +++++
 * 1.0.m/s --- Length#/(TimeUnit)
 * 1.0 m/s ( = 1.0.m(/).s) --- LengthUnitInterpreter.m(Per): TimePostfixOps, TimePostfixOps.s
 * 1.0 (m/s) ( = 1.0.apply(m./(s))) --- LengthUnit./(TimeUnit): VelocityUnit, LengthUnitInterpreter.apply(VelocityUnit)
 *
 * velocity.m/s ( = velocity.m./(s)) --- Velocity.m: DivisibleByTime, DivisibleByTime./(s)
 * velocity m/s ( = velocity.m(/).s) --- Velocity.m(Per): TimePostfixOps, TimePostfixOps.s
 * velocity (m/s) ( = velocity.apply(m./(s))) --- LengthUnit./(TimeUnit): VelocityUnit, Velocity.apply(VelocityUnit)
 */

trait LengthPostfixOps[A]{
  import LengthPostfixOps._
  import LengthUnit._

  protected def lengthPostfixOps(lengthUnit: LengthUnit): A

  def ym: A = lengthPostfixOps(YoctoMetre)
  def zm: A = lengthPostfixOps(ZeptoMetre)
  def am: A = lengthPostfixOps(AttoMetre)
  def fm: A = lengthPostfixOps(FemtoMetre)
  def pm: A = lengthPostfixOps(PicoMetre)
  def nm: A = lengthPostfixOps(NanoMetre)
  def μm: A = lengthPostfixOps(MicroMetre)
  def mm: A = lengthPostfixOps(MilliMetre)
  def cm: A = lengthPostfixOps(CentiMetre)
  def dm: A = lengthPostfixOps(DeciMetre)
  def m : A = lengthPostfixOps(Metre)
  def dam: A = lengthPostfixOps(DecaMetre)
  def hm: A = lengthPostfixOps(HectoMetre)
  def km: A = lengthPostfixOps(KiloMetre)
  def Mm: A = lengthPostfixOps(MegaMetre)
  def Gm: A = lengthPostfixOps(GigaMetre)
  def Tm: A = lengthPostfixOps(TeraMetre)
  def Pm: A = lengthPostfixOps(PetaMetre)
  def Em: A = lengthPostfixOps(ExaMetre)
  def Zm: A = lengthPostfixOps(ZettaMetre)
  def Ym: A = lengthPostfixOps(YottaMetre)

  // microscopic
  def μ: A = lengthPostfixOps(Micron)
  def Å: A = lengthPostfixOps(Angstrom)
  def a0: A = lengthPostfixOps(AtomicUnitOfLength)
  def xu: A = lengthPostfixOps(XUnit)
  def xu(c: Context): A = lengthPostfixOps(_xu(c))
  def lp: A = lengthPostfixOps(PlanckLength)

  // astronomy
  def au: A = lengthPostfixOps(AstronomicalUnit)
  def ly: A = lengthPostfixOps(LightYear)
  def pc: A = lengthPostfixOps(Parsec)

  // yard-pond
  def mil : A = lengthPostfixOps(Mil)
  def thou: A = mil
  def twp : A = lengthPostfixOps(Twip)
  def pt  : A = lengthPostfixOps(Point)
  def ln  : A = lengthPostfixOps(Line)
  def in  : A = lengthPostfixOps(Inch)
  def li  : A = lengthPostfixOps(Link)
  def lnk : A = li
  def ft  : A = lengthPostfixOps(Foot)
  def yd  : A = lengthPostfixOps(Yard)
  def ell : A = lengthPostfixOps(Ell)
  def ftm : A = lengthPostfixOps(Fathom)
  def rd  : A = lengthPostfixOps(Rod)
  def rope: A = lengthPostfixOps(Rope)
  def ch  : A = lengthPostfixOps(Chain)
  def mi  : A = lengthPostfixOps(Mile)
  def lea : A = lengthPostfixOps(League)

  def nmi: A = lengthPostfixOps(NauticalMile)
  def nmi(c: Context): A = lengthPostfixOps(_nmi(c))
  def NM: A = nmi
  def NM(c: Context): A = lengthPostfixOps(_NM(c))
  def nl: A = lengthPostfixOps(NauticalLeague)
  def NL: A = nl
  def cb: A = lengthPostfixOps(Cable)
  def cb(c: Context): A = lengthPostfixOps(_cb(c))

  def li (c: Context): A = lengthPostfixOps(_li(c))
  def lnk(c: Context): A = lengthPostfixOps(_lnk(c))
  def ft (c: Context): A = lengthPostfixOps(_ft(c))
  def mi (c: Context): A = lengthPostfixOps(_mi(c))

  def mf : A = lengthPostfixOps(MetricFoot)
  def smf: A = lengthPostfixOps(ShortMetricFoot)
  def lmf: A = lengthPostfixOps(LongMetricFoot)

  def Fr : A = lengthPostfixOps(French)
  def fur: A = lengthPostfixOps(Furlong)
}

object LengthPostfixOps{
  lazy val _xu: PartialFunction[Context, LengthUnit] = {
    case Cu_KAlpha1 => LengthUnit.XUnit_CuKAlpha1
    case Mo_KAlpha1 => LengthUnit.XUnit_MoKAlpha1
  }

  lazy val _nmi: PartialFunction[Context, LengthUnit] = {
    case Admiralty => LengthUnit.NauticalMile_Admiralty
  }

  lazy val _NM: PartialFunction[Context, LengthUnit] = _nmi

  lazy val _li: PartialFunction[Context, LengthUnit] = {
    case UnitedStates => LengthUnit.Link_US_Survey
  }

  lazy val _lnk: PartialFunction[Context, LengthUnit] = _li

  lazy val _ft: PartialFunction[Context, LengthUnit] = {
    case UnitedStates => LengthUnit.Foot_US_Survey
  }

  lazy val _mi: PartialFunction[Context, LengthUnit] = {
    case UnitedStates => LengthUnit.Mile_US_Survey
  }

  lazy val _cb: PartialFunction[Context, LengthUnit] = {
    case UnitedStates => LengthUnit.Cable_US
    case Imperial => LengthUnit.Cable_imperial
  }
}

trait LengthPer[A]{
  import LengthUnit._

  protected def lengthPer(lengthUnit: LengthUnit): A

  def ym(per: Per): A = lengthPer(YoctoMetre)
  def zm(per: Per): A = lengthPer(ZeptoMetre)
  def am(per: Per): A = lengthPer(AttoMetre)
  def fm(per: Per): A = lengthPer(FemtoMetre)
  def pm(per: Per): A = lengthPer(PicoMetre)
  def nm(per: Per): A = lengthPer(NanoMetre)
  def μm(per: Per): A = lengthPer(MicroMetre)
  def mm(per: Per): A = lengthPer(MilliMetre)
  def cm(per: Per): A = lengthPer(CentiMetre)
  def dm(per: Per): A = lengthPer(DeciMetre)
  def m (per: Per): A = lengthPer(Metre)
  def dam(per: Per):A = lengthPer(DecaMetre)
  def hm(per: Per): A = lengthPer(HectoMetre)
  def km(per: Per): A = lengthPer(KiloMetre)
  def Mm(per: Per): A = lengthPer(MegaMetre)
  def Gm(per: Per): A = lengthPer(GigaMetre)
  def Tm(per: Per): A = lengthPer(TeraMetre)
  def Pm(per: Per): A = lengthPer(PetaMetre)
  def Em(per: Per): A = lengthPer(ExaMetre)
  def Zm(per: Per): A = lengthPer(ZettaMetre)
  def Ym(per: Per): A = lengthPer(YottaMetre)

  // microscopic
  def μ(per: Per): A = lengthPer(Micron)
  def Å(per: Per): A = lengthPer(Angstrom)
  def a0(per: Per): A = lengthPer(AtomicUnitOfLength)
  def xu(per: Per): A = lengthPer(XUnit)
  def lp(per: Per): A = lengthPer(PlanckLength)

  // astronomy
  def au(per: Per): A = lengthPer(AstronomicalUnit)
  def ly(per: Per): A = lengthPer(LightYear)
  def pc(per: Per): A = lengthPer(Parsec)

  // yard-pond
  def mil (per: Per): A = lengthPer(Mil)
  def thou(per: Per): A = mil(per)
  def twp (per: Per): A = lengthPer(Twip)
  def pt  (per: Per): A = lengthPer(Point)
  def ln  (per: Per): A = lengthPer(Line)
  def in  (per: Per): A = lengthPer(Inch)
  def li  (per: Per): A = lengthPer(Link)
  def lnk (per: Per): A = li(per)
  def ft  (per: Per): A = lengthPer(Foot)
  def yd  (per: Per): A = lengthPer(Yard)
  def ell (per: Per): A = lengthPer(Ell)
  def ftm (per: Per): A = lengthPer(Fathom)
  def rd  (per: Per): A = lengthPer(Rod)
  def rope(per: Per): A = lengthPer(Rope)
  def ch  (per: Per): A = lengthPer(Chain)
  def mi  (per: Per): A = lengthPer(Mile)
  def lea (per: Per): A = lengthPer(League)

  def nmi(per: Per): A = lengthPer(NauticalMile)
  def NM(per: Per): A = nmi(per)
  def nl(per: Per): A = lengthPer(NauticalLeague)
  def NL(per: Per): A = nl(per)
  def cb(per: Per): A = lengthPer(Cable)

  def mf (per: Per): A = lengthPer(MetricFoot)
  def smf(per: Per): A = lengthPer(ShortMetricFoot)
  def lmf(per: Per): A = lengthPer(LongMetricFoot)

  def Fr (per: Per): A = lengthPer(French)
  def fur(per: Per): A = lengthPer(Furlong)
}

class Length[A: Fractional](val value: A, val unit: LengthUnit)
    extends Quantity[A, LengthUnit]
    with LengthPostfixOps[A]
    with DivisibleByTime[Velocity[A]]
    with DivisibleByTimeSquared[Acceleration[A]]
    with UnitConverter[A]{

  protected lazy val algebra = implicitly[Fractional[A]]

  /**
    *  For style like <code>length.m</code> and <code>length m</code>
    *  where <code>length</code> is a Length object
    */
  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(lengthUnit)

  /** For style like <code>length (m)</code> where <code>length</code> is a Length object*/
  def apply(evalUnit: LengthUnit): A =
    if(unit == evalUnit) value
    else value * real(unit.unitInMetre) / real(evalUnit.unitInMetre)

  /** For style like <code>1.0.m/s</code> */
  override def /(timeUnit: TimeUnit): Velocity[A] = new Velocity(value, unit / timeUnit)

  override def /(timeSquaredUnit: TimeSquaredUnit): Acceleration[A] =
    new Acceleration[A](value, unit / timeSquaredUnit)
}

sealed abstract class LengthUnit(val symbol: String, val unitInMetre: Real)
    extends PhysicalUnit[LengthUnit]
    with Ordered[LengthUnit]
    with DivisibleByTime[VelocityUnit] // for style like "1.0 (m/s)" ( = "1.0.apply(m./(s))")
    with DivisibleByTimeSquared[AccelerationUnit]{

  def this(symbol: String, factor: Real, lengthUnit: LengthUnit) =
    this(symbol, factor * lengthUnit.unitInMetre)

  override val baseUnit = LengthUnit.Metre
  override val inBaseUnitAccessor = () => unitInMetre

  override def /(timeUnit: TimeUnit) = VelocityUnit(this, timeUnit)
  override def /(timeSquaredUnit: TimeSquaredUnit) = AccelerationUnit(this, timeSquaredUnit)

  override def compare(that: LengthUnit): Int = unitInMetre.compare(that.unitInMetre)
}

object LengthUnit{

  case object YoctoMetre extends LengthUnit("ym", r"1e-24")
  case object ZeptoMetre extends LengthUnit("zm", r"1e-21")
  case object AttoMetre  extends LengthUnit("am", r"1e-18")
  case object FemtoMetre extends LengthUnit("am", r"1e-15")
  case object PicoMetre  extends LengthUnit("pm", r"1e-12")
  case object NanoMetre  extends LengthUnit("nm", r"1e-9")
  case object MicroMetre extends LengthUnit("μm", r"1e-6")
  case object MilliMetre extends LengthUnit("mm", r"1e-3")
  case object CentiMetre extends LengthUnit("cm", r"1e-2")
  case object DeciMetre  extends LengthUnit("dm", r"1e-1")
  case object Metre      extends LengthUnit("m" , 1)
  case object DecaMetre  extends LengthUnit("dam", r"1e1")
  case object HectoMetre extends LengthUnit("hm", r"1e2")
  case object KiloMetre  extends LengthUnit("km", r"1e3")
  case object MegaMetre  extends LengthUnit("Mm", r"1e6")
  case object GigaMetre  extends LengthUnit("Gm", r"1e9")
  case object TeraMetre  extends LengthUnit("Tm", r"1e12")
  case object PetaMetre  extends LengthUnit("Pm", r"1e15")
  case object ExaMetre   extends LengthUnit("Em", r"1e18")
  case object ZettaMetre extends LengthUnit("Zm", r"1e21")
  case object YottaMetre extends LengthUnit("Ym", r"1e24")

  // microscopic
  case object Micron             extends LengthUnit("μ" , r"1e-6")
  case object Angstrom           extends LengthUnit("Å" , r"1e-10")
  case object AtomicUnitOfLength extends LengthUnit("a0", r"5.291772109217e-11") with NotExact
  case object XUnit              extends LengthUnit("xu", r"1.0021e-13") with NotExact
  case object XUnit_CuKAlpha1    extends LengthUnit("xu(CuKα1)", r"1.0020769928e-13") with NotExact
  case object XUnit_MoKAlpha1    extends LengthUnit("xu(MoKα1)", r"1.0020995553e-13") with NotExact
  case object PlanckLength       extends LengthUnit("lp", r"1.61624e-35") with NotExact

  // astronomy
  case object AstronomicalUnit extends LengthUnit("au", r"149597870700")
  case object LightYear        extends LengthUnit("ly", r"9.4607304725808e15")
  case object Parsec           extends LengthUnit("pc", r"3.08567782e16") with NotExact

  // yard-pond
  case object Mil    extends LengthUnit("mil;thou", r"1/1000", Inch)
  case object Twip   extends LengthUnit("twp", r"1/20", Point)
  case object Point  extends LengthUnit("pt"  , r"1/72", Inch)
  case object Line   extends LengthUnit("ln"  , r"1/12", Inch)
  case object Inch   extends LengthUnit("in"  , r"2.54", CentiMetre)
  case object Foot   extends LengthUnit("ft"  , 12, Inch)
  case object Link   extends LengthUnit("li;lnk", r"0.66", Foot)
  case object Yard   extends LengthUnit("yd"  , 3, Foot)
  case object Ell    extends LengthUnit("ell" , 45, Inch)
  case object Fathom extends LengthUnit("ftm" , 6, Foot)
  case object Rod    extends LengthUnit("rd"  , r"16.5", Foot)
  case object Rope   extends LengthUnit("rope", 20, Foot)
  case object Chain  extends LengthUnit("ch"  , 66, Foot)
  case object Mile   extends LengthUnit("mi"  , 1760, Yard)
  case object League extends LengthUnit("lea" , 3, Mile)

  case object NauticalMile           extends LengthUnit("NM;nmi", 1852)
  case object NauticalMile_Admiralty extends LengthUnit("NM(Adm);nmi(Adm)", 6080, Foot)
  case object NauticalLeague         extends LengthUnit("NL;nl", 3, NauticalMile)
  case object Cable          extends LengthUnit("cb"     , r"1/10", NauticalMile)
  case object Cable_US       extends LengthUnit("cb(US)" , 720, Foot)
  case object Cable_imperial extends LengthUnit("cb(imp)", 608, Foot)

  case object Link_US_Survey  extends LengthUnit("lnk(US)", r"0.66", Foot_US_Survey)
  case object Foot_US_Survey  extends LengthUnit("ft(US)", r"1200/3937")
  case object Chain_US_Survey extends LengthUnit("ch(US)", 66, Foot_US_Survey)
  case object Mile_US_Survey  extends LengthUnit("mi(US)", 5280, Foot_US_Survey)

  case object MetricFoot      extends LengthUnit("mf", Real(r"1/10").sqrt())
  case object ShortMetricFoot extends LengthUnit("smf", r"0.3")
  case object LongMetricFoot  extends LengthUnit("lmf", r"1/3")

  case object French  extends LengthUnit("Fr", r"1/3", MilliMetre)
  case object Furlong extends LengthUnit("fur", 660, Foot)
}

trait PredefinedLengthUnit extends LengthPostfixOps[LengthUnit]{

  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = lengthUnit
}

object PredefinedLengthUnit extends PredefinedLengthUnit

trait LengthUnitInterpreter[A]
    extends LengthPostfixOps[Length[A]]
    with LengthPer[TimePostfixOps[Velocity[A]]  // for style like "1.0 m/s" ( = 1.0.m(/).s)
      with TimeSquaredPostfixOps[Acceleration[A]]]{

  // for style like "1.0 (m)" ( = "1.0.apply(m)")
  def apply(unit: LengthUnit): Length[A]

  override protected def lengthPostfixOps(lengthUnit: LengthUnit) = apply(lengthUnit)

  // Length -> Velocity
  // for style "1.0 m/s" ( = "1.0.m(/).s")
  override protected def lengthPer(lengthUnit: LengthUnit) =
    new TimePostfixOps[Velocity[A]] with TimeSquaredPostfixOps[Acceleration[A]]{
      override protected def timePostfixOps(timeUnit: TimeUnit) = apply(lengthUnit / timeUnit)
      override protected def timeSquaredPostfixOps(timeSquaredUnit: TimeSquaredUnit) =
        apply(lengthUnit / timeSquaredUnit)
    }

  def apply(velocityUnit: VelocityUnit): Velocity[A]
  def apply(accelerationUnit: AccelerationUnit): Acceleration[A]
}