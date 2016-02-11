package org.waman.multiverse

import org.waman.multiverse.Context._
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

  protected def lengthPostfixOps(lengthUnit: LengthUnit): A

  def ym: A = lengthPostfixOps(LengthUnit.YoctoMetre)
  def zm: A = lengthPostfixOps(LengthUnit.ZeptoMetre)
  def am: A = lengthPostfixOps(LengthUnit.AttoMetre)
  def fm: A = lengthPostfixOps(LengthUnit.FemtoMetre)
  def pm: A = lengthPostfixOps(LengthUnit.PicoMetre)
  def nm: A = lengthPostfixOps(LengthUnit.NanoMetre)
  def μm: A = lengthPostfixOps(LengthUnit.MicroMetre)
  def mm: A = lengthPostfixOps(LengthUnit.MilliMetre)
  def cm: A = lengthPostfixOps(LengthUnit.CentiMetre)
  def dm: A = lengthPostfixOps(LengthUnit.DeciMetre)
  def m : A = lengthPostfixOps(LengthUnit.Metre)
  def dam: A = lengthPostfixOps(LengthUnit.DecaMetre)
  def hm: A = lengthPostfixOps(LengthUnit.HectoMetre)
  def km: A = lengthPostfixOps(LengthUnit.KiloMetre)
  def Mm: A = lengthPostfixOps(LengthUnit.MegaMetre)
  def Gm: A = lengthPostfixOps(LengthUnit.GigaMetre)
  def Tm: A = lengthPostfixOps(LengthUnit.TeraMetre)
  def Pm: A = lengthPostfixOps(LengthUnit.PetaMetre)
  def Em: A = lengthPostfixOps(LengthUnit.ExaMetre)
  def Zm: A = lengthPostfixOps(LengthUnit.ZettaMetre)
  def Ym: A = lengthPostfixOps(LengthUnit.YottaMetre)

  // microscopic
  def μ: A = lengthPostfixOps(LengthUnit.Micron)
  def Å: A = lengthPostfixOps(LengthUnit.Angstrom)
  def a0: A = lengthPostfixOps(LengthUnit.AtomicUnitOfLength)
  def xu: A = lengthPostfixOps(LengthUnit.XUnit)
  def xu(c: Context): A = _xu(c)
  lazy val _xu: PartialFunction[Context, A] = {
    case Cu_KAlpha1 => lengthPostfixOps(LengthUnit.XUnit_Cu_KAlpha1)
    case Mo_KAlpha1 => lengthPostfixOps(LengthUnit.XUnit_Mo_KAlpha1)
  }
  def lp: A = lengthPostfixOps(LengthUnit.PlanckLength)

  // astronomy
  def au: A = lengthPostfixOps(LengthUnit.AstronomicalUnit)
  def ly: A = lengthPostfixOps(LengthUnit.LightYear)
  def pc: A = lengthPostfixOps(LengthUnit.Parsec)

  // yard-pond
  def mil : A = lengthPostfixOps(LengthUnit.Mil)
  def thou: A = mil
  def twp : A = lengthPostfixOps(LengthUnit.Twip)
  def pt  : A = lengthPostfixOps(LengthUnit.Point)
  def ln  : A = lengthPostfixOps(LengthUnit.Line)
  def in  : A = lengthPostfixOps(LengthUnit.Inch)
  def lnk : A = lengthPostfixOps(LengthUnit.Link)
  def ft  : A = lengthPostfixOps(LengthUnit.Foot)
  def yd  : A = lengthPostfixOps(LengthUnit.Yard)
  def ell : A = lengthPostfixOps(LengthUnit.Ell)
  def ftm : A = lengthPostfixOps(LengthUnit.Fathom)
  def rd  : A = lengthPostfixOps(LengthUnit.Rod)
  def rope: A = lengthPostfixOps(LengthUnit.Rope)
  def ch  : A = lengthPostfixOps(LengthUnit.Chain)
  def mi  : A = lengthPostfixOps(LengthUnit.Mile)
  def lea : A = lengthPostfixOps(LengthUnit.League)

  def nmi: A = lengthPostfixOps(LengthUnit.NauticalMile)
  def nmi(c: Context): A = _nmi(c)
  lazy val _nmi: PartialFunction[Context, A] = {
    case Admiralty => lengthPostfixOps(LengthUnit.NauticalMile_Admiralty)
  }
  def NM: A = nmi
  def NM(c: Context): A = _NM(c)
  lazy val _NM: PartialFunction[Context, A] = _nmi
  def nl: A = lengthPostfixOps(LengthUnit.NauticalLeague)
  def NL: A = nl

  def lnk(c: Context) = _lnk(c)
  lazy val _lnk: PartialFunction[Context, A] = {
    case UnitedStates => lengthPostfixOps(LengthUnit.Link_US_Survey)
  }
  def ft(c: Context) = _ft(c)
  lazy val _ft: PartialFunction[Context, A] = {
    case UnitedStates => lengthPostfixOps(LengthUnit.Foot_US_Survey)
  }
  def mi(c: Context) = _mi(c)
  lazy val _mi: PartialFunction[Context, A] = {
    case UnitedStates => lengthPostfixOps(LengthUnit.Mile_US_Survey)
  }

  def mf : A = lengthPostfixOps(LengthUnit.MetricFoot)
  def smf: A = lengthPostfixOps(LengthUnit.ShortMetricFoot)
  def lmf: A = lengthPostfixOps(LengthUnit.LongMetricFoot)

  def Fr : A = lengthPostfixOps(LengthUnit.French)
  def fur: A = lengthPostfixOps(LengthUnit.Furlong)
}

trait LengthPer[A]{

  protected def lengthPer(lengthUnit: LengthUnit): A

  def ym(per: Per): A = lengthPer(LengthUnit.YoctoMetre)
  def zm(per: Per): A = lengthPer(LengthUnit.ZeptoMetre)
  def am(per: Per): A = lengthPer(LengthUnit.AttoMetre)
  def fm(per: Per): A = lengthPer(LengthUnit.FemtoMetre)
  def pm(per: Per): A = lengthPer(LengthUnit.PicoMetre)
  def nm(per: Per): A = lengthPer(LengthUnit.NanoMetre)
  def μm(per: Per): A = lengthPer(LengthUnit.MicroMetre)
  def mm(per: Per): A = lengthPer(LengthUnit.MilliMetre)
  def cm(per: Per): A = lengthPer(LengthUnit.CentiMetre)
  def dm(per: Per): A = lengthPer(LengthUnit.DeciMetre)
  def m (per: Per): A = lengthPer(LengthUnit.Metre)
  def dam(per: Per):A = lengthPer(LengthUnit.DecaMetre)
  def hm(per: Per): A = lengthPer(LengthUnit.HectoMetre)
  def km(per: Per): A = lengthPer(LengthUnit.KiloMetre)
  def Mm(per: Per): A = lengthPer(LengthUnit.MegaMetre)
  def Gm(per: Per): A = lengthPer(LengthUnit.GigaMetre)
  def Tm(per: Per): A = lengthPer(LengthUnit.TeraMetre)
  def Pm(per: Per): A = lengthPer(LengthUnit.PetaMetre)
  def Em(per: Per): A = lengthPer(LengthUnit.ExaMetre)
  def Zm(per: Per): A = lengthPer(LengthUnit.ZettaMetre)
  def Ym(per: Per): A = lengthPer(LengthUnit.YottaMetre)

  // microscopic
  def μ(per: Per): A = lengthPer(LengthUnit.Micron)
  def Å(per: Per): A = lengthPer(LengthUnit.Angstrom)
  def a0(per: Per): A = lengthPer(LengthUnit.AtomicUnitOfLength)
  def xu(per: Per): A = lengthPer(LengthUnit.XUnit)
  def lp(per: Per): A = lengthPer(LengthUnit.PlanckLength)

  // astronomy
  def au(per: Per): A = lengthPer(LengthUnit.AstronomicalUnit)
  def ly(per: Per): A = lengthPer(LengthUnit.LightYear)
  def pc(per: Per): A = lengthPer(LengthUnit.Parsec)

  // yard-pond
  def mil (per: Per): A = lengthPer(LengthUnit.Mil)
  def thou(per: Per): A = mil(per)
  def twp (per: Per): A = lengthPer(LengthUnit.Twip)
  def pt  (per: Per): A = lengthPer(LengthUnit.Point)
  def ln  (per: Per): A = lengthPer(LengthUnit.Line)
  def in  (per: Per): A = lengthPer(LengthUnit.Inch)
  def lnk (per: Per): A = lengthPer(LengthUnit.Link)
  def ft  (per: Per): A = lengthPer(LengthUnit.Foot)
  def yd  (per: Per): A = lengthPer(LengthUnit.Yard)
  def ell (per: Per): A = lengthPer(LengthUnit.Ell)
  def ftm (per: Per): A = lengthPer(LengthUnit.Fathom)
  def rd  (per: Per): A = lengthPer(LengthUnit.Rod)
  def rope(per: Per): A = lengthPer(LengthUnit.Rope)
  def ch  (per: Per): A = lengthPer(LengthUnit.Chain)
  def mi  (per: Per): A = lengthPer(LengthUnit.Mile)
  def lea (per: Per): A = lengthPer(LengthUnit.League)

  def nmi(per: Per): A = lengthPer(LengthUnit.NauticalMile)
  def NM(per: Per): A = nmi(per)
  def nl(per: Per): A = lengthPer(LengthUnit.NauticalLeague)
  def NL(per: Per): A = nl(per)

  def mf (per: Per): A = lengthPer(LengthUnit.MetricFoot)
  def smf(per: Per): A = lengthPer(LengthUnit.ShortMetricFoot)
  def lmf(per: Per): A = lengthPer(LengthUnit.LongMetricFoot)

  def Fr (per: Per): A = lengthPer(LengthUnit.French)
  def fur(per: Per): A = lengthPer(LengthUnit.Furlong)
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
    extends PhysicalUnit
    with DivisibleByTime[VelocityUnit] // for style like "1.0 (m/s)" ( = "1.0.apply(m./(s))")
    with DivisibleByTimeSquared[AccelerationUnit]{

  def this(symbol: String, factor: Real, lengthUnit: LengthUnit) =
    this(symbol, factor * lengthUnit.unitInMetre)

  override protected val baseUnit = LengthUnit.Metre
  override protected val inBaseUnitAccessor = () => unitInMetre

  override def /(timeUnit: TimeUnit) = VelocityUnit(this, timeUnit)
  override def /(timeSquaredUnit: TimeSquaredUnit) = AccelerationUnit(this, timeSquaredUnit)
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
  case object AtomicUnitOfLength extends LengthUnit("a0", r"5.2917721092e-11") with NotExact
  case object XUnit              extends LengthUnit("xu", r"1.0021e-13") with NotExact
  case object XUnit_Cu_KAlpha1   extends LengthUnit("xu(CuKα1)", r"1.0020769928e-13") with NotExact
  case object XUnit_Mo_KAlpha1   extends LengthUnit("xu(MoKα1)", r"1.0020995553e-13") with NotExact
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
  case object Link   extends LengthUnit("lnk", r"0.66", Foot)
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