package org.waman.multiverse.time

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._


sealed trait TimeUnit extends PhysicalUnit[TimeUnit]
  with MultiplicativeByTimeUnit[TimeSquaredUnit]
  with CanSquare[TimeSquaredUnit]{

  override def getSIUnit = org.waman.multiverse.time.TimeUnit.Second

  override def *(unit: TimeUnit) = TimeSquaredUnit(this, unit)

  override def square: TimeSquaredUnit = this * this
}

object TimeUnit extends ConstantsDefined[TimeUnit]{

  // intrinsic
  private[TimeUnit]
  class IntrinsicTimeUnit(val name: String, val symbols: Seq[String], val unitValueInSIUnit: Real)
      extends TimeUnit{

    def this(name: String, symbols: Seq[String], unit: TimeUnit) =
      this(name, symbols, unit.unitValueInSIUnit)

    def this(name: String, symbols: Seq[String], factor: Real, unit: TimeUnit) =
      this(name, symbols, factor * unit.unitValueInSIUnit)
  }


  case object YoctoSecond extends IntrinsicTimeUnit("YoctoSecond", Seq("ys"), r"1e-24")
  case object ZeptoSecond extends IntrinsicTimeUnit("ZeptoSecond", Seq("zs"), r"1e-21")
  case object AttoSecond extends IntrinsicTimeUnit("AttoSecond", Seq("as"), r"1e-18")
  case object FemtoSecond extends IntrinsicTimeUnit("FemtoSecond", Seq("fs"), r"1e-15")
  case object PicoSecond extends IntrinsicTimeUnit("PicoSecond", Seq("ps"), r"1e-12")
  case object NanoSecond extends IntrinsicTimeUnit("NanoSecond", Seq("ns"), r"1e-9")
  case object MicroSecond extends IntrinsicTimeUnit("MicroSecond", Seq("μs", "mcs"), r"1e-6")
  case object MilliSecond extends IntrinsicTimeUnit("MilliSecond", Seq("ms"), r"1e-3")
  case object CentiSecond extends IntrinsicTimeUnit("CentiSecond", Seq("cs"), r"1e-2")
  case object DeciSecond extends IntrinsicTimeUnit("DeciSecond", Seq("ds"), r"1e-1")
  case object Second extends IntrinsicTimeUnit("Second", Seq("s"), r"1")
  case object DecaSecond extends IntrinsicTimeUnit("DecaSecond", Seq("das"), r"1e1")
  case object HectoSecond extends IntrinsicTimeUnit("HectoSecond", Seq("hs"), r"1e2")
  case object KiloSecond extends IntrinsicTimeUnit("KiloSecond", Seq("ks"), r"1e3")
  case object MegaSecond extends IntrinsicTimeUnit("MegaSecond", Seq("Ms"), r"1e6")
  case object GigaSecond extends IntrinsicTimeUnit("GigaSecond", Seq("Gs"), r"1e9")
  case object TeraSecond extends IntrinsicTimeUnit("TeraSecond", Seq("Ts"), r"1e12")
  case object PetaSecond extends IntrinsicTimeUnit("PetaSecond", Seq("Ps"), r"1e15")
  case object ExaSecond extends IntrinsicTimeUnit("ExaSecond", Seq("Es"), r"1e18")
  case object ZettaSecond extends IntrinsicTimeUnit("ZettaSecond", Seq("Zs"), r"1e21")
  case object YottaSecond extends IntrinsicTimeUnit("YottaSecond", Seq("Ys"), r"1e24")
  case object Minute extends IntrinsicTimeUnit("Minute", Seq("minute"), 60)
  case object Hour extends IntrinsicTimeUnit("Hour", Seq("h"), 60, Minute)
  case object Day extends IntrinsicTimeUnit("Day", Seq("d"), 24, Hour)
  case object Week extends IntrinsicTimeUnit("Week", Seq("wk"), 7, Day)
  case object Month_Gregorian extends IntrinsicTimeUnit("Month_Gregorian", Seq("mo(Gregorian)"), 30.436875, Day)
  case object Year_Gregorian extends IntrinsicTimeUnit("Year_Gregorian", Seq("y", "yr", "a(Gregorian)", "y(Gregorian)", "yr(Gregorian)"), r"365.2425", Day)
  case object Svedberg extends IntrinsicTimeUnit("Svedberg", Seq("S"), r"1e-13")
  case object MilliDay extends IntrinsicTimeUnit("MilliDay", Seq("md"), r"24", Hour)
  case object Decade extends IntrinsicTimeUnit("Decade", Seq("dec", "dec(Gregorian)"), 10, Year_Gregorian)
  case object Century extends IntrinsicTimeUnit("Century", Seq("century", "c(Gregorian)"), 100, Year_Gregorian)
  case object Jitty extends IntrinsicTimeUnit("Jitty", Seq("j"), r"1/60", Second)
  case object JittyAlternative extends IntrinsicTimeUnit("JittyAlternative", Seq("ja"), 1, CentiSecond)
  case object Fortnight extends IntrinsicTimeUnit("Fortnight", Seq("fn"), 2, Week)
  case object Day_sidereal extends IntrinsicTimeUnit("Day_sidereal", Seq("d(sidereal)"), 23.9344699, Hour) with NotExact
  case object Year_sidereal extends IntrinsicTimeUnit("Year_sidereal", Seq("y(sidereal)", "yr(sidereal)"), r"365.256363", Day) with NotExact
  case object Decade_sidereal extends IntrinsicTimeUnit("Decade_sidereal", Seq("dec(sidereal)"), 10, Year_sidereal)
  case object Century_sidereal extends IntrinsicTimeUnit("Century_sidereal", Seq("c(sidereal)"), 100, Year_sidereal)
  case object Month_full extends IntrinsicTimeUnit("Month_full", Seq("mo(full)"), 30, Day)
  case object Month_hollow extends IntrinsicTimeUnit("Month_hollow", Seq("mo(hollow)"), 29, Day)
  case object Month_synodic extends IntrinsicTimeUnit("Month_synodic", Seq("mo(synodic)"), 29.530589, Day)
  case object Year_common extends IntrinsicTimeUnit("Year_common", Seq("a(common)", "y(common)", "yr(common)"), 365, Day)
  case object Year_leap extends IntrinsicTimeUnit("Year_leap", Seq("a(leap)", "y(leap)", "yr(leap)"), 366, Day)
  case object Year_Julian extends IntrinsicTimeUnit("Year_Julian", Seq("a(Julian)", "y(Julian)", "yr(Julian)"), r"365.25", Day)
  case object Year_tropical extends IntrinsicTimeUnit("Year_tropical", Seq("a(tropical)", "y(tropical)", "yr(tropical)"), r"365.256363", Day) with NotExact
  case object Decade_Julian extends IntrinsicTimeUnit("Decade_Julian", Seq("dec(Julian)"), 10, Year_Julian)
  case object Century_Julian extends IntrinsicTimeUnit("Century_Julian", Seq("c(Julian)"), 100, Year_Julian)
  case object Decade_tropical extends IntrinsicTimeUnit("Decade_tropical", Seq("dec(tropical)"), 10, Year_tropical)
  case object Century_tropical extends IntrinsicTimeUnit("Century_tropical", Seq("c(tropical)"), 100, Year_tropical)
  case object PlanckTime extends IntrinsicTimeUnit("PlanckTime", Seq("t_p"), r"5.3910632e-44") with NotExact

  override lazy val values = Seq(YoctoSecond, ZeptoSecond, AttoSecond, FemtoSecond, PicoSecond, NanoSecond, MicroSecond, MilliSecond, CentiSecond, DeciSecond, Second, DecaSecond, HectoSecond, KiloSecond, MegaSecond, GigaSecond, TeraSecond, PetaSecond, ExaSecond, ZettaSecond, YottaSecond, Minute, Hour, Day, Week, Month_Gregorian, Year_Gregorian, Svedberg, MilliDay, Decade, Century, Jitty, JittyAlternative, Fortnight, Day_sidereal, Year_sidereal, Decade_sidereal, Century_sidereal, Month_full, Month_hollow, Month_synodic, Year_common, Year_leap, Year_Julian, Year_tropical, Decade_Julian, Century_Julian, Decade_tropical, Century_tropical, PlanckTime)
}

trait MultiplicativeByTimeUnit[R]{
  def *(unit: TimeUnit): R
}

trait DivisibleByTimeUnit[R]{
  def /(unit: TimeUnit): R
}

trait TimePostfixOps[A]{
  import TimeUnit._

  protected def timePostfixOps(unit: TimeUnit): A


  def ys : A = timePostfixOps(YoctoSecond)
  def zs : A = timePostfixOps(ZeptoSecond)
  def as : A = timePostfixOps(AttoSecond)
  def fs : A = timePostfixOps(FemtoSecond)
  def ps : A = timePostfixOps(PicoSecond)
  def ns : A = timePostfixOps(NanoSecond)
  def μs : A = timePostfixOps(MicroSecond)
  def mcs : A = timePostfixOps(MicroSecond)
  def ms : A = timePostfixOps(MilliSecond)
  def cs : A = timePostfixOps(CentiSecond)
  def ds : A = timePostfixOps(DeciSecond)
  def s : A = timePostfixOps(Second)
  def das : A = timePostfixOps(DecaSecond)
  def hs : A = timePostfixOps(HectoSecond)
  def ks : A = timePostfixOps(KiloSecond)
  def Ms : A = timePostfixOps(MegaSecond)
  def Gs : A = timePostfixOps(GigaSecond)
  def Ts : A = timePostfixOps(TeraSecond)
  def Ps : A = timePostfixOps(PetaSecond)
  def Es : A = timePostfixOps(ExaSecond)
  def Zs : A = timePostfixOps(ZettaSecond)
  def Ys : A = timePostfixOps(YottaSecond)
  def minute : A = timePostfixOps(Minute)
  def h : A = timePostfixOps(Hour)
  def d : A = timePostfixOps(Day)
  def wk : A = timePostfixOps(Week)
  def y : A = timePostfixOps(Year_Gregorian)
  def yr : A = timePostfixOps(Year_Gregorian)
  def S : A = timePostfixOps(Svedberg)
  def md : A = timePostfixOps(MilliDay)
  def dec : A = timePostfixOps(Decade)
  def century : A = timePostfixOps(Century)
  def j : A = timePostfixOps(Jitty)
  def ja : A = timePostfixOps(JittyAlternative)
  def fn : A = timePostfixOps(Fortnight)
  def t_p : A = timePostfixOps(PlanckTime)

  import TimePostfixOps._
  import org.waman.multiverse.time.TimeContext
  import TimeContext._

  def mo(c: TimeContext): A = timePostfixOps(_mo(c))
  def a(c: TimeContext): A = timePostfixOps(_a(c))
  def y(c: TimeContext): A = timePostfixOps(_y(c))
  def yr(c: TimeContext): A = timePostfixOps(_yr(c))
  def dec(c: TimeContext): A = timePostfixOps(_dec(c))
  def c(c: TimeContext): A = timePostfixOps(_c(c))
  def d(c: TimeContext): A = timePostfixOps(_d(c))
}

object TimePostfixOps{
  import TimeUnit._
  import org.waman.multiverse.time.TimeContext
  import TimeContext._


  lazy val _y : PartialFunction[TimeContext, TimeUnit] = {
    case GregorianCalendar => Year_Gregorian
    case Sidereal => Year_sidereal
    case CommonYear => Year_common
    case LeapYear => Year_leap
    case JulianCalendar => Year_Julian
    case Tropical => Year_tropical
  }

  lazy val _a : PartialFunction[TimeContext, TimeUnit] = {
    case GregorianCalendar => Year_Gregorian
    case CommonYear => Year_common
    case LeapYear => Year_leap
    case JulianCalendar => Year_Julian
    case Tropical => Year_tropical
  }

  lazy val _mo : PartialFunction[TimeContext, TimeUnit] = {
    case GregorianCalendar => Month_Gregorian
    case FullMonth => Month_full
    case HollowMonth => Month_hollow
    case Synodic => Month_synodic
  }

  lazy val _c : PartialFunction[TimeContext, TimeUnit] = {
    case GregorianCalendar => Century
    case Sidereal => Century_sidereal
    case JulianCalendar => Century_Julian
    case Tropical => Century_tropical
  }

  lazy val _dec : PartialFunction[TimeContext, TimeUnit] = {
    case GregorianCalendar => Decade
    case Sidereal => Decade_sidereal
    case JulianCalendar => Decade_Julian
    case Tropical => Decade_tropical
  }

  lazy val _yr : PartialFunction[TimeContext, TimeUnit] = {
    case GregorianCalendar => Year_Gregorian
    case Sidereal => Year_sidereal
    case CommonYear => Year_common
    case LeapYear => Year_leap
    case JulianCalendar => Year_Julian
    case Tropical => Year_tropical
  }

  lazy val _d : PartialFunction[TimeContext, TimeUnit] = {
    case Sidereal => Day_sidereal
  }
}

trait TimeDot[A]{
  import TimeUnit._

  protected def timeDot(unit: TimeUnit): A

  def ys(dot: Dot): A = timeDot(YoctoSecond)
  def zs(dot: Dot): A = timeDot(ZeptoSecond)
  def as(dot: Dot): A = timeDot(AttoSecond)
  def fs(dot: Dot): A = timeDot(FemtoSecond)
  def ps(dot: Dot): A = timeDot(PicoSecond)
  def ns(dot: Dot): A = timeDot(NanoSecond)
  def μs(dot: Dot): A = timeDot(MicroSecond)
  def mcs(dot: Dot): A = timeDot(MicroSecond)
  def ms(dot: Dot): A = timeDot(MilliSecond)
  def cs(dot: Dot): A = timeDot(CentiSecond)
  def ds(dot: Dot): A = timeDot(DeciSecond)
  def s(dot: Dot): A = timeDot(Second)
  def das(dot: Dot): A = timeDot(DecaSecond)
  def hs(dot: Dot): A = timeDot(HectoSecond)
  def ks(dot: Dot): A = timeDot(KiloSecond)
  def Ms(dot: Dot): A = timeDot(MegaSecond)
  def Gs(dot: Dot): A = timeDot(GigaSecond)
  def Ts(dot: Dot): A = timeDot(TeraSecond)
  def Ps(dot: Dot): A = timeDot(PetaSecond)
  def Es(dot: Dot): A = timeDot(ExaSecond)
  def Zs(dot: Dot): A = timeDot(ZettaSecond)
  def Ys(dot: Dot): A = timeDot(YottaSecond)
  def minute(dot: Dot): A = timeDot(Minute)
  def h(dot: Dot): A = timeDot(Hour)
  def d(dot: Dot): A = timeDot(Day)
  def wk(dot: Dot): A = timeDot(Week)
  def y(dot: Dot): A = timeDot(Year_Gregorian)
  def yr(dot: Dot): A = timeDot(Year_Gregorian)
  def S(dot: Dot): A = timeDot(Svedberg)
  def md(dot: Dot): A = timeDot(MilliDay)
  def dec(dot: Dot): A = timeDot(Decade)
  def century(dot: Dot): A = timeDot(Century)
  def j(dot: Dot): A = timeDot(Jitty)
  def ja(dot: Dot): A = timeDot(JittyAlternative)
  def fn(dot: Dot): A = timeDot(Fortnight)
  def t_p(dot: Dot): A = timeDot(PlanckTime)
}

trait TimePer[A]{
  import TimeUnit._

  protected def timePer(unit: TimeUnit): A

  def ys(per: Per): A = timePer(YoctoSecond)
  def zs(per: Per): A = timePer(ZeptoSecond)
  def as(per: Per): A = timePer(AttoSecond)
  def fs(per: Per): A = timePer(FemtoSecond)
  def ps(per: Per): A = timePer(PicoSecond)
  def ns(per: Per): A = timePer(NanoSecond)
  def μs(per: Per): A = timePer(MicroSecond)
  def mcs(per: Per): A = timePer(MicroSecond)
  def ms(per: Per): A = timePer(MilliSecond)
  def cs(per: Per): A = timePer(CentiSecond)
  def ds(per: Per): A = timePer(DeciSecond)
  def s(per: Per): A = timePer(Second)
  def das(per: Per): A = timePer(DecaSecond)
  def hs(per: Per): A = timePer(HectoSecond)
  def ks(per: Per): A = timePer(KiloSecond)
  def Ms(per: Per): A = timePer(MegaSecond)
  def Gs(per: Per): A = timePer(GigaSecond)
  def Ts(per: Per): A = timePer(TeraSecond)
  def Ps(per: Per): A = timePer(PetaSecond)
  def Es(per: Per): A = timePer(ExaSecond)
  def Zs(per: Per): A = timePer(ZettaSecond)
  def Ys(per: Per): A = timePer(YottaSecond)
  def minute(per: Per): A = timePer(Minute)
  def h(per: Per): A = timePer(Hour)
  def d(per: Per): A = timePer(Day)
  def wk(per: Per): A = timePer(Week)
  def y(per: Per): A = timePer(Year_Gregorian)
  def yr(per: Per): A = timePer(Year_Gregorian)
  def S(per: Per): A = timePer(Svedberg)
  def md(per: Per): A = timePer(MilliDay)
  def dec(per: Per): A = timePer(Decade)
  def century(per: Per): A = timePer(Century)
  def j(per: Per): A = timePer(Jitty)
  def ja(per: Per): A = timePer(JittyAlternative)
  def fn(per: Per): A = timePer(Fortnight)
  def t_p(per: Per): A = timePer(PlanckTime)
}

trait PredefinedTimeUnit extends TimePostfixOps[TimeUnit]{
  override protected def timePostfixOps(unit: TimeUnit) = unit
  
}

object PredefinedTimeUnit extends PredefinedTimeUnit
