package org.waman.multiverse.time

import spire.math.Real
import spire.implicits._
import org.waman.multiverse._


sealed trait TimeUnit extends PhysicalUnit[TimeUnit]
  with MultiplicativeByTimeUnit[TimeSquaredUnit]
  with CanSquare[TimeSquaredUnit]{

  def unitInSecond: Real

  override def baseUnit = org.waman.multiverse.time.TimeUnit.Second
  override def valueInBaseUnit = unitInSecond

  override def *(unit: TimeUnit) = TimeSquaredUnit(this, unit)

  override def square: TimeSquaredUnit = this * this
}

object TimeUnit extends ConstantsDefined[TimeUnit]{

  // intrinsic
  private[TimeUnit]
  class IntrinsicTimeUnit(name: String, val symbols: Seq[String], val unitInSecond: Real)
      extends TimeUnit{

    def this(name: String, symbols: Seq[String], unit: TimeUnit) =
      this(name, symbols, unit.unitInSecond)

    def this(name: String, symbols: Seq[String], factor: Real, unit: TimeUnit) =
      this(name, symbols, factor * unit.unitInSecond)
  }


  case object YoctoSecond extends IntrinsicTimeUnit("YoctoSecond", Seq("ys"), r"1e-24")
  case object ZeptoSecond extends IntrinsicTimeUnit("ZeptoSecond", Seq("zs"), r"1e-21")
  case object AttoSecond extends IntrinsicTimeUnit("AttoSecond", Seq("as"), r"1e-18")
  case object FemtoSecond extends IntrinsicTimeUnit("FemtoSecond", Seq("fs"), r"1e-15")
  case object PicoSecond extends IntrinsicTimeUnit("PicoSecond", Seq("ps"), r"1e-12")
  case object NanoSecond extends IntrinsicTimeUnit("NanoSecond", Seq("ns"), r"1e-9")
  case object MicroSecond extends IntrinsicTimeUnit("MicroSecond", Seq("microSecond", "μs"), r"1e-6")
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
  case object Year_Gregorian extends IntrinsicTimeUnit("Year_Gregorian", Seq(), r"365.2425", Day)
  case object Svedberg extends IntrinsicTimeUnit("Svedberg", Seq("S"), r"1e-13")
  case object MilliDay extends IntrinsicTimeUnit("MilliDay", Seq("md"), r"24", Hour)
  case object Decade extends IntrinsicTimeUnit("Decade", Seq("dec"), 10, Year_Gregorian)
  case object Century extends IntrinsicTimeUnit("Century", Seq("century"), 100, Year_Gregorian)
  case object JittyAlternative extends IntrinsicTimeUnit("JittyAlternative", Seq("ja"), 1, CentiSecond)
  case object Jitty extends IntrinsicTimeUnit("Jitty", Seq("j"), r"1/60", Second)
  case object Fortnight extends IntrinsicTimeUnit("Fortnight", Seq("fn"), 2, Week)
  case object Day_sidereal extends IntrinsicTimeUnit("Day_sidereal", Seq("d(sidereal)"), 23.9344699, Hour) with NotExact
  case object Year_sidereal extends IntrinsicTimeUnit("Year_sidereal", Seq(), r"365.256363", Day) with NotExact
  case object Year_common extends IntrinsicTimeUnit("Year_common", Seq(), 365, Day)
  case object Year_leap extends IntrinsicTimeUnit("Year_leap", Seq(), 366, Day)
  case object Year_Julian extends IntrinsicTimeUnit("Year_Julian", Seq(), r"365.25", Day)
  case object Year_tropical extends IntrinsicTimeUnit("Year_tropical", Seq(), r"365.256363", Day) with NotExact

  override lazy val values = Seq(YoctoSecond, ZeptoSecond, AttoSecond, FemtoSecond, PicoSecond, NanoSecond, MicroSecond, MilliSecond, CentiSecond, DeciSecond, Second, DecaSecond, HectoSecond, KiloSecond, MegaSecond, GigaSecond, TeraSecond, PetaSecond, ExaSecond, ZettaSecond, YottaSecond, Minute, Hour, Day, Week, Year_Gregorian, Svedberg, MilliDay, Decade, Century, JittyAlternative, Jitty, Fortnight, Day_sidereal, Year_sidereal, Year_common, Year_leap, Year_Julian, Year_tropical)
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
  def microSecond : A = timePostfixOps(MicroSecond)
  def μs : A = timePostfixOps(MicroSecond)
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
  def S : A = timePostfixOps(Svedberg)
  def md : A = timePostfixOps(MilliDay)
  def dec : A = timePostfixOps(Decade)
  def century : A = timePostfixOps(Century)
  def ja : A = timePostfixOps(JittyAlternative)
  def j : A = timePostfixOps(Jitty)
  def fn : A = timePostfixOps(Fortnight)

  import TimePostfixOps._
  import org.waman.multiverse.time.TimeContext
  import TimeContext._

  def d(c: TimeContext): A = timePostfixOps(_d(c))
}

object TimePostfixOps{
  import TimeUnit._
  import org.waman.multiverse.time.TimeContext
  import TimeContext._


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
  def microSecond(dot: Dot): A = timeDot(MicroSecond)
  def μs(dot: Dot): A = timeDot(MicroSecond)
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
  def S(dot: Dot): A = timeDot(Svedberg)
  def md(dot: Dot): A = timeDot(MilliDay)
  def dec(dot: Dot): A = timeDot(Decade)
  def century(dot: Dot): A = timeDot(Century)
  def ja(dot: Dot): A = timeDot(JittyAlternative)
  def j(dot: Dot): A = timeDot(Jitty)
  def fn(dot: Dot): A = timeDot(Fortnight)
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
  def microSecond(per: Per): A = timePer(MicroSecond)
  def μs(per: Per): A = timePer(MicroSecond)
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
  def S(per: Per): A = timePer(Svedberg)
  def md(per: Per): A = timePer(MilliDay)
  def dec(per: Per): A = timePer(Decade)
  def century(per: Per): A = timePer(Century)
  def ja(per: Per): A = timePer(JittyAlternative)
  def j(per: Per): A = timePer(Jitty)
  def fn(per: Per): A = timePer(Fortnight)
}

trait PredefinedTimeUnit extends TimePostfixOps[TimeUnit]{
  override protected def timePostfixOps(unit: TimeUnit) = unit
  
}

object PredefinedTimeUnit extends PredefinedTimeUnit
