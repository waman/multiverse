package org.waman.multiverse.predef.basic

import spire.math.Real
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.predef._
import org.waman.multiverse.units.basic.TimeUnit

class DefaultTimeUnit(val name: String, val symbol: String, val aliases: Seq[String], val intervalInSIUnit: Real) extends TimeUnit

sealed trait dayAttribute
sealed trait monthAttribute
sealed trait yearAttribute
sealed trait decadeAttribute
sealed trait centuryAttribute

object TimeAttributes{
  final object topical extends centuryAttribute
  final object sidereal extends dayAttribute with yearAttribute with decadeAttribute with centuryAttribute
  final object gregorian extends monthAttribute with yearAttribute with decadeAttribute with centuryAttribute
  final object tropical extends yearAttribute with decadeAttribute
  final object full extends monthAttribute
  final object common extends yearAttribute
  final object leap extends yearAttribute
  final object hollow extends monthAttribute
  final object julian extends yearAttribute with decadeAttribute with centuryAttribute
  final object synodic extends monthAttribute
}

object TimeUnitObjects{
  import TimeAttributes._

  final object second extends DefaultTimeUnit("second", "s", Seq("sec"), r"1")
  final object yoctosecond extends DefaultTimeUnit("yoctosecond", "ys", Seq("ysec"), r"1" * r"1e-24")
  final object zeptosecond extends DefaultTimeUnit("zeptosecond", "zs", Seq("zsec"), r"1" * r"1e-21")
  final object attosecond extends DefaultTimeUnit("attosecond", "as", Seq("asec"), r"1" * r"1e-18")
  final object femtosecond extends DefaultTimeUnit("femtosecond", "fs", Seq("fsec"), r"1" * r"1e-15")
  final object picosecond extends DefaultTimeUnit("picosecond", "ps", Seq("psec"), r"1" * r"1e-12")
  final object nanosecond extends DefaultTimeUnit("nanosecond", "ns", Seq("nsec"), r"1" * r"1e-9")
  final object microsecond extends DefaultTimeUnit("microsecond", "μs", Seq("μsec", "mcs", "mcsec"), r"1" * r"1e-6")
  final object millisecond extends DefaultTimeUnit("millisecond", "ms", Seq("msec"), r"1" * r"1e-3")
  final object centisecond extends DefaultTimeUnit("centisecond", "cs", Seq("csec"), r"1" * r"1e-2")
  final object decisecond extends DefaultTimeUnit("decisecond", "ds", Seq("dsec"), r"1" * r"1e-1")
  final object decasecond extends DefaultTimeUnit("decasecond", "das", Seq("dasec"), r"1" * r"1e1")
  final object hectosecond extends DefaultTimeUnit("hectosecond", "hs", Seq("hsec"), r"1" * r"1e2")
  final object kilosecond extends DefaultTimeUnit("kilosecond", "ks", Seq("ksec"), r"1" * r"1e3")
  final object megasecond extends DefaultTimeUnit("megasecond", "Ms", Seq("Msec"), r"1" * r"1e6")
  final object gigasecond extends DefaultTimeUnit("gigasecond", "Gs", Seq("Gsec"), r"1" * r"1e9")
  final object terasecond extends DefaultTimeUnit("terasecond", "Ts", Seq("Tsec"), r"1" * r"1e12")
  final object petasecond extends DefaultTimeUnit("petasecond", "Ps", Seq("Psec"), r"1" * r"1e15")
  final object exasecond extends DefaultTimeUnit("exasecond", "Es", Seq("Esec"), r"1" * r"1e18")
  final object zettasecond extends DefaultTimeUnit("zettasecond", "Zs", Seq("Zsec"), r"1" * r"1e21")
  final object yottasecond extends DefaultTimeUnit("yottasecond", "Ys", Seq("Ysec"), r"1" * r"1e24")
  final object minute extends DefaultTimeUnit("minute", "min", Nil, r"60")
  final object hour extends DefaultTimeUnit("hour", "h", Nil, r"60" * minute.intervalInSIUnit)
  final object day extends DefaultTimeUnit("day", "d", Nil, r"24" * hour.intervalInSIUnit){
    def apply(a: dayAttribute): TimeUnit = a match {
      case sidereal => `day(sidereal)`
    }
  }
  final object `day(sidereal)` extends DefaultTimeUnit("day(sidereal)", "d(sidereal)", Nil, r"23.9344699" * hour.intervalInSIUnit) with NotExact
  final object week extends DefaultTimeUnit("week", "wk", Nil, r"7" * day.intervalInSIUnit)
  final object month extends DefaultTimeUnit("month", "mo", Nil, r"30.436875" * day.intervalInSIUnit){
    def apply(a: monthAttribute): TimeUnit = a match {
      case gregorian => `month(gregorian)`
      case full => `month(full)`
      case hollow => `month(hollow)`
      case synodic => `month(synodic)`
    }
  }
  final object `month(gregorian)` extends DefaultTimeUnit("month(gregorian)", "mo(gregorian)", Nil, r"30.436875" * day.intervalInSIUnit)
  final object `month(full)` extends DefaultTimeUnit("month(full)", "mo(full)", Nil, r"30" * day.intervalInSIUnit)
  final object `month(hollow)` extends DefaultTimeUnit("month(hollow)", "mo(hollow)", Nil, r"29" * day.intervalInSIUnit)
  final object `month(synodic)` extends DefaultTimeUnit("month(synodic)", "mo(synodic)", Nil, r"29.530589" * day.intervalInSIUnit)
  final object year extends DefaultTimeUnit("year", "yr", Seq("y", "a"), r"365.2425" * day.intervalInSIUnit){
    def apply(a: yearAttribute): TimeUnit = a match {
      case common => `year(common)`
      case leap => `year(leap)`
      case gregorian => `year(gregorian)`
      case sidereal => `year(sidereal)`
      case julian => `year(julian)`
      case tropical => `year(tropical)`
    }
  }
  final object `year(common)` extends DefaultTimeUnit("year(common)", "yr(common)", Seq("y(common)", "a(common)"), r"365" * day.intervalInSIUnit)
  final object `year(leap)` extends DefaultTimeUnit("year(leap)", "yr(leap)", Seq("y(leap)", "a(leap)"), r"366" * day.intervalInSIUnit)
  final object `year(gregorian)` extends DefaultTimeUnit("year(gregorian)", "yr(gregorian)", Seq("y(gregorian)", "a(gregorian)"), r"365.2425" * day.intervalInSIUnit)
  final object `year(sidereal)` extends DefaultTimeUnit("year(sidereal)", "yr(sidereal)", Seq("y(sidereal)", "a(sidereal)"), r"365.256363" * day.intervalInSIUnit) with NotExact
  final object `year(julian)` extends DefaultTimeUnit("year(julian)", "yr(julian)", Seq("y(julian)", "a(julian)"), r"365.25" * day.intervalInSIUnit)
  final object `year(tropical)` extends DefaultTimeUnit("year(tropical)", "yr(tropical)", Seq("y(tropical)", "a(tropical)"), r"365.256363" * day.intervalInSIUnit) with NotExact
  final object decade extends DefaultTimeUnit("decade", "dec", Nil, r"10" * year(gregorian).intervalInSIUnit){
    def apply(a: decadeAttribute): TimeUnit = a match {
      case gregorian => `decade(gregorian)`
      case sidereal => `decade(sidereal)`
      case julian => `decade(julian)`
      case tropical => `decade(tropical)`
    }
  }
  final object `decade(gregorian)` extends DefaultTimeUnit("decade(gregorian)", "dec(gregorian)", Nil, r"10" * year(gregorian).intervalInSIUnit)
  final object `decade(sidereal)` extends DefaultTimeUnit("decade(sidereal)", "dec(sidereal)", Nil, r"10" * year(sidereal).intervalInSIUnit)
  final object `decade(julian)` extends DefaultTimeUnit("decade(julian)", "dec(julian)", Nil, r"10" * year(julian).intervalInSIUnit)
  final object `decade(tropical)` extends DefaultTimeUnit("decade(tropical)", "dec(tropical)", Nil, r"10" * year(tropical).intervalInSIUnit)
  final object century extends DefaultTimeUnit("century", "century", Nil, r"100" * year(gregorian).intervalInSIUnit){
    def apply(a: centuryAttribute): TimeUnit = a match {
      case gregorian => `century(gregorian)`
      case sidereal => `century(sidereal)`
      case julian => `century(julian)`
      case topical => `century(topical)`
    }
  }
  final object `century(gregorian)` extends DefaultTimeUnit("century(gregorian)", "century(gregorian)", Nil, r"100" * year(gregorian).intervalInSIUnit)
  final object `century(sidereal)` extends DefaultTimeUnit("century(sidereal)", "century(sidereal)", Nil, r"100" * year(sidereal).intervalInSIUnit)
  final object `century(julian)` extends DefaultTimeUnit("century(julian)", "century(julian)", Nil, r"100" * year(julian).intervalInSIUnit)
  final object `century(topical)` extends DefaultTimeUnit("century(topical)", "century(topical)", Nil, r"100" * year(tropical).intervalInSIUnit)
  final object svedberg extends DefaultTimeUnit("svedberg", "S", Nil, r"1e-13")
  final object milliday extends DefaultTimeUnit("milliday", "md", Nil, r"0.001" * day.intervalInSIUnit)
  final object jitty extends DefaultTimeUnit("jitty", "j", Nil, r"1/60")
  final object jitty_alternative extends DefaultTimeUnit("jitty_alternative", "ja", Nil, r"1" * centisecond.intervalInSIUnit)
  final object fortnight extends DefaultTimeUnit("fortnight", "fn", Nil, r"2" * week.intervalInSIUnit)
  final object planck_time extends DefaultTimeUnit("planck time", "t_p", Nil, r"5.3910632e-44") with NotExact

  def getUnits: Seq[TimeUnit] = 
    Seq(second, yoctosecond, zeptosecond, attosecond, femtosecond, picosecond, nanosecond, microsecond, millisecond, centisecond, decisecond, decasecond, hectosecond, kilosecond, megasecond, gigasecond, terasecond, petasecond, exasecond, zettasecond, yottasecond, minute, hour, day, `day(sidereal)`, week, month, `month(gregorian)`, `month(full)`, `month(hollow)`, `month(synodic)`, year, `year(common)`, `year(leap)`, `year(gregorian)`, `year(sidereal)`, `year(julian)`, `year(tropical)`, decade, `decade(gregorian)`, `decade(sidereal)`, `decade(julian)`, `decade(tropical)`, century, `century(gregorian)`, `century(sidereal)`, `century(julian)`, `century(topical)`, svedberg, milliday, jitty, jitty_alternative, fortnight, planck_time)
}

object TimeUnits{
  def s: TimeUnit = TimeUnitObjects.second
  def sec: TimeUnit = TimeUnitObjects.second
  def ys: TimeUnit = TimeUnitObjects.yoctosecond
  def ysec: TimeUnit = TimeUnitObjects.yoctosecond
  def zs: TimeUnit = TimeUnitObjects.zeptosecond
  def zsec: TimeUnit = TimeUnitObjects.zeptosecond
  def as: TimeUnit = TimeUnitObjects.attosecond
  def asec: TimeUnit = TimeUnitObjects.attosecond
  def fs: TimeUnit = TimeUnitObjects.femtosecond
  def fsec: TimeUnit = TimeUnitObjects.femtosecond
  def ps: TimeUnit = TimeUnitObjects.picosecond
  def psec: TimeUnit = TimeUnitObjects.picosecond
  def ns: TimeUnit = TimeUnitObjects.nanosecond
  def nsec: TimeUnit = TimeUnitObjects.nanosecond
  def μs: TimeUnit = TimeUnitObjects.microsecond
  def μsec: TimeUnit = TimeUnitObjects.microsecond
  def mcs: TimeUnit = TimeUnitObjects.microsecond
  def mcsec: TimeUnit = TimeUnitObjects.microsecond
  def ms: TimeUnit = TimeUnitObjects.millisecond
  def msec: TimeUnit = TimeUnitObjects.millisecond
  def cs: TimeUnit = TimeUnitObjects.centisecond
  def csec: TimeUnit = TimeUnitObjects.centisecond
  def ds: TimeUnit = TimeUnitObjects.decisecond
  def dsec: TimeUnit = TimeUnitObjects.decisecond
  def das: TimeUnit = TimeUnitObjects.decasecond
  def dasec: TimeUnit = TimeUnitObjects.decasecond
  def hs: TimeUnit = TimeUnitObjects.hectosecond
  def hsec: TimeUnit = TimeUnitObjects.hectosecond
  def ks: TimeUnit = TimeUnitObjects.kilosecond
  def ksec: TimeUnit = TimeUnitObjects.kilosecond
  def Ms: TimeUnit = TimeUnitObjects.megasecond
  def Msec: TimeUnit = TimeUnitObjects.megasecond
  def Gs: TimeUnit = TimeUnitObjects.gigasecond
  def Gsec: TimeUnit = TimeUnitObjects.gigasecond
  def Ts: TimeUnit = TimeUnitObjects.terasecond
  def Tsec: TimeUnit = TimeUnitObjects.terasecond
  def Ps: TimeUnit = TimeUnitObjects.petasecond
  def Psec: TimeUnit = TimeUnitObjects.petasecond
  def Es: TimeUnit = TimeUnitObjects.exasecond
  def Esec: TimeUnit = TimeUnitObjects.exasecond
  def Zs: TimeUnit = TimeUnitObjects.zettasecond
  def Zsec: TimeUnit = TimeUnitObjects.zettasecond
  def Ys: TimeUnit = TimeUnitObjects.yottasecond
  def Ysec: TimeUnit = TimeUnitObjects.yottasecond
  def min: TimeUnit = TimeUnitObjects.minute
  def h: TimeUnit = TimeUnitObjects.hour
  def d: TimeUnit = TimeUnitObjects.day
  def wk: TimeUnit = TimeUnitObjects.week
  def mo: TimeUnit = TimeUnitObjects.month
  def yr: TimeUnit = TimeUnitObjects.year
  def y: TimeUnit = TimeUnitObjects.year
  def a: TimeUnit = TimeUnitObjects.year
  def dec: TimeUnit = TimeUnitObjects.decade
  def century: TimeUnit = TimeUnitObjects.century
  def S: TimeUnit = TimeUnitObjects.svedberg
  def md: TimeUnit = TimeUnitObjects.milliday
  def j: TimeUnit = TimeUnitObjects.jitty
  def ja: TimeUnit = TimeUnitObjects.jitty_alternative
  def fn: TimeUnit = TimeUnitObjects.fortnight
  def t_p: TimeUnit = TimeUnitObjects.planck_time

  def getUnits: Seq[TimeUnit] = TimeUnitObjects.getUnits
}
