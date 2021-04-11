package org.waman.multiverse.unit.defs

import spire.math._
import spire.implicits._

import org.waman.multiverse._
import org.waman.multiverse.unit.defs.em._
import org.waman.multiverse.unit.defs.fluid._
import org.waman.multiverse.unit.defs.mechanics._

class Time[A: Fractional](val value: A, val unit: TimeUnit)
    extends LinearQuantity[Time[A], A, TimeUnit] {

  override protected def newQuantity(value: A, unit: TimeUnit): Time[A] = new Time(value, unit)

  def *(electricCurrent: ElectricCurrent[A]): ElectricCharge[A] = new ElectricCharge(this.value * electricCurrent.value, electricCurrent.unit * this.unit)

  def *(voltage: Voltage[A]): MagneticFlux[A] = new MagneticFlux(this.value * voltage.value, voltage.unit * this.unit)

  def /(length: Length[A]): TimePerLength[A] = new TimePerLength(this.value / length.value, this.unit / length.unit)

  def *(pressure: Pressure[A]): DynamicViscosity[A] = new DynamicViscosity(this.value * pressure.value, pressure.unit * this.unit)

  def *(energy: Energy[A]): AngularMomentum[A] = new AngularMomentum(this.value * energy.value, energy.unit * this.unit)

  def *(power: Power[A]): Energy[A] = new Energy(this.value * power.value, power.unit * this.unit)

  def *(force: Force[A]): Momentum[A] = new Momentum(this.value * force.value, force.unit * this.unit)

  def *(time: Time[A]): TimeSquared[A] = new TimeSquared(this.value * time.value, this.unit * time.unit)
  def squared: TimeSquared[A] = this * this
}

trait TimeUnit extends LinearUnit[TimeUnit] with TimeUnitCanSquare{

  override def getSIUnit: TimeUnit = TimeUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = TimeUnit.dimension

  def /(lengthUnit: LengthUnit): TimePerLengthUnit =
    new QuotientUnit[TimePerLengthUnit, TimeUnit, LengthUnit](TimeUnit.this, lengthUnit) with TimePerLengthUnit
}

object TimeUnit extends UnitInfo[TimeUnit]{
  import DimensionSymbol._

  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 1).withDefaultValue(0)

  def getSIUnit: TimeUnit = TimeUnitObjects.second
  import TimeUnitObjects._

  def getUnits: Seq[TimeUnit] =
    Seq(second, yoctosecond, zeptosecond, attosecond, femtosecond, picosecond, nanosecond, microsecond, millisecond, centisecond, decisecond, decasecond, hectosecond, kilosecond, megasecond, gigasecond, terasecond, petasecond, exasecond, zettasecond, yottasecond, minute, hour, day, `day(sidereal)`, week, month, `month(gregorian)`, `month(full)`, `month(hollow)`, `month(synodic)`, year, `year(common)`, `year(leap)`, `year(gregorian)`, `year(sidereal)`, `year(julian)`, `year(tropical)`, decade, `decade(gregorian)`, `decade(sidereal)`, `decade(julian)`, `decade(tropical)`, century, `century(gregorian)`, `century(sidereal)`, `century(julian)`, `century(topical)`, svedberg, milliday, jitty, jitty_alternative, fortnight, atomic_unit_of_time, planck_time)
}


/** For no aliase or user defined units */
class SimpleTimeUnit(val name: String, val symbol: String, val interval: Real) extends TimeUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultTimeUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends TimeUnit
  
object TimeUnitObjects{

  final case object second extends DefaultTimeUnit("second", "s", Seq("sec"), 1)
  final case object yoctosecond extends DefaultTimeUnit("yoctosecond", "ys", Seq("ysec"), r"1e-24")
  final case object zeptosecond extends DefaultTimeUnit("zeptosecond", "zs", Seq("zsec"), r"1e-21")
  final case object attosecond extends DefaultTimeUnit("attosecond", "as", Seq("asec"), r"1e-18")
  final case object femtosecond extends DefaultTimeUnit("femtosecond", "fs", Seq("fsec"), r"1e-15")
  final case object picosecond extends DefaultTimeUnit("picosecond", "ps", Seq("psec"), r"1e-12")
  final case object nanosecond extends DefaultTimeUnit("nanosecond", "ns", Seq("nsec"), r"1e-9")
  final case object microsecond extends DefaultTimeUnit("microsecond", "μs", Seq("mcs", "μsec", "mcsec"), r"1e-6")
  final case object millisecond extends DefaultTimeUnit("millisecond", "ms", Seq("msec"), r"1e-3")
  final case object centisecond extends DefaultTimeUnit("centisecond", "cs", Seq("csec"), r"1e-2")
  final case object decisecond extends DefaultTimeUnit("decisecond", "ds", Seq("dsec"), r"1e-1")
  final case object decasecond extends DefaultTimeUnit("decasecond", "das", Seq("dasec"), r"1e1")
  final case object hectosecond extends DefaultTimeUnit("hectosecond", "hs", Seq("hsec"), r"1e2")
  final case object kilosecond extends DefaultTimeUnit("kilosecond", "ks", Seq("Ks", "ksec", "Ksec"), r"1e3")
  final case object megasecond extends DefaultTimeUnit("megasecond", "Ms", Seq("Msec"), r"1e6")
  final case object gigasecond extends DefaultTimeUnit("gigasecond", "Gs", Seq("Gsec"), r"1e9")
  final case object terasecond extends DefaultTimeUnit("terasecond", "Ts", Seq("Tsec"), r"1e12")
  final case object petasecond extends DefaultTimeUnit("petasecond", "Ps", Seq("Psec"), r"1e15")
  final case object exasecond extends DefaultTimeUnit("exasecond", "Es", Seq("Esec"), r"1e18")
  final case object zettasecond extends DefaultTimeUnit("zettasecond", "Zs", Seq("Zsec"), r"1e21")
  final case object yottasecond extends DefaultTimeUnit("yottasecond", "Ys", Seq("Ysec"), r"1e24")
  final case object minute extends SimpleTimeUnit("minute", "min", r"60")
  final case object hour extends SimpleTimeUnit("hour", "h", r"60" * minute.interval)
  final case object day extends SimpleTimeUnit("day", "d", r"24" * hour.interval)
  final case object `day(sidereal)` extends SimpleTimeUnit("day(sidereal)", "d(sidereal)", r"23.9344699" * hour.interval) with NotExact
  final case object week extends SimpleTimeUnit("week", "wk", r"7" * day.interval)
  final case object month extends SimpleTimeUnit("month", "mo", `month(gregorian)`.interval)
  final case object `month(gregorian)` extends SimpleTimeUnit("month(gregorian)", "mo(gregorian)", r"30.436875" * day.interval)
  final case object `month(full)` extends SimpleTimeUnit("month(full)", "mo(full)", r"30" * day.interval)
  final case object `month(hollow)` extends SimpleTimeUnit("month(hollow)", "mo(hollow)", r"29" * day.interval)
  final case object `month(synodic)` extends SimpleTimeUnit("month(synodic)", "mo(synodic)", r"29.530589" * day.interval)
  final case object year extends DefaultTimeUnit("year", "yr", Seq("y", "a"), `year(gregorian)`.interval)
  final case object `year(common)` extends DefaultTimeUnit("year(common)", "yr(common)", Seq("y(common)", "a(common)"), r"365" * day.interval)
  final case object `year(leap)` extends DefaultTimeUnit("year(leap)", "yr(leap)", Seq("y(leap)", "a(leap)"), r"366" * day.interval)
  final case object `year(gregorian)` extends DefaultTimeUnit("year(gregorian)", "yr(gregorian)", Seq("y(gregorian)", "a(gregorian)"), r"365.2425" * day.interval)
  final case object `year(sidereal)` extends DefaultTimeUnit("year(sidereal)", "yr(sidereal)", Seq("y(sidereal)", "a(sidereal)"), r"365.256363" * day.interval) with NotExact
  final case object `year(julian)` extends DefaultTimeUnit("year(julian)", "yr(julian)", Seq("y(julian)", "a(julian)"), r"365.25" * day.interval)
  final case object `year(tropical)` extends DefaultTimeUnit("year(tropical)", "yr(tropical)", Seq("y(tropical)", "a(tropical)"), r"365.256363" * day.interval) with NotExact
  final case object decade extends SimpleTimeUnit("decade", "dec", `decade(gregorian)`.interval)
  final case object `decade(gregorian)` extends SimpleTimeUnit("decade(gregorian)", "dec(gregorian)", r"10" * `year(gregorian)`.interval)
  final case object `decade(sidereal)` extends SimpleTimeUnit("decade(sidereal)", "dec(sidereal)", r"10" * `year(sidereal)`.interval)
  final case object `decade(julian)` extends SimpleTimeUnit("decade(julian)", "dec(julian)", r"10" * `year(julian)`.interval)
  final case object `decade(tropical)` extends SimpleTimeUnit("decade(tropical)", "dec(tropical)", r"10" * `year(tropical)`.interval)
  final case object century extends SimpleTimeUnit("century", "century", `century(gregorian)`.interval)
  final case object `century(gregorian)` extends SimpleTimeUnit("century(gregorian)", "century(gregorian)", r"100" * `year(gregorian)`.interval)
  final case object `century(sidereal)` extends SimpleTimeUnit("century(sidereal)", "century(sidereal)", r"100" * `year(sidereal)`.interval)
  final case object `century(julian)` extends SimpleTimeUnit("century(julian)", "century(julian)", r"100" * `year(julian)`.interval)
  final case object `century(topical)` extends SimpleTimeUnit("century(topical)", "century(topical)", r"100" * `year(tropical)`.interval)
  final case object svedberg extends SimpleTimeUnit("svedberg", "S", r"1e-13")
  final case object milliday extends SimpleTimeUnit("milliday", "md", r"0.001" * day.interval)
  final case object jitty extends SimpleTimeUnit("jitty", "j", r"1/60")
  final case object jitty_alternative extends SimpleTimeUnit("jitty_alternative", "ja", centisecond.interval)
  final case object fortnight extends SimpleTimeUnit("fortnight", "fn", r"2" * week.interval)
  final case object atomic_unit_of_time extends SimpleTimeUnit("atomic unit of time", "au", r"2.418884254e-17") with NotExact
  final case object planck_time extends SimpleTimeUnit("planck time", "t_p", r"5.3910632e-44") with NotExact
}

sealed trait dayAttribute
sealed trait yearAttribute
sealed trait decadeAttribute
sealed trait centuryAttribute
sealed trait monthAttribute

object TimeUnits{
  final object sidereal extends dayAttribute with yearAttribute with decadeAttribute with centuryAttribute
  final object gregorian extends monthAttribute with yearAttribute with decadeAttribute with centuryAttribute
  final object full extends monthAttribute
  final object hollow extends monthAttribute
  final object synodic extends monthAttribute
  final object common extends yearAttribute
  final object leap extends yearAttribute
  final object julian extends yearAttribute with decadeAttribute with centuryAttribute
  final object tropical extends yearAttribute with decadeAttribute
  final object topical extends centuryAttribute

  /** second */
  def s: TimeUnit = TimeUnitObjects.second
  /** second */
  def sec: TimeUnit = TimeUnitObjects.second
  /** yoctosecond */
  def ys: TimeUnit = TimeUnitObjects.yoctosecond
  /** yoctosecond */
  def ysec: TimeUnit = TimeUnitObjects.yoctosecond
  /** zeptosecond */
  def zs: TimeUnit = TimeUnitObjects.zeptosecond
  /** zeptosecond */
  def zsec: TimeUnit = TimeUnitObjects.zeptosecond
  /** attosecond */
  def as: TimeUnit = TimeUnitObjects.attosecond
  /** attosecond */
  def asec: TimeUnit = TimeUnitObjects.attosecond
  /** femtosecond */
  def fs: TimeUnit = TimeUnitObjects.femtosecond
  /** femtosecond */
  def fsec: TimeUnit = TimeUnitObjects.femtosecond
  /** picosecond */
  def ps: TimeUnit = TimeUnitObjects.picosecond
  /** picosecond */
  def psec: TimeUnit = TimeUnitObjects.picosecond
  /** nanosecond */
  def ns: TimeUnit = TimeUnitObjects.nanosecond
  /** nanosecond */
  def nsec: TimeUnit = TimeUnitObjects.nanosecond
  /** microsecond */
  def μs: TimeUnit = TimeUnitObjects.microsecond
  /** microsecond */
  def mcs: TimeUnit = TimeUnitObjects.microsecond
  /** microsecond */
  def μsec: TimeUnit = TimeUnitObjects.microsecond
  /** microsecond */
  def mcsec: TimeUnit = TimeUnitObjects.microsecond
  /** millisecond */
  def ms: TimeUnit = TimeUnitObjects.millisecond
  /** millisecond */
  def msec: TimeUnit = TimeUnitObjects.millisecond
  /** centisecond */
  def cs: TimeUnit = TimeUnitObjects.centisecond
  /** centisecond */
  def csec: TimeUnit = TimeUnitObjects.centisecond
  /** decisecond */
  def ds: TimeUnit = TimeUnitObjects.decisecond
  /** decisecond */
  def dsec: TimeUnit = TimeUnitObjects.decisecond
  /** decasecond */
  def das: TimeUnit = TimeUnitObjects.decasecond
  /** decasecond */
  def dasec: TimeUnit = TimeUnitObjects.decasecond
  /** hectosecond */
  def hs: TimeUnit = TimeUnitObjects.hectosecond
  /** hectosecond */
  def hsec: TimeUnit = TimeUnitObjects.hectosecond
  /** kilosecond */
  def ks: TimeUnit = TimeUnitObjects.kilosecond
  /** kilosecond */
  def Ks: TimeUnit = TimeUnitObjects.kilosecond
  /** kilosecond */
  def ksec: TimeUnit = TimeUnitObjects.kilosecond
  /** kilosecond */
  def Ksec: TimeUnit = TimeUnitObjects.kilosecond
  /** megasecond */
  def Ms: TimeUnit = TimeUnitObjects.megasecond
  /** megasecond */
  def Msec: TimeUnit = TimeUnitObjects.megasecond
  /** gigasecond */
  def Gs: TimeUnit = TimeUnitObjects.gigasecond
  /** gigasecond */
  def Gsec: TimeUnit = TimeUnitObjects.gigasecond
  /** terasecond */
  def Ts: TimeUnit = TimeUnitObjects.terasecond
  /** terasecond */
  def Tsec: TimeUnit = TimeUnitObjects.terasecond
  /** petasecond */
  def Ps: TimeUnit = TimeUnitObjects.petasecond
  /** petasecond */
  def Psec: TimeUnit = TimeUnitObjects.petasecond
  /** exasecond */
  def Es: TimeUnit = TimeUnitObjects.exasecond
  /** exasecond */
  def Esec: TimeUnit = TimeUnitObjects.exasecond
  /** zettasecond */
  def Zs: TimeUnit = TimeUnitObjects.zettasecond
  /** zettasecond */
  def Zsec: TimeUnit = TimeUnitObjects.zettasecond
  /** yottasecond */
  def Ys: TimeUnit = TimeUnitObjects.yottasecond
  /** yottasecond */
  def Ysec: TimeUnit = TimeUnitObjects.yottasecond
  /** minute */
  def min: TimeUnit = TimeUnitObjects.minute
  /** hour */
  def h: TimeUnit = TimeUnitObjects.hour
  /** day */
  def d: TimeUnit = TimeUnitObjects.day
  /** day(sidereal) */
  def d(a: dayAttribute): TimeUnit = a match {
    case TimeUnits.sidereal => TimeUnitObjects.`day(sidereal)`
  }
  /** day(sidereal) */
  def `d(sidereal)`: TimeUnit = TimeUnitObjects.`day(sidereal)`
  /** week */
  def wk: TimeUnit = TimeUnitObjects.week
  /** month */
  def mo: TimeUnit = TimeUnitObjects.month
  /** month(gregorian)<br/>month(full)<br/>month(hollow)<br/>month(synodic) */
  def mo(a: monthAttribute): TimeUnit = a match {
    case TimeUnits.gregorian => TimeUnitObjects.`month(gregorian)`
    case TimeUnits.full => TimeUnitObjects.`month(full)`
    case TimeUnits.hollow => TimeUnitObjects.`month(hollow)`
    case TimeUnits.synodic => TimeUnitObjects.`month(synodic)`
  }
  /** month(gregorian) */
  def `mo(gregorian)`: TimeUnit = TimeUnitObjects.`month(gregorian)`
  /** month(full) */
  def `mo(full)`: TimeUnit = TimeUnitObjects.`month(full)`
  /** month(hollow) */
  def `mo(hollow)`: TimeUnit = TimeUnitObjects.`month(hollow)`
  /** month(synodic) */
  def `mo(synodic)`: TimeUnit = TimeUnitObjects.`month(synodic)`
  /** year */
  def yr: TimeUnit = TimeUnitObjects.year
  /** year(common)<br/>year(leap)<br/>year(gregorian)<br/>year(sidereal)<br/>year(julian)<br/>year(tropical) */
  def yr(a: yearAttribute): TimeUnit = a match {
    case TimeUnits.common => TimeUnitObjects.`year(common)`
    case TimeUnits.leap => TimeUnitObjects.`year(leap)`
    case TimeUnits.gregorian => TimeUnitObjects.`year(gregorian)`
    case TimeUnits.sidereal => TimeUnitObjects.`year(sidereal)`
    case TimeUnits.julian => TimeUnitObjects.`year(julian)`
    case TimeUnits.tropical => TimeUnitObjects.`year(tropical)`
  }
  /** year */
  def y: TimeUnit = TimeUnitObjects.year
  /**   year(common)<br/>  year(leap)<br/>  year(gregorian)<br/>  year(sidereal)<br/>  year(julian)<br/>  year(tropical) */
  def y(a: yearAttribute): TimeUnit = yr(a)
  /** year */
  def a: TimeUnit = TimeUnitObjects.year
  /**   year(common)<br/>  year(leap)<br/>  year(gregorian)<br/>  year(sidereal)<br/>  year(julian)<br/>  year(tropical) */
  def a(a: yearAttribute): TimeUnit = yr(a)
  /** year(common) */
  def `yr(common)`: TimeUnit = TimeUnitObjects.`year(common)`
  /** year(common) */
  def `y(common)`: TimeUnit = TimeUnitObjects.`year(common)`
  /** year(common) */
  def `a(common)`: TimeUnit = TimeUnitObjects.`year(common)`
  /** year(leap) */
  def `yr(leap)`: TimeUnit = TimeUnitObjects.`year(leap)`
  /** year(leap) */
  def `y(leap)`: TimeUnit = TimeUnitObjects.`year(leap)`
  /** year(leap) */
  def `a(leap)`: TimeUnit = TimeUnitObjects.`year(leap)`
  /** year(gregorian) */
  def `yr(gregorian)`: TimeUnit = TimeUnitObjects.`year(gregorian)`
  /** year(gregorian) */
  def `y(gregorian)`: TimeUnit = TimeUnitObjects.`year(gregorian)`
  /** year(gregorian) */
  def `a(gregorian)`: TimeUnit = TimeUnitObjects.`year(gregorian)`
  /** year(sidereal) */
  def `yr(sidereal)`: TimeUnit = TimeUnitObjects.`year(sidereal)`
  /** year(sidereal) */
  def `y(sidereal)`: TimeUnit = TimeUnitObjects.`year(sidereal)`
  /** year(sidereal) */
  def `a(sidereal)`: TimeUnit = TimeUnitObjects.`year(sidereal)`
  /** year(julian) */
  def `yr(julian)`: TimeUnit = TimeUnitObjects.`year(julian)`
  /** year(julian) */
  def `y(julian)`: TimeUnit = TimeUnitObjects.`year(julian)`
  /** year(julian) */
  def `a(julian)`: TimeUnit = TimeUnitObjects.`year(julian)`
  /** year(tropical) */
  def `yr(tropical)`: TimeUnit = TimeUnitObjects.`year(tropical)`
  /** year(tropical) */
  def `y(tropical)`: TimeUnit = TimeUnitObjects.`year(tropical)`
  /** year(tropical) */
  def `a(tropical)`: TimeUnit = TimeUnitObjects.`year(tropical)`
  /** decade */
  def dec: TimeUnit = TimeUnitObjects.decade
  /** decade(gregorian)<br/>decade(sidereal)<br/>decade(julian)<br/>decade(tropical) */
  def dec(a: decadeAttribute): TimeUnit = a match {
    case TimeUnits.gregorian => TimeUnitObjects.`decade(gregorian)`
    case TimeUnits.sidereal => TimeUnitObjects.`decade(sidereal)`
    case TimeUnits.julian => TimeUnitObjects.`decade(julian)`
    case TimeUnits.tropical => TimeUnitObjects.`decade(tropical)`
  }
  /** decade(gregorian) */
  def `dec(gregorian)`: TimeUnit = TimeUnitObjects.`decade(gregorian)`
  /** decade(sidereal) */
  def `dec(sidereal)`: TimeUnit = TimeUnitObjects.`decade(sidereal)`
  /** decade(julian) */
  def `dec(julian)`: TimeUnit = TimeUnitObjects.`decade(julian)`
  /** decade(tropical) */
  def `dec(tropical)`: TimeUnit = TimeUnitObjects.`decade(tropical)`
  /** century */
  def century: TimeUnit = TimeUnitObjects.century
  /** century(gregorian)<br/>century(sidereal)<br/>century(julian)<br/>century(topical) */
  def century(a: centuryAttribute): TimeUnit = a match {
    case TimeUnits.gregorian => TimeUnitObjects.`century(gregorian)`
    case TimeUnits.sidereal => TimeUnitObjects.`century(sidereal)`
    case TimeUnits.julian => TimeUnitObjects.`century(julian)`
    case TimeUnits.topical => TimeUnitObjects.`century(topical)`
  }
  /** century(gregorian) */
  def `century(gregorian)`: TimeUnit = TimeUnitObjects.`century(gregorian)`
  /** century(sidereal) */
  def `century(sidereal)`: TimeUnit = TimeUnitObjects.`century(sidereal)`
  /** century(julian) */
  def `century(julian)`: TimeUnit = TimeUnitObjects.`century(julian)`
  /** century(topical) */
  def `century(topical)`: TimeUnit = TimeUnitObjects.`century(topical)`
  /** svedberg */
  def S: TimeUnit = TimeUnitObjects.svedberg
  /** milliday */
  def md: TimeUnit = TimeUnitObjects.milliday
  /** jitty */
  def j: TimeUnit = TimeUnitObjects.jitty
  /** jitty_alternative */
  def ja: TimeUnit = TimeUnitObjects.jitty_alternative
  /** fortnight */
  def fn: TimeUnit = TimeUnitObjects.fortnight
  /** atomic unit of time */
  def au: TimeUnit = TimeUnitObjects.atomic_unit_of_time
  /** planck time */
  def t_p: TimeUnit = TimeUnitObjects.planck_time
}