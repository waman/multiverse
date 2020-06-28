package waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional

import waman.multiverse._


import waman.multiverse.unit.basic.Length
import waman.multiverse.unit.basic.LengthUnit


import waman.multiverse.unit.electromagnetism.TimeSquaredPerLength
import waman.multiverse.unit.electromagnetism.TimeSquaredPerLengthUnit


class TimeSquared[A: Fractional](val value: A, val unit: TimeSquaredUnit)
    extends LinearQuantity[TimeSquared[A], A, TimeSquaredUnit] {

  import spire.implicits._

  override protected def newQuantity(value: A, unit: TimeSquaredUnit): TimeSquared[A] = new TimeSquared(value, unit)

  def /(length: Length[A]): TimeSquaredPerLength[A] = new TimeSquaredPerLength(this.value / length.value, this.unit / length.unit)
}

trait TimeSquaredUnit extends LinearUnit[TimeSquaredUnit]{

  override def getSIUnit: TimeSquaredUnit = TimeSquaredUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = TimeSquaredUnit.dimension

  def /(lengthUnit: LengthUnit): TimeSquaredPerLengthUnit =
    new QuotientUnit[TimeSquaredPerLengthUnit, TimeSquaredUnit, LengthUnit](TimeSquaredUnit.this, lengthUnit) with TimeSquaredPerLengthUnit
}

object TimeSquaredUnit extends UnitInfo[TimeSquaredUnit]{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 2).withDefaultValue(0)

  import waman.multiverse.unit.basic.TimeUnit
  val getSIUnit: TimeSquaredUnit = TimeUnit.getSIUnit * TimeUnit.getSIUnit

  import TimeSquaredUnitObjects._
  def getUnits: Seq[TimeSquaredUnit] =
    Seq(second_squared, yoctosecond_squared, zeptosecond_squared, attosecond_squared, femtosecond_squared, picosecond_squared, nanosecond_squared, microsecond_squared, millisecond_squared, centisecond_squared, decisecond_squared)
}

/** For no aliase or user defined units */
class SimpleTimeSquaredUnit(val name: String, val symbol: String, val interval: Real) extends TimeSquaredUnit {
  override def aliases: Seq[String] = Nil
}

/** For units which has aliases */
class DefaultTimeSquaredUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends TimeSquaredUnit

object TimeSquaredUnitObjects{

  import waman.multiverse.unit.basic.TimeUnitObjects._

  final case object second_squared extends DefaultTimeSquaredUnit("second squared", "s²", Seq("s2", "sec²", "sec2"), second.interval**2)
  final case object yoctosecond_squared extends DefaultTimeSquaredUnit("yoctosecond squared", "ys²", Seq("ys2", "ysec²", "ysec2"), yoctosecond.interval**2)
  final case object zeptosecond_squared extends DefaultTimeSquaredUnit("zeptosecond squared", "zs²", Seq("zs2", "zsec²", "zsec2"), zeptosecond.interval**2)
  final case object attosecond_squared extends DefaultTimeSquaredUnit("attosecond squared", "as²", Seq("as2", "asec²", "asec2"), attosecond.interval**2)
  final case object femtosecond_squared extends DefaultTimeSquaredUnit("femtosecond squared", "fs²", Seq("fs2", "fsec²", "fsec2"), femtosecond.interval**2)
  final case object picosecond_squared extends DefaultTimeSquaredUnit("picosecond squared", "ps²", Seq("ps2", "psec²", "psec2"), picosecond.interval**2)
  final case object nanosecond_squared extends DefaultTimeSquaredUnit("nanosecond squared", "ns²", Seq("ns2", "nsec²", "nsec2"), nanosecond.interval**2)
  final case object microsecond_squared extends DefaultTimeSquaredUnit("microsecond squared", "μs²", Seq("μs2", "μsec²", "μsec2", "mcs²", "mcs2", "mcsec²", "mcsec2"), microsecond.interval**2)
  final case object millisecond_squared extends DefaultTimeSquaredUnit("millisecond squared", "ms²", Seq("ms2", "msec²", "msec2"), millisecond.interval**2)
  final case object centisecond_squared extends DefaultTimeSquaredUnit("centisecond squared", "cs²", Seq("cs2", "csec²", "csec2"), centisecond.interval**2)
  final case object decisecond_squared extends DefaultTimeSquaredUnit("decisecond squared", "ds²", Seq("ds2", "dsec²", "dsec2"), decisecond.interval**2)
}

object TimeSquaredUnits{

  def `s²`: TimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def s2: TimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def `sec²`: TimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def sec2: TimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def `ys²`: TimeSquaredUnit = TimeSquaredUnitObjects.yoctosecond_squared
  def ys2: TimeSquaredUnit = TimeSquaredUnitObjects.yoctosecond_squared
  def `ysec²`: TimeSquaredUnit = TimeSquaredUnitObjects.yoctosecond_squared
  def ysec2: TimeSquaredUnit = TimeSquaredUnitObjects.yoctosecond_squared
  def `zs²`: TimeSquaredUnit = TimeSquaredUnitObjects.zeptosecond_squared
  def zs2: TimeSquaredUnit = TimeSquaredUnitObjects.zeptosecond_squared
  def `zsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.zeptosecond_squared
  def zsec2: TimeSquaredUnit = TimeSquaredUnitObjects.zeptosecond_squared
  def `as²`: TimeSquaredUnit = TimeSquaredUnitObjects.attosecond_squared
  def as2: TimeSquaredUnit = TimeSquaredUnitObjects.attosecond_squared
  def `asec²`: TimeSquaredUnit = TimeSquaredUnitObjects.attosecond_squared
  def asec2: TimeSquaredUnit = TimeSquaredUnitObjects.attosecond_squared
  def `fs²`: TimeSquaredUnit = TimeSquaredUnitObjects.femtosecond_squared
  def fs2: TimeSquaredUnit = TimeSquaredUnitObjects.femtosecond_squared
  def `fsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.femtosecond_squared
  def fsec2: TimeSquaredUnit = TimeSquaredUnitObjects.femtosecond_squared
  def `ps²`: TimeSquaredUnit = TimeSquaredUnitObjects.picosecond_squared
  def ps2: TimeSquaredUnit = TimeSquaredUnitObjects.picosecond_squared
  def `psec²`: TimeSquaredUnit = TimeSquaredUnitObjects.picosecond_squared
  def psec2: TimeSquaredUnit = TimeSquaredUnitObjects.picosecond_squared
  def `ns²`: TimeSquaredUnit = TimeSquaredUnitObjects.nanosecond_squared
  def ns2: TimeSquaredUnit = TimeSquaredUnitObjects.nanosecond_squared
  def `nsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.nanosecond_squared
  def nsec2: TimeSquaredUnit = TimeSquaredUnitObjects.nanosecond_squared
  def `μs²`: TimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `μs2`: TimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `μsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `μsec2`: TimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `mcs²`: TimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def mcs2: TimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `mcsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def mcsec2: TimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `ms²`: TimeSquaredUnit = TimeSquaredUnitObjects.millisecond_squared
  def ms2: TimeSquaredUnit = TimeSquaredUnitObjects.millisecond_squared
  def `msec²`: TimeSquaredUnit = TimeSquaredUnitObjects.millisecond_squared
  def msec2: TimeSquaredUnit = TimeSquaredUnitObjects.millisecond_squared
  def `cs²`: TimeSquaredUnit = TimeSquaredUnitObjects.centisecond_squared
  def cs2: TimeSquaredUnit = TimeSquaredUnitObjects.centisecond_squared
  def `csec²`: TimeSquaredUnit = TimeSquaredUnitObjects.centisecond_squared
  def csec2: TimeSquaredUnit = TimeSquaredUnitObjects.centisecond_squared
  def `ds²`: TimeSquaredUnit = TimeSquaredUnitObjects.decisecond_squared
  def ds2: TimeSquaredUnit = TimeSquaredUnitObjects.decisecond_squared
  def `dsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.decisecond_squared
  def dsec2: TimeSquaredUnit = TimeSquaredUnitObjects.decisecond_squared
}