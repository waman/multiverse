package multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional

import multiverse._


import multiverse.unit.basic.Length
import multiverse.unit.basic.LengthUnit


import multiverse.unit.electromagnetism.TimeSquaredPerLength
import multiverse.unit.electromagnetism.TimeSquaredPerLengthUnit


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

  import multiverse.unit.basic.TimeUnit
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

  import multiverse.unit.basic.TimeUnitObjects._

  final case object second_squared extends TimePoweredTimeSquaredUnit(second, Seq("s2", "sec²", "sec2"))
  final case object yoctosecond_squared extends TimePoweredTimeSquaredUnit(yoctosecond, Seq("ys2", "ysec²", "ysec2"))
  final case object zeptosecond_squared extends TimePoweredTimeSquaredUnit(zeptosecond, Seq("zs2", "zsec²", "zsec2"))
  final case object attosecond_squared extends TimePoweredTimeSquaredUnit(attosecond, Seq("as2", "asec²", "asec2"))
  final case object femtosecond_squared extends TimePoweredTimeSquaredUnit(femtosecond, Seq("fs2", "fsec²", "fsec2"))
  final case object picosecond_squared extends TimePoweredTimeSquaredUnit(picosecond, Seq("ps2", "psec²", "psec2"))
  final case object nanosecond_squared extends TimePoweredTimeSquaredUnit(nanosecond, Seq("ns2", "nsec²", "nsec2"))
  final case object microsecond_squared extends TimePoweredTimeSquaredUnit(microsecond, Seq("μs2", "μsec²", "μsec2", "mcs²", "mcs2", "mcsec²", "mcsec2"))
  final case object millisecond_squared extends TimePoweredTimeSquaredUnit(millisecond, Seq("ms2", "msec²", "msec2"))
  final case object centisecond_squared extends TimePoweredTimeSquaredUnit(centisecond, Seq("cs2", "csec²", "csec2"))
  final case object decisecond_squared extends TimePoweredTimeSquaredUnit(decisecond, Seq("ds2", "dsec²", "dsec2"))
}

object TimeSquaredUnits{

  def `s²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def s2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def `sec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def sec2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.second_squared
  def `ys²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.yoctosecond_squared
  def ys2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.yoctosecond_squared
  def `ysec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.yoctosecond_squared
  def ysec2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.yoctosecond_squared
  def `zs²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.zeptosecond_squared
  def zs2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.zeptosecond_squared
  def `zsec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.zeptosecond_squared
  def zsec2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.zeptosecond_squared
  def `as²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.attosecond_squared
  def as2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.attosecond_squared
  def `asec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.attosecond_squared
  def asec2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.attosecond_squared
  def `fs²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.femtosecond_squared
  def fs2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.femtosecond_squared
  def `fsec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.femtosecond_squared
  def fsec2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.femtosecond_squared
  def `ps²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.picosecond_squared
  def ps2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.picosecond_squared
  def `psec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.picosecond_squared
  def psec2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.picosecond_squared
  def `ns²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.nanosecond_squared
  def ns2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.nanosecond_squared
  def `nsec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.nanosecond_squared
  def nsec2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.nanosecond_squared
  def `μs²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `μs2`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `μsec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `μsec2`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `mcs²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def mcs2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `mcsec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def mcsec2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `ms²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.millisecond_squared
  def ms2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.millisecond_squared
  def `msec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.millisecond_squared
  def msec2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.millisecond_squared
  def `cs²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.centisecond_squared
  def cs2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.centisecond_squared
  def `csec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.centisecond_squared
  def csec2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.centisecond_squared
  def `ds²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.decisecond_squared
  def ds2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.decisecond_squared
  def `dsec²`: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.decisecond_squared
  def dsec2: TimePoweredTimeSquaredUnit = TimeSquaredUnitObjects.decisecond_squared
}