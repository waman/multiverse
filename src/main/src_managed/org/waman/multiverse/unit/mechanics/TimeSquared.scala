package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional
import org.waman.multiverse._

class TimeSquared[A: Fractional](val value: A, val unit: TimeSquaredUnit)
    extends LinearQuantity[TimeSquared[A], A, TimeSquaredUnit] {

  override protected def newQuantity(value: A, unit: TimeSquaredUnit): TimeSquared[A] = new TimeSquared(value, unit)
}

trait TimeSquaredUnit extends LinearUnit[TimeSquaredUnit]{

  override def getSIUnit: TimeSquaredUnit = TimeSquaredUnit.getSIUnit
  override def dimension: Map[DimensionSymbol, Int] = TimeSquaredUnit.dimension

}

object TimeSquaredUnit{
  import DimensionSymbol._
  val dimension: Map[DimensionSymbol, Int] =
    Map[DimensionSymbol, Int](T -> 2).withDefaultValue(0)

  import org.waman.multiverse.unit.basic.TimeUnit
  val getSIUnit: TimeSquaredUnit = TimeUnit.getSIUnit * TimeUnit.getSIUnit

  import TimeSquaredUnitObjects._
  def getUnits: Seq[TimeSquaredUnit] =
    Seq(square_second, square_yoctosecond, square_zeptosecond, square_attosecond, square_femtosecond, square_picosecond, square_nanosecond, square_microsecond, square_millisecond, square_centisecond, square_decisecond)
}



class DefaultTimeSquaredUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends TimeSquaredUnit

object TimeSquaredUnitObjects{
  import org.waman.multiverse.unit.basic.TimeUnitObjects
  import org.waman.multiverse.unit.basic.TimeUnitObjects

  val square_second: TimeSquaredUnit = TimeUnitObjects.second.squared
  val square_yoctosecond: TimeSquaredUnit = TimeUnitObjects.yoctosecond.squared
  val square_zeptosecond: TimeSquaredUnit = TimeUnitObjects.zeptosecond.squared
  val square_attosecond: TimeSquaredUnit = TimeUnitObjects.attosecond.squared
  val square_femtosecond: TimeSquaredUnit = TimeUnitObjects.femtosecond.squared
  val square_picosecond: TimeSquaredUnit = TimeUnitObjects.picosecond.squared
  val square_nanosecond: TimeSquaredUnit = TimeUnitObjects.nanosecond.squared
  val square_microsecond: TimeSquaredUnit = TimeUnitObjects.microsecond.squared
  val square_millisecond: TimeSquaredUnit = TimeUnitObjects.millisecond.squared
  val square_centisecond: TimeSquaredUnit = TimeUnitObjects.centisecond.squared
  val square_decisecond: TimeSquaredUnit = TimeUnitObjects.decisecond.squared
}

object TimeSquaredUnits{
  def `s²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_second
  def s2: TimeSquaredUnit = TimeSquaredUnitObjects.square_second
  def `sec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_second
  def sec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_second
  def `ys²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_yoctosecond
  def ys2: TimeSquaredUnit = TimeSquaredUnitObjects.square_yoctosecond

  def `ysec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_yoctosecond
  def ysec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_yoctosecond
  def `zs²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_zeptosecond
  def zs2: TimeSquaredUnit = TimeSquaredUnitObjects.square_zeptosecond
  def `zsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_zeptosecond
  def zsec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_zeptosecond
  def `as²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_attosecond
  def as2: TimeSquaredUnit = TimeSquaredUnitObjects.square_attosecond
  def `asec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_attosecond
  def asec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_attosecond
  def `fs²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_femtosecond
  def fs2: TimeSquaredUnit = TimeSquaredUnitObjects.square_femtosecond
  def `fsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_femtosecond
  def fsec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_femtosecond
  def `ps²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_picosecond
  def ps2: TimeSquaredUnit = TimeSquaredUnitObjects.square_picosecond
  def `psec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_picosecond
  def psec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_picosecond
  def `ns²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_nanosecond
  def ns2: TimeSquaredUnit = TimeSquaredUnitObjects.square_nanosecond
  def `nsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_nanosecond
  def nsec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_nanosecond
  def `μs²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_microsecond
  def μs2: TimeSquaredUnit = TimeSquaredUnitObjects.square_microsecond
  def `μsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_microsecond
  def μsec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_microsecond
  def `mcs²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_microsecond
  def mcs2: TimeSquaredUnit = TimeSquaredUnitObjects.square_microsecond
  def `mcsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_microsecond
  def mcsec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_microsecond
  def `ms²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_millisecond
  def ms2: TimeSquaredUnit = TimeSquaredUnitObjects.square_millisecond
  def `msec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_millisecond
  def msec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_millisecond
  def `cs²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_centisecond
  def cs2: TimeSquaredUnit = TimeSquaredUnitObjects.square_centisecond
  def `csec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_centisecond
  def csec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_centisecond
  def `ds²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_decisecond
  def ds2: TimeSquaredUnit = TimeSquaredUnitObjects.square_decisecond
  def `dsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.square_decisecond
  def dsec2: TimeSquaredUnit = TimeSquaredUnitObjects.square_decisecond
}