package org.waman.multiverse.unit.mechanics

import spire.math.Real
import spire.math.Fractional

import org.waman.multiverse._

class TimeSquared[A: Fractional](val value: A, val unit: TimeSquaredUnit)
    extends LinearQuantity[TimeSquared[A], A, TimeSquaredUnit] {

  override protected def newQuantity(value: A, unit: TimeSquaredUnit): TimeSquared[A] = new TimeSquared(value, unit)
           
}

trait TimeSquaredUnit extends LinearUnit[TimeSquaredUnit]{
  override def getSIUnit: TimeSquaredUnit = TimeSquaredUnitObjects.getSIUnit

}

class DefaultTimeSquaredUnit(val name: String, val symbol: String, val aliases: Seq[String], val interval: Real)
  extends TimeSquaredUnit


object TimeSquaredUnitObjects{
  import org.waman.multiverse.unit.basic.TimeUnitObjects

  val getSIUnit: TimeSquaredUnit = TimeUnitObjects.getSIUnit * TimeUnitObjects.getSIUnit

  val second_squared: TimeSquaredUnit = TimeUnitObjects.second.square
  val yoctosecond_squared: TimeSquaredUnit = TimeUnitObjects.yoctosecond.square
  val zeptosecond_squared: TimeSquaredUnit = TimeUnitObjects.zeptosecond.square
  val attosecond_squared: TimeSquaredUnit = TimeUnitObjects.attosecond.square
  val femtosecond_squared: TimeSquaredUnit = TimeUnitObjects.femtosecond.square
  val picosecond_squared: TimeSquaredUnit = TimeUnitObjects.picosecond.square
  val nanosecond_squared: TimeSquaredUnit = TimeUnitObjects.nanosecond.square
  val microsecond_squared: TimeSquaredUnit = TimeUnitObjects.microsecond.square
  val millisecond_squared: TimeSquaredUnit = TimeUnitObjects.millisecond.square
  val centisecond_squared: TimeSquaredUnit = TimeUnitObjects.centisecond.square
  val decisecond_squared: TimeSquaredUnit = TimeUnitObjects.decisecond.square

  def getUnits: Seq[TimeSquaredUnit] =
    Seq(second_squared, yoctosecond_squared, zeptosecond_squared, attosecond_squared, femtosecond_squared, picosecond_squared, nanosecond_squared, microsecond_squared, millisecond_squared, centisecond_squared, decisecond_squared)
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
  def μs2: TimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def `μsec²`: TimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
  def μsec2: TimeSquaredUnit = TimeSquaredUnitObjects.microsecond_squared
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

  def getSIUnit: TimeSquaredUnit = TimeSquaredUnitObjects.getSIUnit
  def getUnits: Seq[TimeSquaredUnit] = TimeSquaredUnitObjects.getUnits
}
