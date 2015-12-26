package org.waman.multiverse

trait Time{
  def ms: Double = s * 1000.0
  def s : Double
  def min : Double = s / 60.0
  def h   : Double = s / 3600.0
}

sealed abstract class TimeUnit(code: String)

object TimeUnit{
  case object ms extends TimeUnit("millisecond")
  case object s  extends TimeUnit("second")
  case object min  extends TimeUnit("minute")
  case object h    extends TimeUnit("hour")
}

trait TimeUnitInterpreter {

  val value: Double

  def ms: Time = MilliSecondTime(value)
  def s: Time = SecondTime(value)
  def minute: Time = MinuteTime(value)
  def h: Time = HourTime(value)

  def apply(unit: TimeUnit): Time = unit match {
    case _ if unit == TimeUnit.ms => ms
    case _ if unit == TimeUnit.s => s
    case _ if unit == TimeUnit.min => minute
    case _ if unit == TimeUnit.h => h
  }

  case class MilliSecondTime(value: Double) extends Time {
    override def ms: Double = value
    override def s: Double = value / 1000.0
  }

  case class SecondTime(value: Double) extends Time {
    override def s: Double = value
  }

  case class MinuteTime(value: Double) extends Time {
    override def s: Double = value * 60.0
    override def min: Double = value
  }

  case class HourTime(value: Double) extends Time {
    override def s: Double = value * 3600.0
    override def h: Double = value
  }
}