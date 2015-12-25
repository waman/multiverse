package org.waman.multiverse

class UnitInterpreter(val value: Double)
  extends LengthUnitInterpreter
  with TimeUnitInterpreter
  with VelocityUnitInterpreter

trait LengthUnitInterpreter {

  val value: Double

  def mm: Length = MillimetreLength(value)
  def cm: Length = CentimetreLength(value)
  def m : Length = MetreLength(value)
  def km: Length = KilometreLength(value)

  def m(per: Per): MetrePer = new MetrePer(value)

  def apply(unit: LengthUnit): Length = unit match {
    case _ if unit == UnitSystem.mm => mm
    case _ if unit == UnitSystem.cm => cm
    case _ if unit == UnitSystem.m => m
    case _ if unit == UnitSystem.km => km
  }

  case class MillimetreLength(value: Double) extends Length {
    override def mm: Double = value

    override def m: Double = value / 1000.0
  }

  case class CentimetreLength(value: Double) extends Length {
    override def cm: Double = value

    override def m: Double = value / 100.0
  }

  case class MetreLength(value: Double) extends Length {
    override def m: Double = value
  }

  case class KilometreLength(value: Double) extends Length {
    override def m: Double = value * 1000

    override def km: Double = value
  }
}

trait TimeUnitInterpreter {

  val value: Double

  def ms: Time = MilliSecondTime(value)
  def s: Time = SecondTime(value)
  def minute: Time = MinuteTime(value)
  def h: Time = HourTime(value)

  def apply(unit: TimeUnit): Time = unit match {
    case _ if unit == UnitSystem.ms => ms
    case _ if unit == UnitSystem.s => s
    case _ if unit == UnitSystem.min => minute
    case _ if unit == UnitSystem.h => h
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

trait VelocityUnitInterpreter{

  val value: Double

  def `m/s` : Velocity = MetrePerSecondVelocity(value)
  def `km/h`: Velocity = KilometrePerHour(value)
//  def c     : Velocity = SpeedOfLight(value)

  def apply(unit: VelocityUnit): Velocity = unit match{
    case _ if unit == UnitSystem.`m/s` => `m/s`
    case _ if unit == UnitSystem.`km/h` => `km/h`
  }

  case class MetrePerSecondVelocity(value: Double) extends Velocity{
    override def `m/s`: Double = value
  }

  case class KilometrePerHour(value: Double) extends Velocity{
    override def `m/s` : Double = value * (1000.0 / 3600.0)
    override def `km/h`: Double = value
  }
}
