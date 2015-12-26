package org.waman.multiverse


trait Velocity{
  def `m/s` : Double
  def `km/h`: Double = `m/s` * (3600.0 / 1000.0)
  //  def c     : Double =
}

sealed abstract class VelocityUnit(code: String)

object VelocityUnit{
  case object `m/s` extends VelocityUnit("metre per second")
  case object `km/h` extends VelocityUnit("kilometre per hour")
}

trait VelocityUnitInterpreter{

  val value: Double

  def `m/s` : Velocity = MetrePerSecondVelocity(value)
  def `km/h`: Velocity = KilometrePerHour(value)
  //  def c     : Velocity = SpeedOfLight(value)

  def apply(unit: VelocityUnit): Velocity = unit match{
    case _ if unit == VelocityUnit.`m/s` => `m/s`
    case _ if unit == VelocityUnit.`km/h` => `km/h`
  }

  case class MetrePerSecondVelocity(value: Double) extends Velocity{
    override def `m/s`: Double = value
  }

  case class KilometrePerHour(value: Double) extends Velocity{
    override def `m/s` : Double = value * (1000.0 / 3600.0)
    override def `km/h`: Double = value
  }
}
