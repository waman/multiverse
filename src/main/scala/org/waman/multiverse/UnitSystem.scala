package org.waman.multiverse

import scala.language.implicitConversions

class Per

class MetrePer(metre: Double){
  def s: Velocity = new UnitInterpreter(metre).`m/s`
}

trait UnitSystem{

  implicit def convertDoubleToUnitInterpreter(value: Double): UnitInterpreter =
    new UnitInterpreter(value)

  val / = new Per
}

trait Length{
  def mm: Double = m * 1000.0
  def cm: Double = m * 100.0
  def m : Double
  def km: Double = m / 1000.0

  def /(timeUnit: TimeUnit): Velocity = {
    val timeInSecond: Double = timeUnit match{
      case _ if timeUnit == UnitSystem.ms => 0.001
      case _ if timeUnit == UnitSystem.s  => 1
      case _ if timeUnit == UnitSystem.min  => 60
      case _ if timeUnit == UnitSystem.h    => 60 * 60
    }
    new UnitInterpreter(m / timeInSecond).`m/s`
  }
}

trait Time{
  def ms: Double = s * 1000.0
  def s : Double
  def min : Double = s / 60.0
  def h   : Double = s / 3600.0
}

trait Velocity{
  def `m/s` : Double
  def `km/h`: Double = `m/s` * (3600.0 / 1000.0)
//  def c     : Double =
}

sealed abstract class LengthUnit(code: String)
sealed abstract class TimeUnit(code: String)
sealed abstract class VelocityUnit(code: String)

object UnitSystem extends UnitSystem{

  // Length Unit
  case object mm extends LengthUnit("millimetre")
  case object cm extends LengthUnit("centimetre")
  case object m  extends LengthUnit("metre")
  case object km extends LengthUnit("kilometre")

  // Time Unit
  case object ms extends TimeUnit("millisecond")
  case object s  extends TimeUnit("second")
  case object min  extends TimeUnit("minute")
  case object h    extends TimeUnit("hour")

  // Velocity Unit
  case object `m/s` extends VelocityUnit("metre per second")
  case object `km/h` extends VelocityUnit("kilometre per hour")
  //  object perSpeedOfLight extends VelocityUnit("speed of light")
}