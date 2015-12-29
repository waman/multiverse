package org.waman.multiverse

import spire.math.Fractional
import spire.implicits._

abstract class Velocity[A: Fractional]{

  val algebra = implicitly[Fractional[A]]

  def `m/s` : A
  def `km/h`: A = `m/s` * 3600.0 / 1000.0
  //  def c     : Double =
}

sealed abstract class VelocityUnit(code: String) extends CompositeUnit[LengthUnit, TimeUnit]

object VelocityUnit{
  case object `m/s` extends VelocityUnit("metre per second")
  case object `km/h` extends VelocityUnit("kilometre per hour")
}

trait VelocityUnitInterpreter[A]{

  def `m/s` : Velocity[A]
  def `km/h`: Velocity[A]
  //  def c     : Velocity

  def apply(unit: VelocityUnit): Velocity[A] = unit match{
    case _ if unit == VelocityUnit.`m/s`  => `m/s`
    case _ if unit == VelocityUnit.`km/h` => `km/h`
  }
}
