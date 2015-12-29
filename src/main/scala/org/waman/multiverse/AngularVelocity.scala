package org.waman.multiverse

import spire.math.Fractional

abstract class AngularVelocity[A: Fractional]{

  def `rad/s` : A
}

sealed abstract class AngularVelocityUnit(code: String)

object AngularVelocityUnit{
  case object `rad/s` extends AngularVelocityUnit("radian per second")
}

trait AngularVelocityUnitInterpreter[A]{

  def `rad/s` : AngularVelocity[A]

  def apply(unit: AngularVelocityUnit): AngularVelocity[A] = unit match{
    case _ if unit == AngularVelocityUnit.`rad/s`  => `rad/s`
  }
}
