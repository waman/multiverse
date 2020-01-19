package org.waman.multiverse.unitsystem

import org.waman.multiverse
import multiverse.unit.basic._
import multiverse.unit.mechanics.Acceleration
import org.waman.multiverse.unit.basic.LengthUnits.cm
import org.waman.multiverse.unit.basic.MassUnits.g
import org.waman.multiverse.unit.basic.TimeUnits.s
import org.waman.multiverse.unit.mechanics.TimeSquaredUnits.s2
import spire.math.Fractional

import scala.language.implicitConversions

trait CGS extends UnitSystem{

  implicit def convertLengthToFractional[A: Fractional](q: Length[A]): A = q(cm)
  implicit def convertMassToFractional[A: Fractional](q: Mass[A]): A = q(g)
  implicit def convertTimeToFractional[A: Fractional](q: Time[A]): A = q(s)
  implicit def convertVelocityToFractional[A: Fractional](q: Velocity[A]): A = q(cm/s)
  implicit def convertAccelerationToFractional[A: Fractional](q: Acceleration[A]): A = q(cm/s2)
}

object CGS extends CGS
