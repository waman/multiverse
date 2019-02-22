package org.waman.multiverse.unitsystem

import org.waman.multiverse
import multiverse.units.basic.{Length, Mass, Time, Velocity}
import multiverse.units.mechanics.Acceleration
import multiverse.predef.BasicUnits._
import multiverse.predef.MechanicsUnits._

import spire.math.Fractional

import scala.language.implicitConversions

trait CGS extends UnitSystem{

  implicit def convertLengthToFractional[A: Fractional](q: Length[A]): A = q(cm)
  implicit def convertMassToFractional[A: Fractional](q: Mass[A]): A = q(g)
  implicit def convertTimeToFractional[A: Fractional](q: Time[A]): A = q(s)
  implicit def convertVelocityToFractional[A: Fractional](q: Velocity[A]): A = q(cm/s)
  implicit def convertAccelerationToFractional[A: Fractional](q: Acceleration[A]): A = q(cm/s2)
}

object CGSUnitSystem extends CGS
