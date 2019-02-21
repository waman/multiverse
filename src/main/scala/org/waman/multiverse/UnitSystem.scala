package org.waman.multiverse

import org.waman.multiverse.predef.BasicUnits._
import org.waman.multiverse.predef.MechanicsUnits._
import org.waman.multiverse.units.basic.{Length, Mass, Time, Velocity}
import org.waman.multiverse.units.mechanics.Acceleration
import spire.math.Fractional

import scala.language.implicitConversions

trait UnitSystem

//***** MKS Unit System *****
trait MKSUnitSystem extends UnitSystem{

  implicit def convertLengthToFractional[A: Fractional](q: Length[A]): A = q(m)
  implicit def convertMassToFractional[A: Fractional](q: Mass[A]): A = q(kg)
  implicit def convertTimeToFractional[A: Fractional](q: Time[A]): A = q(s)
  implicit def convertVelocityToFractional[A: Fractional](q: Velocity[A]): A = q(m/s)
  implicit def convertAccelerationToFractional[A: Fractional](q: Acceleration[A]): A = q(m/s2)
}

object MKSUnitSystem extends MKSUnitSystem

//***** MKSA Unit System *****
trait MKSAUnitSystem extends MKSUnitSystem

object MKSAUnitSystem extends MKSAUnitSystem

//***** CGS Unit System *****
trait CGSUnitSystem extends UnitSystem{

  implicit def convertLengthToFractional[A: Fractional](q: Length[A]): A = q(cm)
  implicit def convertMassToFractional[A: Fractional](q: Mass[A]): A = q(g)
  implicit def convertTimeToFractional[A: Fractional](q: Time[A]): A = q(s)
  implicit def convertVelocityToFractional[A: Fractional](q: Velocity[A]): A = q(cm/s)
  implicit def convertAccelerationToFractional[A: Fractional](q: Acceleration[A]): A = q(cm/s2)
}

object CGSUnitSystem extends CGSUnitSystem
