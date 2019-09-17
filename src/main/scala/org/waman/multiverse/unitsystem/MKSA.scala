package org.waman.multiverse.unitsystem

import org.waman.multiverse.unit.basic.LengthUnits.m
import org.waman.multiverse.unit.basic.MassUnits.kg
import org.waman.multiverse.unit.basic.TimeUnits.s
import org.waman.multiverse.unit.mechanics.TimeSquaredUnits.s2
import org.waman.multiverse.unit.basic._
import org.waman.multiverse.unit.mechanics.Acceleration
import spire.math.Fractional

import scala.language.implicitConversions

//***** MKS Unit System *****
trait MKS extends UnitSystem{

  implicit def convertLengthToFractional[A: Fractional](q: Length[A]): A = q(m)
  implicit def convertMassToFractional[A: Fractional](q: Mass[A]): A = q(kg)
  implicit def convertTimeToFractional[A: Fractional](q: Time[A]): A = q(s)
  implicit def convertVelocityToFractional[A: Fractional](q: Velocity[A]): A = q(m/s)
  implicit def convertAccelerationToFractional[A: Fractional](q: Acceleration[A]): A = q(m/s2)
}

object MKS extends MKS

//***** MKSA Unit System *****
trait MKSA extends MKS
object MKSA extends MKSA

//***** SI Unit System (= MKSA Unit System) *****
trait SI extends MKSA
object SI extends SI