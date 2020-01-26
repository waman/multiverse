package org.waman.multiverse.unitsystem

import scala.language.implicitConversions

import org.waman.multiverse.unit.basic.Mass
import org.waman.multiverse.unit.basic.Length
import org.waman.multiverse.unit.basic.Time
import org.waman.multiverse.unit.basic.Velocity

import org.waman.multiverse.unit.basic.MassUnitObjects.kilogram
import org.waman.multiverse.unit.basic.LengthUnitObjects.metre
import org.waman.multiverse.unit.basic.TimeUnitObjects.second

trait MKS {
  implicit def evaluateMass[A: Fractional](q: Mass[A]): A = q(kilogram)
  implicit def evaluateLength[A: Fractional](q: Length[A]): A = q(metre)
  implicit def evaluateTime[A: Fractional](q: Time[A]): A = q(second)
  implicit def evaluateVelocity[A: Fractional](q: Velocity[A]): A = q(metre / second)
}

object MKS extends MKS
